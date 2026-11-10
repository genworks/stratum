;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :stratum)

;;
;; New pixels from an image generator: (generate-fill prompt) repaints
;; the selection and (generate-layer prompt) makes a picture of the
;; document's size.  The generator is a separate service,
;; stable-diffusion.cpp's sd-server, spoken to through its native job
;; API (POST /sdcpp/v1/img_gen, then GET the job until it is done), not
;; its WebUI one, whose img2img was reported to ignore the mask;
;; (configure-generator! :url ...) says where.  There is none by default.
;;
;; A generator works at a megapixel or so and a photograph has many, so
;; generate-fill sends a crop: the selection and some of what surrounds
;; it, scaled to the generator's size, with the selection as its mask.
;; What comes back is scaled to the crop and lands as a new raster layer
;; whose alpha is the selection's coverage, faded in from its edge.  So a
;; generator that strays outside its mask changes nothing there, nothing
;; below the layer is touched, and the layer takes masks, blends and
;; undo like any other.
;;
;; A generator repaints the whole of its mask, the surroundings of a new
;; thing included, and its copy of a sky is never quite the sky: so by
;; default the layer keeps only what the generator changed (changed-matte,
;; :keep :changed), and a new thing comes without a repainted copy of
;; what was around it.  :keep :all keeps every pixel, for a fill that
;; means to replace everything it covers.
;;
;; A generation takes many seconds, so these operations run outside the
;; server's lock (run-forms, *long-operators*) and take it themselves to
;; read the document and to add the layer.
;;

(defstruct (generator (:constructor make-generator
                          (url steps cfg-scale strength size negative seconds invert-mask
                           &optional (turn #'funcall) recipe sampler scheduler)))
  "A generator's address and how to ask it.  URL is its base, with no
trailing slash; STEPS, CFG-SCALE and STRENGTH (denoising, 0-1) suit its
model; SIZE is the longer side, in pixels, of what it is sent; NEGATIVE
a negative prompt; SECONDS how long to wait; INVERT-MASK true when its
model wants the selection dark; TURN a function of one thunk that calls
it when the generator may be used, for a card shared with something
else.  It runs with no lock of ours held and must not wait on one.
RECIPE is the id of the model the steps and the rest were taken from
(nil when they were given), and STEPS, CFG-SCALE, SAMPLER and SCHEDULER
are each left out of a request when nil, so the model's own apply."
  url steps cfg-scale strength size negative seconds invert-mask turn recipe sampler scheduler)

(defvar *generator* nil
  "The image generator, or nil: there is none by default.")

(defun configure-generator! (&key url (steps 28) (cfg-scale 7) (strength 0.9d0) (size 1024)
                                  (negative "") (seconds 300) invert-mask (turn #'funcall))
  "Generate images through the service at URL, or not at all with nil.
The generator as it stands."
  (setf *generator* (and url (make-generator (string-right-trim "/" url) steps cfg-scale strength
                                             size negative seconds invert-mask turn))))

(defun fitted-size (w h size)
  "W x H scaled so the longer side is SIZE, each side a multiple of 64,
as generators want: (values width height)."
  (let ((scale (/ size (max w h))))
    (flet ((side (n) (* 64 (max 1 (round (* n scale) 64)))))
      (values (side w) (side h)))))

(defun grown-rect (rect w h)
  "RECT with a margin of what surrounds it, inside W x H: a generator
matches what it paints to what it can see."
  (destructuring-bind (x y rw rh) rect
    (let* ((margin (max 32 (ceiling (* 35 (max rw rh)) 100)))
           (x0 (max 0 (- x margin)))
           (y0 (max 0 (- y margin)))
           (x1 (min w (+ x rw margin)))
           (y1 (min h (+ y rh margin))))
      (list x0 y0 (- x1 x0) (- y1 y0)))))

(defun gray-png (gray w h invert?)
  "GRAY, W x H bytes, as a one-channel PNG, inverted with INVERT?."
  (let* ((png (make-instance 'zpng:png :color-type :grayscale :width w :height h))
         (data (zpng:data-array png)))
    (dotimes (y h)
      (dotimes (x w)
        (let ((v (aref gray (+ x (* y w)))))
          (setf (aref data y x 0) (if invert? (- 255 v) v)))))
    (flexi-streams:with-output-to-sequence (out :element-type '(unsigned-byte 8))
      (zpng:write-png-stream png out))))

(defun generator-body (generator prompt seed w h &key image mask strength reference)
  "The job for a picture of W x H, as JSON text.  With IMAGE, PNG octets,
a repainting of it, by STRENGTH or the generator's own, and with MASK
of that part alone; with REFERENCE, PNG octets or a list of them, the
pictures the model is shown and works from, in that order."
  (flet ((object (&rest keys-and-values)
           (let ((table (make-hash-table :test 'equal)))
             (loop for (key value) on keys-and-values by #'cddr
                   do (setf (gethash key table) value))
             table)))
    (let ((body (object "prompt" prompt
                        "negative_prompt" (generator-negative generator)
                        "width" w
                        "height" h
                        "seed" seed
                        "output_format" "png"))
          (params (object)))
      ;; what is not given is left to the model loaded, whose recipe says
      (when (generator-steps generator)
        (setf (gethash "sample_steps" params) (generator-steps generator)))
      (when (generator-cfg-scale generator)
        (setf (gethash "guidance" params) (object "txt_cfg" (float (generator-cfg-scale generator) 1d0))))
      (when (generator-sampler generator)
        (setf (gethash "sample_method" params) (generator-sampler generator)))
      (when (generator-scheduler generator)
        (setf (gethash "scheduler" params) (generator-scheduler generator)))
      (when (plusp (hash-table-count params))
        (setf (gethash "sample_params" body) params))
      (when image
        (setf (gethash "init_image" body) (data-url image)
              (gethash "strength" body) (float (or strength (generator-strength generator)) 1d0)))
      (when mask
        (setf (gethash "mask_image" body) (data-url mask)))
      (when reference
        (setf (gethash "ref_images" body)
              (mapcar #'data-url (if (listp reference) reference (list reference)))))
      (yason:with-output-to-string* () (yason:encode body)))))

(defparameter *generator-poll-seconds* 1
  "How often a job is asked after.")

(defun generator-get (generator path)
  "PATH of the generator, parsed: (values reply status)."
  (multiple-value-bind (text status)
      (glisp:http-request (format nil "~a~a" (generator-url generator) path)
                          :connect-timeout 10 :read-timeout 30)
    (values (ignore-errors (yason:parse text)) status)))

(defun job-picture (job)
  "The picture of a finished JOB: (values pixels w h)."
  (let* ((result (gethash "result" job))
         (images (and (hash-table-p result) (gethash "images" result)))
         (image (and images (gethash "b64_json" (first images)))))
    (unless (stringp image)
      (error "The image generator finished with no image."))
    (read-png (cl-base64:base64-string-to-usb8-array image))))

(defun generator-job (generator body)
  "BODY submitted to the generator as a job, and the job waited for:
the picture, (values pixels w h).  Errors say what a person can act on."
  (multiple-value-bind (status text)
      (handler-case (sexp-agent:post-json (format nil "~a/sdcpp/v1/img_gen" (generator-url generator)) body
                                          :seconds 60)
        (error (e)
          (error "The image generator could not be reached: ~a" e)))
    (unless (member status '(200 202))
      (error "The image generator answered ~a: ~a" status (subseq text 0 (min 200 (length text)))))
    (let* ((reply (handler-case (yason:parse text)
                    (error () (error "The image generator's answer was not JSON."))))
           (poll (and (hash-table-p reply) (gethash "poll_url" reply)))
           (deadline (+ (get-universal-time) (generator-seconds generator))))
      (unless (stringp poll)
        (error "The image generator gave no job to follow."))
      (loop
        (let* ((job (generator-get generator poll))
               (state (and (hash-table-p job) (gethash "status" job))))
          (cond ((equal state "completed") (return (job-picture job)))
                ((member state '("failed" "cancelled") :test #'equal)
                 (let ((why (gethash "error" job)))
                   (error "The image generator ~a: ~a" state
                          (or (and (hash-table-p why) (gethash "message" why)) "no reason given"))))
                ((> (get-universal-time) deadline)
                 (error "The image generator took more than ~a seconds." (generator-seconds generator)))
                (t (sleep *generator-poll-seconds*))))))))

(defun ask-generator (generator body)
  "The job BODY run in the generator's turn: the picture, (values pixels
w h), whatever the turn itself answers."
  (let ((picture nil))
    (funcall (generator-turn generator)
             (lambda () (setf picture (multiple-value-list (generator-job generator body)))))
    (unless picture
      (error "The image generator's turn never came."))
    (values-list picture)))

(defun require-generator ()
  "The generator, its settings brought to the model it has loaded."
  (let ((generator (or *generator* (error "This server has no image generator."))))
    (ignore-errors (generator-in-step! generator))
    generator))

(defun generator-references (generator)
  "Whether GENERATOR's loaded model can be shown a picture: :yes, :no, or
nil when it cannot be told.  Asked for each generation, since the model
can be changed behind us.  A server with nothing loaded answers with
every feature false, which says nothing about any model, so a model's
name comes first."
  (let* ((reply (ignore-errors (generator-get generator "/sdcpp/v1/capabilities")))
         (features (and (hash-table-p reply) (gethash "generation_features" reply)))
         (model (and (hash-table-p reply) (gethash "model" reply)))
         (name (and (hash-table-p model) (gethash "name" model))))
    (when (and (hash-table-p features) (stringp name) (plusp (length name)))
      (if (gethash "ref_images" features) :yes :no))))

(defparameter *unsaid-reference-note*
  "The image generator did not say whether its model can be shown a picture, so it may have worked from the words alone."
  "The :note of a generation whose generator could not be asked what its
model takes.")

;;; Which model the generator has loaded.  sd-server holds one model for
;;; everything it serves and changes it by recipe (POST
;;; /sdcpp/v1/models/load {"recipe": id}); each recipe carries the steps,
;;; guidance, sampler and scheduler its model wants, and a request that
;;; leaves them out gets those.  So Stratum takes them from the recipe
;;; when it switches, and before each generation it asks what is loaded:
;;; a restart of the server, or a switch made by hand, would otherwise
;;; leave it asking one model for another's settings.

(defparameter *generator-recipe-sources* '("user")
  "Where a recipe must come from to be offered: the server's own
builtins name files nobody has put there, so by default only the
recipes its owner wrote.  Nil offers every one.")

(defvar *generator-choosers* nil
  "The invite names that may change the generator's model from the page,
or nil for anyone admitted.  The model is the whole server's: a change is
everyone's.  Lisp callers always may.")

(defun json-table (&rest keys-and-values)
  (let ((table (make-hash-table :test 'equal)))
    (loop for (key value) on keys-and-values by #'cddr
          do (setf (gethash key table) value))
    table))

(defun generator-post (generator path value &key (seconds 600))
  "VALUE, a JSON object, posted to PATH of the generator, waiting up to
SECONDS: (values reply status)."
  (multiple-value-bind (status text)
      (handler-case (sexp-agent:post-json (format nil "~a~a" (generator-url generator) path)
                                          (yason:with-output-to-string* () (yason:encode value))
                                          :seconds seconds)
        (error (e) (error "The image generator could not be reached: ~a" e)))
    (values (ignore-errors (yason:parse text)) status)))

(defun generator-recipes (generator)
  "The models GENERATOR offers: ((id name defaults) ...), DEFAULTS the
recipe's hash table of them, or nil when the listing has none."
  (multiple-value-bind (reply status) (generator-get generator "/sdcpp/v1/recipes")
    (unless (and (eql status 200) (hash-table-p reply))
      (error "The image generator did not list its models (it answered ~a)." status))
    (loop for recipe in (gethash "recipes" reply)
          when (and (hash-table-p recipe) (stringp (gethash "id" recipe))
                    (or (null *generator-recipe-sources*)
                        (member (gethash "source" recipe) *generator-recipe-sources* :test #'equal)))
            collect (list (gethash "id" recipe)
                          (let ((name (gethash "name" recipe)))
                            (if (and (stringp name) (plusp (length name))) name (gethash "id" recipe)))
                          (let ((defaults (gethash "defaults" recipe)))
                            (and (hash-table-p defaults) defaults))))))

(defun generator-loaded (generator)
  "The id of the recipe GENERATOR has loaded and ready, or nil: nothing
loaded, a model named on its command line, or no answer."
  (let ((status (ignore-errors (generator-get generator "/sdcpp/v1/models/status"))))
    (and (hash-table-p status)
         (equal (gethash "state" status) "ready")
         (let ((recipe (gethash "recipe" status)))
           (and (stringp recipe) (plusp (length recipe)) recipe)))))

(defun adopt-recipe! (generator id defaults)
  "GENERATOR's settings those of recipe ID: each of DEFAULTS (a hash
table or nil) the recipe gives, and the others left to the server."
  (flet ((default (key) (and (hash-table-p defaults) (gethash key defaults))))
    (setf (generator-recipe generator) id
          (generator-steps generator) (default "steps")
          (generator-cfg-scale generator) (default "cfg_scale")
          (generator-sampler generator) (default "sampling_method")
          (generator-scheduler generator) (default "scheduler"))
    generator))

(defun generator-in-step! (generator)
  "GENERATOR's settings brought to the model the server has loaded, when
that is a recipe other than the one they came from."
  (let ((loaded (generator-loaded generator)))
    (when (and loaded (not (equal loaded (generator-recipe generator))))
      (let ((recipe (find loaded (let ((*generator-recipe-sources* nil)) (generator-recipes generator))
                          :key #'first :test #'equal)))
        (adopt-recipe! generator loaded (third recipe))))
    generator))

(defun generator-model-info (generator)
  "What GENERATOR runs on, as a plist: :recipe and the settings sent."
  (list :recipe (generator-recipe generator)
        :steps (generator-steps generator) :cfg-scale (generator-cfg-scale generator)
        :sampler (generator-sampler generator) :scheduler (generator-scheduler generator)))

(defun use-generator-model! (id)
  "The generator's model changed to recipe ID, and Stratum's settings with
it: the generator's model-info.  Slow when the model is not in memory: a
minute or so for a large one.  Refused while the server is busy with a
picture."
  (let* ((generator (require-generator))
         (recipes (generator-recipes generator))
         (recipe (or (find id recipes :key #'first :test #'equal)
                     (error "The image generator offers no model ~s; it offers ~{~a~^, ~}."
                            id (mapcar #'first recipes))))
         (reply nil) (status nil))
    (funcall (generator-turn generator)
             (lambda ()
               (multiple-value-setq (reply status)
                 (generator-post generator "/sdcpp/v1/models/load" (json-table "recipe" id)))))
    (cond ((eql status 200))
          ((null status) (error "The image generator's turn never came."))
          ((eql status 409) (error "The image generator is busy with a picture; change the model when it is done."))
          (t (error "The image generator could not load ~a (it answered ~a~@[: ~a~])."
                    id status (and (hash-table-p reply)
                                   (let ((why (or (gethash "error" reply) (gethash "message" reply))))
                                     (if (hash-table-p why) (gethash "message" why) why))))))
    (adopt-recipe! generator id (third recipe))
    (generator-model-info generator)))

(defun stratum-api:generator-models ()
  "The image generator's models, ((id name) ...), and the one loaded:
(:models ... :loaded id)."
  (let ((generator (require-generator)))
    (list :models (mapcar (lambda (r) (list (first r) (second r))) (generator-recipes generator))
          :loaded (generator-loaded generator))))

(defun stratum-api:use-generator-model (id)
  "The image generator's model changed to ID, for everyone using this
server; what it now runs on."
  (use-generator-model! id))

(defun generator-state ()
  "What this server can do about pictures, for the model that plans."
  (cond ((null *generator*)
         "has no image generator: generate-fill, generate-layer, generate-background and generate-picture will fail")
        ((eq (generator-references *generator*) :no)
         "has an image generator whose model cannot be shown a picture: generate-picture starts from the picture, by :strength, and its prompt describes the result")
        (t "has an image generator")))

(defparameter *fill-fade-pixels* 3
  "How wide a fill's fade is: the sigma of its blur, in the generator's
pixels.  The seam it hides is as coarse as those pixels.  It does not
follow the size of the selection, whose bounding box may span the
picture while the part that matters is narrow: a fade set by that box
left the holes around a bench's legs showing through their fill.")

(defun faded-coverage (coverage w h painted-width)
  "COVERAGE, W x H bytes, fading to nothing at its edge over a few of
the generator's pixels, PAINTED-WIDTH of them across these W.  A
generator's idea of the surroundings is never quite the picture's, so a
hard edge shows as a pasted square; and each byte is at most what it
was, so nothing outside the selection can change."
  (let* ((sigma (max 3d0 (/ (* *fill-fade-pixels* w) (float painted-width 1d0))))
         (blurred (feather-mask-exact coverage w h sigma))
         (out (make-gray w h)))
    (dotimes (i (* w h) out)
      ;; the blur is half way at a straight edge, and whole well inside
      (setf (aref out i)
            (floor (* (aref coverage i) (max 0 (- (* 2 (aref blurred i)) 255))) 255)))))

(defparameter *changed-low* 24
  "A difference between what a generator was sent and what it painted,
in levels of the most different channel, under which a pixel is taken
as unchanged: a generator's copy of what it was shown differs by a few
levels everywhere.")

(defparameter *changed-high* 80
  "The difference above which a pixel is kept whole; between the two it
is kept in proportion.")

(defun changed-matte (sent generated w h)
  "SENT and GENERATED, RGBA W x H: a byte for each pixel, 255 where the
generator painted something else, 0 where much the same, in between in
proportion.  The difference is smoothed first, so that a likeness which
is merely noisy does not speckle and the edge of what is kept is soft."
  (let ((difference (make-gray w h)))
    (dotimes (i (* w h))
      (let ((j (* 4 i)))
        (setf (aref difference i)
              (max (abs (- (aref generated j) (aref sent j)))
                   (abs (- (aref generated (+ j 1)) (aref sent (+ j 1))))
                   (abs (- (aref generated (+ j 2)) (aref sent (+ j 2))))))))
    (let ((smoothed (feather-mask-exact difference w h (max 2d0 (/ (max w h) 200d0))))
          (span (max 1 (- *changed-high* *changed-low*))))
      (dotimes (i (* w h) smoothed)
        (setf (aref smoothed i)
              (max 0 (min 255 (floor (* 255 (- (aref smoothed i) *changed-low*)) span))))))))

(defun keep-changed! (coverage rw rh sent sw sh generated w h)
  "COVERAGE, RW x RH bytes, kept only where the generator changed the
picture: SENT, SW x SH, is what it was shown and GENERATED, W x H, what
it painted; the matte is made at the generator's size and scaled to the
crop.  COVERAGE itself, changed."
  (let* ((generated (if (and (= w sw) (= h sh)) generated (resample-rgba generated w h sw sh)))
         (matte (resample-gray (changed-matte sent generated sw sh) sw sh rw rh)))
    (dotimes (i (* rw rh) coverage)
      (setf (aref coverage i) (floor (* (aref coverage i) (aref matte i)) 255)))))

(defun generated-pixels (doc pixels w h rect coverage)
  "The bytes of a layer of DOC holding PIXELS, W x H, scaled to RECT, each
pixel's alpha its own times COVERAGE, the selection over RECT, or its
own alone with none.  Seconds on a photograph, and nothing here touches
the stack, so it wants no lock."
  (destructuring-bind (x y rw rh) rect
    (let* ((dw (the-object doc doc-width))
           (dh (the-object doc doc-height))
           (scaled (if (and (= w rw) (= h rh)) pixels (resample-rgba pixels w h rw rh)))
           (layer (make-rgba dw dh)))
      (unless (and (<= (+ x rw) dw) (<= (+ y rh) dh))
        (error "The document changed size while the picture was being made."))
      (dotimes (row rh)
        (dotimes (col rw)
          (let ((from (* 4 (+ col (* row rw))))
                (to (* 4 (+ x col (* (+ y row) dw)))))
            (replace layer scaled :start1 to :start2 from :end2 (+ from 3))
            (setf (aref layer (+ to 3))
                  (if coverage
                      (floor (* (aref scaled (+ from 3)) (aref coverage (+ col (* row rw)))) 255)
                      (aref scaled (+ from 3)))))))
      (doc-pixels doc layer))))

(defun generated-layer! (doc layer name)
  "LAYER, bytes from generated-pixels, as a new raster layer on top of
DOC, whose lock is held.  The layer's index."
  (unless (= (length layer) (* 4 (the-object doc doc-width) (the-object doc doc-height)))
    (error "The document changed size while the picture was being made."))
  (the-object doc (add-layer! (list :kind :raster :name name :pixels layer))))

(defun whole-layer! (doc pixels w h name)
  "PIXELS, W x H, stretched over the whole of DOC as a new raster layer,
taking the lock only to add it.  A prompt's journal keeps the picture
itself, a tenth of the stretched layer's size on the wire even packed.
The layer's layer-info."
  (let ((layer (generated-pixels doc pixels w h
                                 (list 0 0 (the-object doc doc-width) (the-object doc doc-height))
                                 nil))
        (png (and (the-object doc prompt-journal) (png-octets (doc-pixels doc pixels) w h))))
    (bt:with-lock-held (*lock*)
      (let ((index (generated-layer! doc layer name))
            (journal (the-object doc prompt-journal)))
        (when (and journal png)
          (push (list (the-object (the-object doc (layer index)) pixel-stamp) png w h)
                (prompt-journal-stretched journal)))
        (stratum-api:layer-info index)))))

(defun stratum-api:generate-fill (prompt &key (name "Generated") (seed -1) (keep :changed))
  "PROMPT painted into the selection by the image generator, as a new
raster layer NAME visible only inside the selection, and with KEEP
:changed, the default, only where the generator changed the picture;
:all keeps every pixel of the selection.  Slow: a minute or so.  The
layer's layer-info."
  (unless (member keep '(:changed :all))
    (error "generate-fill's :keep is :changed or :all, not ~s." keep))
  (let ((generator (require-generator))
        (doc (stratum-api::doc))
        rect coverage body sent sw sh)
    (bt:with-lock-held (*lock*)
      (let ((selection (the-object doc selection))
            (dw (the-object doc doc-width))
            (dh (the-object doc doc-height)))
        (unless selection
          (error "Nothing is selected: generate-fill repaints the selection."))
        (setf rect (grown-rect (getf selection :rect) dw dh))
        (destructuring-bind (x y rw rh) rect
          (multiple-value-setq (sw sh) (fitted-size rw rh (generator-size generator)))
          (setf coverage (rgba-rect (getf selection :mask) dw dh x y rw rh 1)
                sent (resample-rgba (rgba-rect (the-object doc (render-cpu)) dw dh x y rw rh)
                                    rw rh sw sh)
                body (generator-body generator prompt seed sw sh
                                     :image (png-octets sent sw sh)
                                     :mask (gray-png (resample-gray coverage rw rh sw sh) sw sh
                                                     (generator-invert-mask generator)))))))
    (multiple-value-bind (pixels w h) (ask-generator generator body)
      (destructuring-bind (x y rw rh) rect
        (declare (ignore x y))
        ;; the alpha is worked out before the lock is taken: nothing
        ;; here reads the document
        (let ((coverage (faded-coverage coverage rw rh sw)))
          (when (eq keep :changed)
            (keep-changed! coverage rw rh sent sw sh pixels w h))
          (let ((layer (generated-pixels doc pixels w h rect coverage)))
            (bt:with-lock-held (*lock*)
              (stratum-api:layer-info (generated-layer! doc layer name)))))))))

(defun stratum-api:generate-layer (prompt &key (name "Generated") (seed -1) references)
  "A picture of PROMPT from the image generator, the document's shape,
as a new raster layer NAME on top.  With REFERENCES -- t for every
reference picture attached to the prompt box, one's number, or a list of
numbers -- the model is shown those and PROMPT reads as an instruction
about them; a model that cannot be shown a picture is an error then,
since it would answer from the words alone and say nothing.  Slow: a
minute or so.  The layer's layer-info, with a :note when the generator
did not say whether its model can be shown a picture."
  (let* ((generator (require-generator))
         (doc (stratum-api::doc))
         (shown (bt:with-lock-held (*lock*) (mapcar #'ref-png (chosen-references references))))
         (takes (and shown (generator-references generator)))
         (note (and shown (not takes) *unsaid-reference-note*))
         (dw (the-object doc doc-width))
         (dh (the-object doc doc-height)))
    (when (eq takes :no)
      (error "This model cannot be shown a picture, so the reference picture~p cannot be shown to it. (place-reference n) puts one in the document as it is."
             (length shown)))
    (multiple-value-bind (sw sh) (fitted-size dw dh (generator-size generator))
      (multiple-value-bind (pixels w h)
          (ask-generator generator (generator-body generator prompt seed sw sh :reference shown))
        (let ((info (whole-layer! doc pixels w h name)))
          (if note (append info (list :note note)) info))))))

(defparameter *background-reach* 6
  "How far, in the picture's pixels, a background made around a subject
reaches in under the subject's edge: the generator's fade and the
subject's own feather both fall there, under the subject, so no rim of
anything else shows between them.")

(defun around-coverage (pixels w h sw sh reach)
  "The generator's mask, SW x SH bytes, for a background around the
subject of PIXELS, RGBA W x H: 255 where the subject is not wholly
opaque, or within REACH of the picture's pixels of such a place, 0
deeper in.  Worked at the generator's size, a megapixel or so, and not
the photograph's."
  (let ((outside (make-gray w h)))
    (dotimes (i (* w h))
      (when (< (aref pixels (+ 3 (* 4 i))) 255)
        (setf (aref outside i) 255)))
    (let* ((small (resample-gray outside w h sw sh))
           (sites (make-gray sw sh))
           (reach (max 1d0 (/ (* reach sw) (float w 1d0))))
           (limit (* reach reach))
           (out (make-gray sw sh)))
      ;; a generator's pixel touched by anything outside counts as outside
      (dotimes (i (* sw sh))
        (when (plusp (aref small i))
          (setf (aref sites i) 255)))
      (let ((grid (distances-to sites sw (list 0 0 sw sh) #'selected-byte?)))
        (dotimes (i (* sw sh) out)
          (when (<= (aref grid i) limit)
            (setf (aref out i) 255)))))))

(defun over-gray (pixels w h)
  "PIXELS, RGBA W x H, over middle gray: what a generator is shown of a
subject alone, with nothing of an old background for it to keep."
  (let ((out (make-rgba w h)))
    (dotimes (i (* w h) out)
      (let* ((j (* 4 i))
             (a (aref pixels (+ j 3))))
        (dotimes (c 3)
          (setf (aref out (+ j c))
                (round (+ (* (aref pixels (+ j c)) a) (* 128 (- 255 a))) 255)))
        (setf (aref out (+ j 3)) 255)))))

(defun stratum-api:generate-background (prompt &key layer (name "New background") (seed -1))
  "PROMPT painted around the subject on raster LAYER (the top layer by
default) as a new raster layer NAME just under it.  The generator is
shown the subject and paints everywhere else, so the ground meets the
subject's feet and the light falls on the scene as on the subject;
LAYER itself does not change.  Slow: a minute or so.  The new layer's
layer-info."
  (let ((generator (require-generator))
        (doc (stratum-api::doc))
        subject dw dh body)
    (bt:with-lock-held (*lock*)
      (setf subject (stratum-api::raster-layer-of
                     (or layer (1- (length (the-object doc layer-list)))))
            dw (the-object doc doc-width)
            dh (the-object doc doc-height))
      (multiple-value-bind (sw sh) (fitted-size dw dh (generator-size generator))
        (let ((pixels (the-object subject pixels)))
          (setf body (generator-body
                      generator prompt seed sw sh
                      :image (png-octets (resample-rgba (over-gray pixels dw dh) dw dh sw sh) sw sh)
                      :mask (gray-png (around-coverage pixels dw dh sw sh *background-reach*)
                                      sw sh (generator-invert-mask generator))
                      ;; nothing outside the subject is worth keeping
                      :strength 1d0)))))
    (multiple-value-bind (pixels w h) (ask-generator generator body)
      (let ((layer (generated-pixels doc pixels w h (list 0 0 dw dh) nil))
            ;; as whole-layer!: the page stretches the picture itself
            (png (and (the-object doc prompt-journal) (png-octets (doc-pixels doc pixels) w h))))
        (bt:with-lock-held (*lock*)
          (let ((below (position subject (the-object doc layer-list)))
                (journal (the-object doc prompt-journal)))
            (unless below
              (error "The subject's layer went while its background was being made."))
            (unless (= (length layer) (* 4 (the-object doc doc-width) (the-object doc doc-height)))
              (error "The document changed size while the picture was being made."))
            (let ((index (the-object doc (add-layer! (list :kind :raster :name name :pixels layer)
                                                     (1- below)))))
              (when (and journal png)
                (push (list (the-object (the-object doc (layer index)) pixel-stamp) png w h)
                      (prompt-journal-stretched journal)))
              (stratum-api:layer-info index))))))))

(defun stratum-api:generate-picture (prompt &key (name "Generated") (seed -1) (reference t) (strength 0.35)
                                              references)
  "The open picture made over by PROMPT, as a new raster layer NAME on
top of it, the picture's shape.  With REFERENCE the model is shown the
picture and PROMPT reads as an instruction: said to keep the composition,
it does.  With none it starts from the picture with STRENGTH of noise,
0 to 1, and PROMPT describes the result: 0.3 keeps the pose and little
of the style, 0.6 redraws the subject.  A model that cannot be shown a
picture would answer from the words alone and say nothing, so with one
of those it starts from the picture all the same.  With REFERENCES -- t
for every reference picture attached to the prompt box, one's number, or
a list of numbers -- the model is shown those too, after the picture, in
that order; they need REFERENCE.  Slow: a minute or so.  The layer's
layer-info, with a :note when REFERENCE did not go as asked."
  (when (and references (not reference))
    (error "Reference pictures are shown to the generator beside the document's picture: :references needs :reference t."))
  (let* ((generator (require-generator))
         (doc (stratum-api::doc))
         (shown (bt:with-lock-held (*lock*) (mapcar #'ref-png (chosen-references references))))
         (takes (and reference (generator-references generator)))
         (reference (and reference (not (eq takes :no))))
         (note (cond (reference
                      (unless takes *unsaid-reference-note*))
                     ((eq takes :no)
                      (format nil "This model cannot be shown a picture, so it started from the picture, with ~,2f of noise~@[, and the reference picture~p went unused~]."
                              strength (and shown (length shown))))))
         dw dh sw sh body)
    (bt:with-lock-held (*lock*)
      (setf dw (the-object doc doc-width)
            dh (the-object doc doc-height))
      (multiple-value-setq (sw sh) (fitted-size dw dh (generator-size generator)))
      (let ((picture (png-octets (resample-rgba (the-object doc (render-cpu)) dw dh sw sh) sw sh)))
        (setf body (if reference
                       (generator-body generator prompt seed sw sh :reference (cons picture shown))
                       (generator-body generator prompt seed sw sh :image picture :strength strength)))))
    (multiple-value-bind (pixels w h) (ask-generator generator body)
      (let ((info (whole-layer! doc pixels w h name)))
        (if note (append info (list :note note)) info)))))

(defparameter *long-operators* '(stratum-api:generate-fill stratum-api:generate-layer
                                 stratum-api:generate-background stratum-api:generate-picture)
  "The operations run-forms calls without the lock, which they take for
themselves: a generation would hold up the session's other requests
for a minute.")
