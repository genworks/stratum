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
;; A generation takes many seconds, so these operations run outside the
;; server's lock (run-forms, *long-operators*) and take it themselves to
;; read the document and to add the layer.
;;

(defstruct (generator (:constructor make-generator
                          (url steps cfg-scale strength size negative seconds invert-mask)))
  "A generator's address and how to ask it.  URL is its base, with no
trailing slash; STEPS, CFG-SCALE and STRENGTH (denoising, 0-1) suit its
model; SIZE is the longer side, in pixels, of what it is sent; NEGATIVE
a negative prompt; SECONDS how long to wait; INVERT-MASK true when its
model wants the selection dark."
  url steps cfg-scale strength size negative seconds invert-mask)

(defvar *generator* nil
  "The image generator, or nil: there is none by default.")

(defun configure-generator! (&key url (steps 28) (cfg-scale 7) (strength 0.9d0) (size 1024)
                                  (negative "") (seconds 300) invert-mask)
  "Generate images through the service at URL, or not at all with nil.
The generator as it stands."
  (setf *generator* (and url (make-generator (string-right-trim "/" url) steps cfg-scale strength
                                             size negative seconds invert-mask))))

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

(defun data-url (png)
  (format nil "data:image/png;base64,~a" (sexp-agent:base64-encode png)))

(defun generator-body (generator prompt seed w h &optional image mask)
  "The job for a picture of W x H, as JSON text; with IMAGE and MASK,
PNG octets, a repainting of them."
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
                        "output_format" "png"
                        "sample_params" (object "sample_steps" (generator-steps generator)
                                                "guidance" (object "txt_cfg" (float (generator-cfg-scale generator) 1d0))))))
      (when image
        (setf (gethash "init_image" body) (data-url image)
              (gethash "mask_image" body) (data-url mask)
              (gethash "strength" body) (float (generator-strength generator) 1d0)))
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

(defun ask-generator (generator body)
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

(defun require-generator ()
  (or *generator* (error "This server has no image generator.")))

(defun faded-coverage (coverage w h)
  "COVERAGE, W x H bytes, fading to nothing at its edge over about a
fiftieth of its longer side.  A generator's idea of the surroundings is
never quite the picture's, so a hard edge shows as a pasted square; and
each byte is at most what it was, so nothing outside the selection can
change."
  (let* ((sigma (max 3d0 (/ (max w h) 50d0)))
         (blurred (feather-mask-exact coverage w h sigma))
         (out (make-gray w h)))
    (dotimes (i (* w h) out)
      ;; the blur is half way at a straight edge, and whole well inside
      (setf (aref out i)
            (floor (* (aref coverage i) (max 0 (- (* 2 (aref blurred i)) 255))) 255)))))

(defun generated-layer! (doc pixels w h rect coverage name)
  "PIXELS, W x H, scaled to RECT of DOC and laid there as a new raster
layer on top, each pixel's alpha its own times COVERAGE, the selection
over RECT, or its own alone with none.  The layer's index."
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
      (the-object doc (add-layer! (list :kind :raster :name name :pixels (doc-pixels doc layer)))))))

(defun stratum-api:generate-fill (prompt &key (name "Generated") (seed -1))
  "PROMPT painted into the selection by the image generator, as a new
raster layer NAME visible only inside the selection.  Slow: a minute or
so.  The layer's layer-info."
  (let ((generator (require-generator))
        (doc (stratum-api::doc))
        rect coverage body)
    (bt:with-lock-held (*lock*)
      (let ((selection (the-object doc selection))
            (dw (the-object doc doc-width))
            (dh (the-object doc doc-height)))
        (unless selection
          (error "Nothing is selected: generate-fill repaints the selection."))
        (setf rect (grown-rect (getf selection :rect) dw dh))
        (destructuring-bind (x y rw rh) rect
          (multiple-value-bind (sw sh) (fitted-size rw rh (generator-size generator))
            (setf coverage (rgba-rect (getf selection :mask) dw dh x y rw rh 1)
                  body (generator-body generator prompt seed sw sh
                                       (png-octets (resample-rgba (rgba-rect (the-object doc (render-cpu))
                                                                             dw dh x y rw rh)
                                                                  rw rh sw sh)
                                                   sw sh)
                                       (gray-png (resample-gray coverage rw rh sw sh) sw sh
                                                 (generator-invert-mask generator))))))))
    (multiple-value-bind (pixels w h) (ask-generator generator body)
      (bt:with-lock-held (*lock*)
        (stratum-api:layer-info
         (generated-layer! doc pixels w h rect
                           (destructuring-bind (x y rw rh) rect
                             (declare (ignore x y))
                             (faded-coverage coverage rw rh))
                           name))))))

(defun stratum-api:generate-layer (prompt &key (name "Generated") (seed -1))
  "A picture of PROMPT from the image generator, the document's shape,
as a new raster layer NAME on top.  Slow: a minute or so.  The layer's
layer-info."
  (let* ((generator (require-generator))
         (doc (stratum-api::doc))
         (dw (the-object doc doc-width))
         (dh (the-object doc doc-height)))
    (multiple-value-bind (sw sh) (fitted-size dw dh (generator-size generator))
      (multiple-value-bind (pixels w h)
          (ask-generator generator (generator-body generator prompt seed sw sh))
        (bt:with-lock-held (*lock*)
          (stratum-api:layer-info (generated-layer! doc pixels w h (list 0 0 dw dh) nil name)))))))

(defparameter *long-operators* '(stratum-api:generate-fill stratum-api:generate-layer)
  "The operations run-forms calls without the lock, which they take for
themselves: a generation would hold every other request up for a
minute.")
