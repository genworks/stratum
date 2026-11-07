;;;; -*- Mode: Lisp; Package: stratum -*-

;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :stratum)

;;
;; The web face: a GWL page that hosts the WebGL2 viewer, and a small
;; JSON and raw-bytes API.  The page fetches the document's description
;; (its shader source included), uploads each layer's pixels once, and
;; from then on renders on the GPU.  Edits go back with POST
;; /stratum/api/set; when an edit changes the plan the reply carries a
;; new planVersion and the viewer asks for the new shader.  Documents
;; are shared, one per sample name, until sessions are wired in.
;;

(defparameter *stratum-dir*
  (let ((base (glisp:source-pathname)))
    (make-pathname :name nil :type nil
                   :directory (butlast (pathname-directory base))
                   :defaults base)))

(defvar *sessions* (make-hash-table :test 'equal)
  "Session id -> its session (document.lisp).  A browser keeps its id in
its own storage and sends it as ?s= on every call, so two browsers
opening \"demo\" get two documents and two clipboards.")

(defparameter *session-idle-seconds* (* 4 3600)
  "A session untouched this long is forgotten, documents and all.")

(defvar *lock* (bt:make-lock "stratum")
  "Held around every request: one GDL tree, many server threads.")

(defun sweep-sessions! ()
  "Forget every session idle longer than *session-idle-seconds*."
  (let ((now (get-universal-time)) (gone '()))
    (maphash (lambda (id session)
               (when (> (- now (sess-touched session)) *session-idle-seconds*) (push id gone)))
             *sessions*)
    (dolist (id gone) (remhash id *sessions*))
    gone))

(defparameter *max-upload-bytes* 100000000
  "The largest body the upload doors (open, place, load) take: a
photo of about 25 megapixels as raw RGBA, and what a content network
in front of the server passes anyway.  The page is told (document-json)
and says so before it sends anything.")

(defparameter *max-request-bytes* (* 16 1024 1024)
  "The largest body any other door takes.  Those read their body as a
string, four bytes a character on SBCL.")

(defparameter *drained-body-bytes* 200000000
  "How much of a body refused by an invited person is read and dropped
so the words reach them; past this the connection is closed on it.
Apart from the door limits on purpose: a body just over any limit must
still hear why.")

(defparameter *upload-heap-factor* 3
  "An upload is admitted when the heap has this many times its size
free: the octets, an undo entry of them, and a render's working room.")

(defvar *upload-octets* nil
  "The body of the upload being served, read before the lock is taken.")

(defvar *upload-read-ms* nil
  "How long that body took to arrive, in milliseconds.")

(defun elapsed-ms (began)
  "Milliseconds since BEGAN, a get-internal-real-time."
  (round (* 1000 (- (get-internal-real-time) began)) internal-time-units-per-second))

(defparameter *heap-high-water* 0.7
  "The fraction of the heap in use above which relieve-memory! acts.")

(defun heap-used-fraction ()
  "How much of the heap is in use, 0 to 1, where the implementation can
say (SBCL); nil elsewhere, and then relieve-memory! never acts."
  #+sbcl (/ (float (sb-kernel:dynamic-usage) 1d0) (sb-ext:dynamic-space-size))
  #-sbcl nil)

(defun full-gc ()
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  #-(or sbcl ccl) nil)

(defparameter *form-high-water* 0.5
  "The fraction of the heap in use above which a form of the Lisp API
is preceded by a collection: on a photograph one form can want a few
hundred megabytes, and garbage counts as use until it is collected.")

(defun collect-if-full! ()
  "A full collection when the heap is above *form-high-water*."
  (let ((used (heap-used-fraction)))
    (when (and used (> used *form-high-water*))
      (full-gc))))

(defun relieve-memory! (keep-id)
  "When the heap is above *heap-high-water*: collect first, since the
measure counts garbage not yet collected, and if it is still above,
drop every document's undo and redo lists, collect, and while still
above forget the sessions touched longest ago, except KEEP-ID's.
Returns what it did, or nil."
  (let ((before (heap-used-fraction)))
    (when (and before (> before *heap-high-water*))
      (full-gc)
      (when (> (heap-used-fraction) *heap-high-water*)
        (let ((done (list :collected)))
          (flet ((drop-history (table)
                   (maphash (lambda (name doc)
                              (declare (ignore name))
                              (the-object doc (set-slot! :undo-entries nil))
                              (the-object doc (set-slot! :redo-entries nil)))
                            table)))
            (drop-history *documents*)
            (maphash (lambda (id session) (declare (ignore id)) (drop-history (sess-table session))) *sessions*)
            (push :undo-dropped done)
            (full-gc)
            (loop while (and (> (heap-used-fraction) *heap-high-water*)
                             (> (hash-table-count *sessions*) 0))
                  do (let ((oldest nil) (when-touched nil))
                       (maphash (lambda (id session)
                                  (unless (equal id keep-id)
                                    (when (or (null when-touched) (< (sess-touched session) when-touched))
                                      (setf oldest id when-touched (sess-touched session)))))
                                *sessions*)
                       (unless oldest (return))
                       (remhash oldest *sessions*)
                       (push (list :session-forgotten oldest) done)
                       (full-gc))))
          (nreverse done))))))

(defun session-for (id)
  "The session of ID, made on first sight and touched now; the shared
session for no id.  A touch also sweeps idle sessions and, when the heap
is short, relieves it (relieve-memory!)."
  (if (or (null id) (string= id ""))
      *shared-session*
      (let ((session (or (gethash id *sessions*)
                         (setf (gethash id *sessions*) (%make-session)))))
        (setf (sess-touched session) (get-universal-time))
        (sweep-sessions!)
        (relieve-memory! id)
        session)))

(defun session-table (id)
  "The documents table of session ID; the shared pool for no id."
  (sess-table (session-for id)))

(defun request-session (req)
  (session-for (net.aserve:request-query-value "s" req)))

(defun find-document (name)
  "The session's document NAME, a sample made on first sight; its
history source taken if it has none (the state at the first request)."
  (let ((doc (or (gethash name *documents*)
                 (setf (gethash name *documents*) (make-sample-document name)))))
    (ensure-history-specs! doc)
    doc))

(defun request-doc (req)
  (find-document (or (net.aserve:request-query-value "doc" req) "demo")))

(defun request-index (req)
  (parse-integer (net.aserve:request-query-value "i" req)))

(defun json-object (&rest keys-and-values)
  (let ((h (make-hash-table :test 'equal)))
    (loop for (k v) on keys-and-values by #'cddr
          do (setf (gethash k h) v))
    h))

(defun json-bool (x) (if x 'yason:true 'yason:false))

(defun json-string (object)
  (yason:with-output-to-string* () (yason:encode object)))

(defun layer-json (layer)
  (let ((kind (the-object layer kind)))
    (json-object
     "index" (the-object layer index)
     "name" (the-object layer layer-name)
     "kind" (string-downcase kind)
     "opacity" (the-object layer opacity)
     "visible" (json-bool (the-object layer visible?))
     "mask" (json-bool (the-object layer has-mask?))
     "maskIndex" (the-object layer mask-index)
     "blend" (when (eql kind :raster) (string-downcase (the-object layer blend-mode)))
     "texIndex" (when (eql kind :raster) (the-object layer texture-index))
     "paintVersion" (when (eql kind :raster) (the-object layer paint-version))
     "maskVersion" (the-object layer mask-version)
     "pixelStamp" (when (eql kind :raster) (the-object layer pixel-stamp))
     "maskStamp" (the-object layer mask-stamp)
     "lutRow" (when (typep layer 'lut-layer) (the-object layer lut-row))
     "settings" (let ((names (the-object layer setting-names)))
                  (when names
                    (apply #'json-object
                           (loop for name in names
                                 collect (string-downcase name)
                                 collect (float (the-object layer (evaluate name)) 1d0)))))
     "curves" (when (eql kind :curves)
                (json-object "master" (normalize-points (the-object layer master))
                             "red" (normalize-points (the-object layer red))
                             "green" (normalize-points (the-object layer green))
                             "blue" (normalize-points (the-object layer blue)))))))

(defun document-json (doc)
  (json-object
   "name" (the-object doc doc-name)
   "width" (the-object doc doc-width)
   "height" (the-object doc doc-height)
   "planVersion" (the-object doc plan-version)
   "shader" (the-object doc shader-source)
   "filterShader" *filter-glsl*
   "shaderBuilds" (aref (the-object doc build-counts) 0)
   "kernelBuilds" (aref (the-object doc build-counts) 1)
   "rasterCount" (length (the-object doc raster-layers))
   "maskCount" (length (the-object doc masked-layers))
   "lutRows" (the-object doc lut-rows)
   "undoDepth" (length (the-object doc undo-entries))
   "redoDepth" (length (the-object doc redo-entries))
   "mode" (string-downcase (the-object doc color-mode))
   "historyVersion" (the-object doc history-version)
   "selection" (selection-json doc)
   "maxUploadBytes" *max-upload-bytes*
   "jpegUpload" (json-bool (turbojpeg?))
   "backgroundLinear" (mapcar (lambda (c) (float c 1d0)) (the-object doc background-linear))
   "layers" (mapcar #'layer-json (the-object doc layer-list))))

(defun handle-set (req)
  "Apply one edit -- {layer, slot, value} for opacity, visible and blend;
{layer, slot: \"setting\", name, value} for a levels or hue/saturation
setting; {layer, slot: \"curve\", channel, points} -- and answer the
document's planVersion."
  (let* ((doc (request-doc req))
         (body (yason:parse (net.aserve:get-request-body req)))
         (layer (the-object doc (layer (gethash "layer" body))))
         (slot (gethash "slot" body))
         (value (gethash "value" body)))
    (cond ((string= slot "opacity")
           (the-object layer (set-slot! :opacity (max 0d0 (min 1d0 (float value 1d0))))))
          ((string= slot "visible")
           (the-object layer (set-slot! :visible? (and value t))))
          ((string= slot "blend")
           (let ((mode (find value *blend-modes* :key #'string-downcase :test #'string-equal)))
             (unless mode (error "Unknown blend mode ~s." value))
             (the-object layer (set-slot! :blend-mode mode))))
          ((string= slot "setting")
           (let ((name (find (gethash "name" body) (the-object layer setting-names)
                             :test #'string-equal)))
             (unless name (error "~a has no setting ~s." (the-object layer layer-name)
                                 (gethash "name" body)))
             (the-object layer (set-slot! name (float value 1d0)))))
          ((string= slot "curve")
           (let ((channel (find (gethash "channel" body) '(:master :red :green :blue)
                                :test #'string-equal)))
             (unless channel (error "Unknown channel ~s." (gethash "channel" body)))
             (the-object layer (set-slot! channel (normalize-points (gethash "points" body))))))
          (t (error "Unknown slot ~s." slot)))
    (json-string (json-object "planVersion" (the-object doc plan-version)))))

(defun parse-json-body (req)
  "The request body as yason parses it, numbers read as double-floats:
a stroke's coordinates must come back as the doubles the browser sent."
  (let ((*read-default-float-format* 'double-float))
    (yason:parse (net.aserve:get-request-body req))))

(defun parse-stroke (body)
  "The stroke plist apply-brush-stroke takes, from a POST body:
samples [[x y pressure] ...] and brush {radius hardness spacing flow
opacity color mode}; missing brush numbers take *default-brush*."
  (let ((brush (gethash "brush" body)))
    (flet ((num (name key)
             (let ((v (and brush (gethash name brush))))
               (if v (float v 1d0) (getf *default-brush* key)))))
      (list :points (gethash "samples" body)
            :radius (num "radius" :radius) :hardness (num "hardness" :hardness)
            :spacing (num "spacing" :spacing) :flow (num "flow" :flow)
            :opacity (num "opacity" :opacity)
            :color (or (and brush (gethash "color" brush)) (getf *default-brush* :color))
            :mode (if (and brush (equal (gethash "mode" brush) "erase")) :erase :paint)
            :clone (let ((c (gethash "clone" body)))
                     (and (consp c) (= (length c) 2) (every #'realp c)
                          (list (round (first c)) (round (second c)))))
            :heal (let ((h (gethash "heal" body))) (and h (not (eql h 'yason:false))))
            :history (let ((h (gethash "history" body))) (and h (not (eql h 'yason:false))))))))

(defun rect-json (doc layer target rect)
  "The rectangle a stroke or an undo changed: on the layer's pixels
(TARGET :layer, with its texture index) or its mask (:mask, with its
mask index)."
  (json-object "layer" (the-object layer index)
               "target" (string-downcase target)
               "texIndex" (when (eql (the-object layer kind) :raster) (the-object layer texture-index))
               "maskIndex" (the-object layer mask-index)
               "x" (first rect) "y" (second rect) "w" (third rect) "h" (fourth rect)
               "paintVersion" (if (eql (the-object layer kind) :raster) (the-object layer paint-version) 0)
               "maskVersion" (the-object layer mask-version)
               "pixelStamp" (when (eql (the-object layer kind) :raster) (the-object layer pixel-stamp))
               "maskStamp" (the-object layer mask-stamp)
               "undoDepth" (length (the-object doc undo-entries))
               "redoDepth" (length (the-object doc redo-entries))))

(defun handle-stroke (req)
  "Commit one stroke -- {layer, target, samples, brush}, target \"layer\"
or \"mask\" -- and answer the dirty rectangle with the layer's texture
or mask index and new version, or {empty: true} when it touched
nothing.  With ?dry=1 nothing is painted: the answer is the server's
coverage buffer and its rectangle, for the browser to compare with its
own."
  (let* ((doc (request-doc req))
         (body (parse-json-body req))
         (layer (the-object doc (layer (gethash "layer" body))))
         (target (if (equal (gethash "target" body) "mask") :mask :layer))
         (stroke (parse-stroke body)))
    (json-string
     (if (net.aserve:request-query-value "dry" req)
         (multiple-value-bind (coverage x y w h)
             (stroke-coverage stroke (the-object doc doc-width) (the-object doc doc-height)
                              (the-object doc selection))
           (if coverage
               (json-object "x" x "y" y "w" w "h" h "coverage" (coerce coverage 'list))
               (json-object "empty" 'yason:true)))
         (let ((rect (apply-brush-stroke layer stroke :target target)))
           (if rect (rect-json doc layer target rect) (json-object "empty" 'yason:true)))))))

(defun entry-json (doc entry)
  "The rectangle an undo ENTRY put back, as rect-json gives it; for a
mode change, {mode, group: [one rectangle per layer]}; for a prompt,
{whole: true}: the stack itself changed, and the page asks for it."
  (case (first entry)
    (:prompt
     (json-object "whole" 'yason:true
                  "undoDepth" (length (the-object doc undo-entries))
                  "redoDepth" (length (the-object doc redo-entries))))
    (:mode
     (json-object "mode" (string-downcase (the-object doc color-mode))
                  "group" (mapcar (lambda (e) (rect-json doc (the-object doc (layer (first e))) (second e) (cddr e)))
                                  (cdddr entry))
                  "undoDepth" (length (the-object doc undo-entries))
                  "redoDepth" (length (the-object doc redo-entries))))
    (t (rect-json doc (the-object doc (layer (first entry))) (second entry) (cddr entry)))))

(defun handle-mode (req)
  "Image > Mode: {mode: \"rgb\" | \"grayscale\"}; answers {mode, group:
[the layers changed, as rectangles]}, the group empty when nothing
changed."
  (let* ((doc (request-doc req))
         (body (parse-json-body req))
         (mode (find (or (gethash "mode" body) "rgb") '(:rgb :grayscale) :test #'string-equal)))
    (unless mode (error "Unknown image mode ~s." (gethash "mode" body)))
    (let ((entry (set-color-mode! doc mode)))
      (json-string (entry-json doc (or entry (list :mode mode mode)))))))

(defun handle-undo (req)
  "Undo the document's last change and answer the rectangle put back, as
handle-stroke does; {empty: true} when there was nothing to undo."
  (let* ((doc (request-doc req))
         (entry (undo-stroke! doc)))
    (json-string
     (if entry
         (entry-json doc entry)
         (json-object "empty" 'yason:true)))))

(defun handle-redo (req)
  "Redo the document's last undo and answer the rectangle put back, as
handle-stroke does; {empty: true} when there was nothing to redo."
  (let* ((doc (request-doc req))
         (entry (redo-stroke! doc)))
    (json-string
     (if entry
         (entry-json doc entry)
         (json-object "empty" 'yason:true)))))

(defun selection-json (doc)
  (let ((selection (the-object doc selection)))
    (if selection
        (destructuring-bind (x y w h) (getf selection :rect)
          (json-object "x" x "y" y "w" w "h" h))
        :null)))

(defun handle-select (req)
  "The selection: {op: \"rect\", x, y, w, h}, {op: \"polygon\", points:
[[x, y], ...]}, {op: \"all\"}, {op: \"none\"} or {op: \"inverse\"};
answers {selection: {x, y, w, h} or null}.  GET selection brings the
bytes."
  (let* ((doc (request-doc req))
         (body (parse-json-body req))
         (op (gethash "op" body))
         (mode (let ((m (gethash "mode" body)))
                 (cond ((equal m "add") :add) ((equal m "subtract") :subtract)
                       ((equal m "intersect") :intersect) (t :new)))))
    (flet ((n (key) (round (gethash key body))))
      (cond ((equal op "rect") (select-rect! doc (n "x") (n "y") (n "w") (n "h") mode))
            ((equal op "polygon") (select-polygon! doc (gethash "points" body) mode))
            ((equal op "wand")
             (select-wand! doc (n "x") (n "y")
                           :tolerance (or (gethash "tolerance" body) 32)
                           :contiguous (let ((c (gethash "contiguous" body)))
                                         (not (or (null c) (eql c 'yason:false))))
                           :mode mode
                           :layer (let ((i (gethash "layer" body)))
                                    (and (integerp i) (< -1 i (length (the-object doc layer-list)))
                                         (the-object doc (layer i))))))
            ((equal op "feather") (feather-selection! doc (or (gethash "radius" body) 1)))
            ((equal op "all") (select-all! doc))
            ((equal op "none") (select-none! doc))
            ((equal op "inverse") (select-inverse! doc))
            (t (error "Unknown selection operation ~s." op))))
    (json-string (json-object "selection" (selection-json doc)))))

(defun handle-fill (req)
  "Edit > Fill or Clear: {layer, color: [r, g, b], opacity, mode: \"paint\"
or \"erase\"} inside the selection; answers the rectangle as
handle-stroke does."
  (let* ((doc (request-doc req))
         (body (parse-json-body req))
         (layer (the-object doc (layer (gethash "layer" body)))))
    (json-string
     (rect-json doc layer :layer
                (fill-selection! layer (or (gethash "color" body) '(0 0 0))
                                 (or (gethash "opacity" body) 1)
                                 (equal (gethash "mode" body) "erase"))))))

(defun handle-move (req)
  "The Move tool's commit: {layer, dx, dy}; answers the whole layer as
a rectangle, with the selection's new rectangle."
  (let* ((doc (request-doc req))
         (body (parse-json-body req))
         (layer (the-object doc (layer (gethash "layer" body))))
         (rect (move-pixels! layer (round (gethash "dx" body)) (round (gethash "dy" body))))
         (json (rect-json doc layer :layer rect)))
    (setf (gethash "selection" json) (selection-json doc))
    (json-string json)))

(defun handle-transform (req)
  "Free Transform's commit: {layer, matrix: [a, b, c, d, e, f]}; answers
the whole layer as a rectangle, with the selection's new rectangle."
  (let* ((doc (request-doc req))
         (body (parse-json-body req))
         (layer (the-object doc (layer (gethash "layer" body))))
         (rect (transform-pixels! layer (gethash "matrix" body)))
         (json (rect-json doc layer :layer rect)))
    (setf (gethash "selection" json) (selection-json doc))
    (json-string json)))

(defun handle-clipboard (req)
  "{op: \"copy\" | \"cut\" | \"paste\", layer, above}.  Copy answers the
clipboard's size; Cut the rectangle cleared, as handle-stroke does;
Paste the new layer as handle-layers does."
  (let* ((doc (request-doc req))
         (body (parse-json-body req))
         (op (gethash "op" body))
         (layer (and (gethash "layer" body) (the-object doc (layer (gethash "layer" body))))))
    (json-string
     (cond ((equal op "copy")
            (copy-selection! layer)
            (json-object "clipboard" (json-object "w" (getf (clipboard) :w) "h" (getf (clipboard) :h))))
           ((equal op "cut")
            (rect-json doc layer :layer (cut-selection! layer)))
           ((equal op "paste")
            (let ((active (paste-clipboard! doc (gethash "above" body))))
              (json-object "planVersion" (the-object doc plan-version)
                           "active" active
                           "layerCount" (length (the-object doc layer-list)))))
           (t (error "Unknown clipboard operation ~s." op))))))

(defun handle-resize (req)
  "Image > Crop, Image Size, Canvas Size and Image Rotation: {op:
\"crop\"} (to the selection), {op: \"image-size\", width, height}, {op:
\"canvas-size\", width, height, anchor: [ax, ay]}, {op: \"rotate\", turn:
\"cw\" | \"ccw\" | \"180\" | \"flip-h\" | \"flip-v\"} or {op:
\"rotate-arbitrary\", degrees} (clockwise).  The document is remade
under its name; answers {width, height} and the page reloads."
  (let* ((name (or (net.aserve:request-query-value "doc" req) "demo"))
         (doc (find-document name))
         (body (parse-json-body req))
         (op (gethash "op" body)))
    (flet ((size (key) (let ((v (round (gethash key body))))
                         (unless (and (<= 1 v) (<= v 16384)) (error "Bad size ~a." v))
                         v)))
      (let ((new (cond ((equal op "crop")
                        (let ((selection (the-object doc selection)))
                          (unless selection (error "Nothing is selected to crop to."))
                          (apply #'crop-document doc (getf selection :rect))))
                       ((equal op "crop-rect")
                        (let* ((w (the-object doc doc-width)) (h (the-object doc doc-height))
                               (x (max 0 (min (1- w) (round (gethash "x" body)))))
                               (y (max 0 (min (1- h) (round (gethash "y" body)))))
                               (cw (max 1 (min (- w x) (round (gethash "w" body)))))
                               (ch (max 1 (min (- h y) (round (gethash "h" body))))))
                          (crop-document doc x y cw ch)))
                       ((equal op "image-size")
                        (resize-document doc (size "width") (size "height")))
                       ((equal op "canvas-size")
                        (let ((anchor (or (gethash "anchor" body) '(0.5 0.5))))
                          (canvas-size-document doc (size "width") (size "height")
                                                (float (first anchor) 1d0) (float (second anchor) 1d0))))
                       ((equal op "rotate")
                        (let ((turn (find (gethash "turn" body) '(:cw :ccw :180 :flip-h :flip-v) :test #'string-equal)))
                          (unless turn (error "Unknown turn ~s." (gethash "turn" body)))
                          (rotate-document doc turn)))
                       ((equal op "rotate-arbitrary")
                        (let ((degrees (gethash "degrees" body)))
                          (unless (realp degrees) (error "Bad angle ~s." degrees))
                          (rotate-document-arbitrary doc degrees)))
                       (t (error "Unknown resize operation ~s." op)))))
        (setf (gethash name *documents*) new)
        (json-string (json-object "width" (the-object new doc-width)
                                  "height" (the-object new doc-height)))))))

(defun handle-filter (req)
  "Filter > ...: {layer, kind: \"gaussian-blur\" | \"unsharp-mask\" |
\"add-noise\", settings: {name: value}} baked into the layer inside
the selection; answers the whole layer as a rectangle."
  (let* ((doc (request-doc req))
         (body (parse-json-body req))
         (layer (the-object doc (layer (gethash "layer" body))))
         (kind (find (gethash "kind" body) '(:gaussian-blur :unsharp-mask :add-noise) :test #'string-equal))
         (settings (let ((h (gethash "settings" body)))
                     (when h (loop for k being the hash-keys of h using (hash-value v)
                                   collect (intern (string-upcase k) :keyword)
                                   collect (if (numberp v) (float v 1d0) (and v (not (eql v 'yason:false)))))))))
    (unless kind (error "Unknown filter ~s." (gethash "kind" body)))
    (json-string (rect-json doc layer :layer (filter-layer! layer kind :settings settings)))))

(defun handle-gradient (req)
  "The Gradient tool's commit: {layer, x0, y0, x1, y1, start: [r, g,
b, a], end: [r, g, b, a], shape: \"linear\" | \"reflected\" | \"radial\"
| \"diamond\" | \"angle\", opacity} laid over the layer inside the
selection; answers the whole layer as a rectangle."
  (let* ((doc (request-doc req))
         (body (parse-json-body req))
         (layer (the-object doc (layer (gethash "layer" body))))
         (shape (find (or (gethash "shape" body) "linear") *gradient-shapes* :test #'string-equal)))
    (unless shape (error "Unknown gradient shape ~s." (gethash "shape" body)))
    (json-string
     (rect-json doc layer :layer
                (gradient-layer! layer (gethash "x0" body) (gethash "y0" body)
                                 (gethash "x1" body) (gethash "y1" body)
                                 :shape shape
                                 :start (or (gethash "start" body) '(0 0 0 255))
                                 :end (or (gethash "end" body) '(255 255 255 255))
                                 :opacity (or (gethash "opacity" body) 1))))))

(defun handle-adjust (req)
  "Image > Adjustments: {layer, kind: \"levels\" | \"curves\" |
\"hue-saturation\", settings: {name: value}, curves: {channel: points}}
baked into the layer's pixels; answers the rectangle (the whole layer)
as handle-stroke does."
  (let* ((doc (request-doc req))
         (body (parse-json-body req))
         (layer (the-object doc (layer (gethash "layer" body))))
         (kind (find (gethash "kind" body) '(:levels :curves :hue-saturation) :test #'string-equal))
         (settings (let ((h (gethash "settings" body)))
                     (when h (loop for k being the hash-keys of h using (hash-value v)
                                   collect (intern (string-upcase k) :keyword)
                                   collect (float v 1d0)))))
         (curves (let ((h (gethash "curves" body)))
                   (when h (loop for k being the hash-keys of h using (hash-value v)
                                 collect (intern (string-upcase k) :keyword)
                                 collect (normalize-points v))))))
    (unless kind (error "Unknown adjustment ~s." (gethash "kind" body)))
    (json-string (rect-json doc layer :layer
                            (adjust-layer! layer kind :settings settings :curves curves)))))

(defun handle-layers (req)
  "A change of the stack -- {op: \"add\", kind, above}, {op: \"delete\",
layer}, {op: \"move\", layer, delta} or {op: \"duplicate\", layer} --
answering the new planVersion and the index of the layer to work on."
  (let* ((doc (request-doc req))
         (body (parse-json-body req))
         (op (gethash "op" body))
         (layer (gethash "layer" body)))
    (flet ((need-layer ()
             (unless (and (integerp layer) (<= 0 layer) (< layer (length (the-object doc layer-list))))
               (error "No layer ~a to ~a." layer op))))
      (let ((active
              (cond ((equal op "add")
                     (let ((kind (find (gethash "kind" body) '(:raster :curves :levels :hue-saturation)
                                       :test #'string-equal)))
                       (unless kind (error "Unknown layer kind ~s." (gethash "kind" body)))
                       (the-object doc (add-layer! (the-object doc (new-layer-spec kind))
                                                   (gethash "above" body)))))
                    ((equal op "delete") (need-layer) (the-object doc (delete-layer! layer)))
                    ((equal op "move") (need-layer)
                     (the-object doc (move-layer! layer (or (gethash "delta" body) 1))))
                    ((equal op "duplicate") (need-layer) (the-object doc (duplicate-layer! layer)))
                    ((equal op "merge") (need-layer) (merge-down! doc layer))
                    ((equal op "flatten") (flatten! doc))
                    ((equal op "mask")
                     ;; {op: "mask", layer, value: "reveal" | "hide" | "delete"}: a
                     ;; white or black mask in place of any the layer had, or none.
                     ;; The undo list goes, since mask indices move with it.
                     (need-layer)
                     (let ((value (gethash "value" body)))
                       (the-object doc (layer layer)
                                   (set-mask! (cond ((equal value "reveal") '(:flat :value 255))
                                                    ((equal value "hide") '(:flat :value 0))
                                                    ((equal value "delete") nil)
                                                    (t (error "Unknown mask operation ~s." value)))))
                       (the-object doc (set-slot! :undo-entries nil))
                       layer))
                    (t (error "Unknown layer operation ~s." op)))))
        (json-string (json-object "planVersion" (the-object doc plan-version)
                                  "active" active
                                  "layerCount" (length (the-object doc layer-list))))))))

;;; Files.  The browser decodes an image file (every format it knows,
;;; exactly: an ImageBitmap with no premultiplication or color
;;; conversion, read back through a texture) and sends the raw RGBA8
;;; with its size, so the server needs no decoder; the PNG it writes
;;; (export.png) is the flattened picture.

(defun heap-free-bytes ()
  "Bytes free in the heap where the implementation can say; else nil."
  #+sbcl (- (sb-ext:dynamic-space-size) (sb-kernel:dynamic-usage))
  #-sbcl nil)

(defun body-refusal (length limit &key upload?)
  "Why a body of LENGTH bytes (nil: undeclared) cannot be taken at a
door of LIMIT bytes, in words for the person sending it; nil when it
can.  An upload also needs room in the heap, asked again after a
collection, since the measure counts garbage."
  (cond ((null length) nil)
        ((> length limit)
         (format nil "That is ~,1f MB as sent, and this server takes up to ~,1f MB."
                 (/ length 1d6) (/ limit 1d6)))
        ((and upload?
              (let ((need (* *upload-heap-factor* length)))
                (flet ((short? () (let ((free (heap-free-bytes))) (and free (< free need)))))
                  (and (short?) (progn (full-gc) (short?))))))
         "The server has no room for an image this size just now. Try a smaller one, or try again later.")))

(defparameter *upload-chunk-bytes* (* 4 1024 1024)
  "How much of an upload one read asks for.")

(defparameter *upload-stall-seconds* 60
  "How long one chunk of an upload may take before it is given up.")

(defun read-declared-body (req length)
  "LENGTH bytes of REQ's body, straight from the socket into one octet
vector.  Not get-request-body-incremental: its read asks for the whole
of its buffer whatever is left, so the last short piece waits for bytes
that never come.  A connection that ends or stalls early is an error,
never a short picture."
  (let ((out (make-array length :element-type '(unsigned-byte 8)))
        (sock (net.aserve:request-socket req))
        (used 0))
    ;; read now: the reply's keep-alive read must not read it again
    (setf (net.aserve::request-request-body req) "")
    (when (net.aserve::request-has-continue-expectation req)
      (net.aserve::send-100-continue req))
    (loop while (< used length)
          do (let ((got (glisp:with-deadline (*upload-stall-seconds*)
                          (read-sequence out sock :start used
                                                  :end (min length (+ used *upload-chunk-bytes*))))))
               (when (<= got used)
                 (error "The upload ended after ~a of ~a bytes." used length))
               (setf used got)))
    out))

(defun read-chunked-body (req limit)
  "REQ's body of undeclared length as one octet vector; nil when it
proves longer than LIMIT, the rest read and dropped so the reply can go
out."
  (let ((out (make-array (min limit 1048576) :element-type '(unsigned-byte 8)))
        (used 0)
        (over? nil))
    (net.aserve:get-request-body-incremental
     req
     (lambda (buffer count)
       (when (and buffer (plusp count) (not over?))
         (let ((needed (+ used count)))
           (cond ((> needed limit) (setf over? t))
                 (t (when (> needed (length out))
                      (let ((grown (make-array (min limit (max needed (* 2 (length out))))
                                               :element-type '(unsigned-byte 8))))
                        (replace grown out :end2 used)
                        (setf out grown)))
                    (replace out buffer :start1 used :end2 count)
                    (setf used needed))))))
     :buffer (make-array 65536 :element-type '(unsigned-byte 8)))
    (and (not over?) (subseq out 0 used))))

(defun read-request-octets (req limit)
  "The request body as one octet vector: never the string
get-request-body makes, which with its conversions costs ten times the
body at its peak.  Nil when it is longer than LIMIT."
  (let ((declared (request-content-length req)))
    (cond ((null declared) (read-chunked-body req limit))
          ((> declared limit) nil)
          (t (read-declared-body req declared)))))

(defun request-octets (req)
  "The request body as octets: the upload the door read, or read now."
  (or *upload-octets*
      (read-request-octets req *max-upload-bytes*)
      (error "The body is over ~a bytes." *max-upload-bytes*)))

;;; The page sends an image as rows of differences, each byte less the
;;; same channel of the pixel to its left, deflated by the browser's own
;;; compressor (?enc=sub3 for an opaque image at three bytes a pixel,
;;; sub4 with alpha): every pixel exact at about a fifth of the raw
;;; bytes for a photograph (5184 x 3456: 72 MB raw, 14 MB so).

(defun inflate-exactly (octets count)
  "COUNT bytes inflated from the zlib stream OCTETS, into a vector made
for them: a stream holding fewer or more is an error, so a body cannot
have the server allocate past what its stated size admits."
  (let ((out (make-array count :element-type '(unsigned-byte 8)))
        (state (chipz:make-dstate 'chipz:zlib)))
    (multiple-value-bind (consumed produced) (chipz:decompress out state octets)
      (unless (= produced count)
        (error "The image's data ended after ~a of ~a bytes." produced count))
      (let ((scratch (make-array 16 :element-type '(unsigned-byte 8))))
        (when (plusp (nth-value 1 (chipz:decompress scratch state octets :input-start consumed)))
          (error "The image's data runs past its stated size.")))
      out)))

(defun unfilter-rows (filtered w h channels)
  "RGBA8 of W x H from FILTERED, rows of CHANNELS bytes a pixel (3:
opaque) in which each byte is the difference from the same channel of
the pixel to its left."
  (let ((out (make-rgba w h)))
    (declare (type (simple-array (unsigned-byte 8) (*)) filtered out)
             (type fixnum w h channels))
    (parallel-ranges
     h
     (lambda (start end)
       (declare (type fixnum start end))
       (loop for y of-type fixnum from start below end
             do (dotimes (x w)
                  (let ((i (* channels (+ x (* y w))))
                        (o (* 4 (+ x (* y w)))))
                    (dotimes (c channels)
                      (setf (aref out (+ o c))
                            (logand 255 (+ (aref filtered (+ i c))
                                           (if (> x 0) (aref out (+ o -4 c)) 0)))))
                    (when (= channels 3) (setf (aref out (+ o 3)) 255)))))))
    out))

;;; Pixels go down packed as well, when the page asks (?enc=d on a door
;;; of pixels): one byte, the step, then a zlib stream of the bytes, each
;;; less the byte STEP before it (step 0: as they are).  Exact, like the
;;; way up.  salza2 writes fixed codes only, so a photograph comes to a
;;; third or so of its bytes; a layer that is mostly empty, or a mask, to
;;; a hundredth and less -- and a new layer is the common case.

(defparameter *pack-band-bytes* 1048576
  "Differences are taken this many bytes at a time, so packing a layer
of a photograph does not need a second layer's worth of heap.")

(defun pack-pixels (octets step)
  "OCTETS as the page takes them under ?enc=d.  No pixels are the step
alone: salza2's stream of nothing is not one every inflater reads."
  (declare (type (simple-array (unsigned-byte 8) (*)) octets) (type fixnum step))
  (let* ((n (length octets))
         (chunks (list (make-array 1 :element-type '(unsigned-byte 8) :initial-element step)))
         (total 1)
         (compressor (make-instance 'salza2:zlib-compressor
                                    :callback (lambda (buffer end)
                                                (push (subseq buffer 0 end) chunks)
                                                (incf total end)))))
    (declare (type fixnum n total))
    (when (zerop n) (return-from pack-pixels (first chunks)))
    (if (zerop step)
        (salza2:compress-octet-vector octets compressor)
        (let* ((size (min n *pack-band-bytes*))
               (band (make-array size :element-type '(unsigned-byte 8))))
          (declare (type fixnum size))
          (loop for start of-type fixnum from 0 below n by size
                for end of-type fixnum = (min n (+ start size))
                do (loop for k of-type fixnum from start below end
                         do (setf (aref band (- k start))
                                  (logand 255 (- (aref octets k)
                                                 (if (>= k step) (aref octets (- k step)) 0)))))
                   (salza2:compress-octet-vector band compressor :end (- end start)))))
    (salza2:finish-compression compressor)
    (let ((out (make-array total :element-type '(unsigned-byte 8))) (at 0))
      (declare (type fixnum at))
      (dolist (chunk (nreverse chunks) out)
        (replace out chunk :start1 at)
        (incf at (length chunk))))))

(defun ensure-room (bytes)
  "An error in words unless the heap has room for BYTES of image and
what working on it takes."
  (flet ((short? () (let ((free (heap-free-bytes)))
                      (and free (< free (* *upload-heap-factor* bytes))))))
    (when (and (short?) (progn (full-gc) (short?)))
      (error "The server has no room for an image this size just now. Try a smaller one, or try again later."))))

(defun request-jpeg (req octets)
  "(values pixels w h): OCTETS, a JPEG file, decoded and turned as its
EXIF says.  With ?hash= from the page, as the page decoded it or an
error, on which the page sends its own pixels."
  (multiple-value-bind (pixels w h)
      (multiple-value-bind (pixels w h)
          (decode-jpeg octets (lambda (w h)
                                (unless (and (< 0 w) (< 0 h) (<= (* 4 w h) *max-upload-bytes*))
                                  (error "An image of ~a x ~a is not one this server takes." w h))
                                (ensure-room (* 4 w h))))
        (oriented-rgba pixels w h (jpeg-orientation octets)))
    (let ((hash (net.aserve:request-query-value "hash" req)))
      (when (and hash (/= (parse-integer hash) (pixels-hash pixels)))
        (error "The server's decode of this JPEG is not the page's.")))
    (values pixels w h)))

(defun request-image (req)
  "(values pixels w h): the image in the body -- a PNG or JPEG file,
decoded here (read-png, request-jpeg); or with W x H from the query,
rows of differences deflated (?enc=sub3 or sub4, see above) or raw
RGBA8."
  (let ((octets (request-octets req)))
    (cond
      ((png-octets-p octets) (read-png octets))
      ((jpeg-octets-p octets) (request-jpeg req octets))
      (t
       (let ((w (parse-integer (or (net.aserve:request-query-value "w" req) "0")))
             (h (parse-integer (or (net.aserve:request-query-value "h" req) "0")))
             (enc (net.aserve:request-query-value "enc" req)))
         (unless (and (< 0 w) (< 0 h) (<= (* 4 w h) *max-upload-bytes*))
           (error "An image of ~a x ~a is not one this server takes." w h))
         (cond ((member enc '("sub3" "sub4") :test #'equal)
                (let ((channels (if (equal enc "sub3") 3 4)))
                  (ensure-room (* 4 w h))
                  (values (unfilter-rows (inflate-exactly octets (* channels w h)) w h channels) w h)))
               (t
                (unless (= (length octets) (* 4 w h))
                  (error "An image of ~a x ~a needs ~a bytes; ~a came." w h (* 4 w h) (length octets)))
                (values octets w h))))))))

(defun handle-open (req)
  "A new document from the image in the body (a PNG or JPEG file, or raw
RGBA8 with ?w=&h=; ?name=), one Background layer; answers {doc, width,
height, readMs, workMs}: how long the body took to arrive here and the
document to make, for the page's account of a slow open."
  (let ((began (get-internal-real-time)))
    (multiple-value-bind (pixels w h) (request-image req)
      (let ((name (unique-document-name (or (net.aserve:request-query-value "name" req) "untitled"))))
        (setf (gethash name *documents*) (make-file-document name w h pixels))
        (json-string (json-object "doc" name "width" w "height" h
                                  "readMs" (or *upload-read-ms* 0)
                                  "workMs" (elapsed-ms began)))))))

(defun handle-place (req)
  "The image in the body (a PNG or JPEG file, or raw RGBA8 with ?w=&h=) as a new
layer of the document (?name=&above=), centered and cropped or padded
to the document's size; answers as handle-layers does."
  (let ((doc (request-doc req)))
    (multiple-value-bind (pixels w h) (request-image req)
      (let* ((fitted (doc-pixels doc (fit-rgba pixels w h (the-object doc doc-width) (the-object doc doc-height))))
             (above (let ((a (net.aserve:request-query-value "above" req)))
                      (and a (plusp (length a)) (parse-integer a))))
             (name (or (net.aserve:request-query-value "name" req) "Placed"))
             (active (the-object doc (add-layer! (list :kind :raster :name name :pixels fitted) above))))
        (json-string (json-object "planVersion" (the-object doc plan-version)
                                  "active" active
                                  "layerCount" (length (the-object doc layer-list))))))))

(defun handle-load (req)
  "File > Open of a .stratum or an OpenRaster .ora file (told apart by
their first bytes): the body is the file; a new document under its
name (?name=); answers {doc, width, height, unsupported: [[layer,
composite-op] ...]}, the last naming OpenRaster layers whose blend
mode is not one of Stratum's and became Normal."
  (let* ((name (unique-document-name (or (net.aserve:request-query-value "name" req) "untitled")))
         (octets (request-octets req)))
    (multiple-value-bind (doc unsupported)
        (if (zip-octets-p octets)
            (read-ora-document octets name)
            (read-document-octets octets name))
      (setf (gethash name *documents*) doc)
      (json-string (json-object "doc" name
                                "width" (the-object doc doc-width)
                                "height" (the-object doc doc-height)
                                "unsupported" (coerce (mapcar (lambda (u) (coerce u 'vector)) unsupported) 'vector))))))

(defun handle-drawing-sources (req)
  "The Gendl models registered to be drawn (drawing.lisp), the standard
views and the hidden-line treatments: {sources, projections,
hiddenLines}, each a list of names."
  (declare (ignore req))
  (json-string (json-object "sources" (coerce (drawing-source-names) 'vector)
                            "projections" (coerce (mapcar #'string-downcase *drawing-projections*) 'vector)
                            "hiddenLines" (vector "draw" "remove" "dashed"))))

(defun handle-drawing (req)
  "File > Place Gendl Drawing: {source, projection, hiddenLines, color:
[r, g, b], weight, margin, above}: a new raster layer of the document,
the registered Gendl model SOURCE drawn by the drawing system in
process at the document's size; answers as layers does."
  (let* ((doc (request-doc req))
         (body (parse-json-body req))
         (source (gethash "source" body))
         (projection (let ((p (gethash "projection" body)))
                       (if p (find p *drawing-projections* :test #'string-equal) :trimetric)))
         (hidden (let ((h (gethash "hiddenLines" body)))
                   (if h (find h '(:draw :remove :dashed) :test #'string-equal) :draw)))
         (color (let ((c (gethash "color" body)))
                  (if (and (listp c) (= (length c) 3)) (mapcar #'round c) '(0 0 0))))
         (above (gethash "above" body)))
    (unless (and (stringp source) (gethash source *drawing-sources*))
      (error "No Gendl model is registered as ~s." source))
    (unless projection (error "Unknown view ~s." (gethash "projection" body)))
    (unless hidden (error "Unknown hidden-line treatment ~s." (gethash "hiddenLines" body)))
    (let ((active (place-drawing! doc source
                                  :projection projection :hidden-lines hidden
                                  :color (doc-color doc color)
                                  :weight (float (or (gethash "weight" body) 1.5) 1d0)
                                  :margin (float (or (gethash "margin" body) 0.05) 1d0)
                                  :above (and (integerp above) above))))
      (json-string (json-object "planVersion" (the-object doc plan-version)
                                "active" active
                                "layerCount" (length (the-object doc layer-list)))))))

(defun handle-new (req)
  "File > New: {name, width, height, background: \"white\" | \"transparent\"
| [r, g, b]}; a new document; answers {doc, width, height}."
  (let* ((body (parse-json-body req))
         (name (unique-document-name (or (gethash "name" body) "untitled")))
         (w (round (gethash "width" body)))
         (h (round (gethash "height" body)))
         (bg (gethash "background" body)))
    (unless (and (<= 1 w 16384) (<= 1 h 16384)) (error "Bad size ~a x ~a." w h))
    (let ((doc (new-document name w h (cond ((equal bg "transparent") :transparent)
                                            ((listp bg) (mapcar #'round bg))
                                            (t :white)))))
      (setf (gethash name *documents*) doc)
      (json-string (json-object "doc" name "width" w "height" h)))))

(defun handle-history-set (req)
  "Set the History Brush's source to the document's current state;
answers {historyVersion}."
  (let ((doc (request-doc req)))
    (json-string (json-object "historyVersion" (set-history! doc)))))

(defun handle-documents (req)
  "The names of the session's open documents, sorted; {documents,
session}."
  (let ((names '()))
    (maphash (lambda (name doc) (declare (ignore doc)) (push name names)) *documents*)
    (json-string (json-object "documents" (coerce (sort names #'string<) 'vector)
                              "session" (or (net.aserve:request-query-value "s" req) "")))))

(defun handle-close (req)
  "Forget the named document; a sample is remade on its next request."
  (let ((name (net.aserve:request-query-value "doc" req)))
    (remhash name *documents*)
    (json-string (json-object "closed" name))))

(defparameter *page-css* "
html, body { margin: 0; height: 100%; background: #1b1d21; color: #d8dbe0;
  font: 13px/1.4 system-ui, -apple-system, Segoe UI, sans-serif; }
#stratum-app { display: flex; height: calc(100vh - 27px); position: relative; }
.st-dialog { position: absolute; top: 24px; left: 60px; width: 300px; background: #2c2f35;
  border: 1px solid #3a3e45; box-shadow: 0 8px 24px rgba(0,0,0,.6); padding: 10px 14px 12px;
  z-index: 20; }
.st-dialog h2 { font-size: 13px; margin: 0 0 8px; }
.st-dialog-buttons { justify-content: flex-end; margin-top: 10px; }
.st-anchor { display: grid; grid-template-columns: repeat(3, 22px); gap: 3px; }
.st-anchor-cell { width: 22px; height: 22px; background: #3a3e45; border-radius: 2px; cursor: pointer; }
.st-anchor-cell.st-on { background: #6c9ce0; }
#stratum-menubar { height: 26px; display: flex; align-items: stretch; background: #2a2d33;
  border-bottom: 1px solid #3a3e45; user-select: none; position: relative; z-index: 5; }
.st-menu { position: relative; }
.st-menu-title { padding: 0 11px; line-height: 26px; cursor: default; }
.st-menu.st-open > .st-menu-title, .st-menu-title:hover { background: #3b5f93; }
.st-menu-list { display: none; position: absolute; top: 100%; left: 0; min-width: 230px;
  background: #2c2f35; border: 1px solid #3a3e45; box-shadow: 0 6px 18px rgba(0,0,0,.5);
  padding: 4px 0; z-index: 10; }
.st-menu.st-open > .st-menu-list { display: block; }
.st-item { display: flex; justify-content: space-between; align-items: center; gap: 24px;
  padding: 3px 14px 3px 24px; white-space: nowrap; cursor: default; position: relative; }
.st-item:hover { background: #3b5f93; }
.st-item.st-disabled { color: #6b7079; }
.st-item.st-disabled:hover { background: none; }
.st-item .st-key { color: #8a9099; font-size: 12px; }
.st-item:hover .st-key { color: #c9cdd3; }
.st-item .st-check { position: absolute; left: 8px; }
.st-sep { border-top: 1px solid #3a3e45; margin: 4px 0; }
.st-item.st-sub > .st-menu-list { top: -5px; left: 100%; }
.st-item.st-sub:hover > .st-menu-list { display: block; }
#stratum-view { flex: 1; min-width: 0; position: relative; }
#stratum-canvas { width: 100%; height: 100%; display: block; cursor: grab; touch-action: none; }
#stratum-overlay { position: absolute; left: 0; top: 0; width: 100%; height: 100%;
  pointer-events: none; }
#stratum-panel { width: 300px; flex: none; overflow-y: auto; box-sizing: border-box;
  padding: 12px 14px; border-left: 1px solid #33363c; background: #222429; }
#stratum-panel h1 { font-size: 15px; margin: 0 0 6px; letter-spacing: .02em; }
#stratum-status { font: 11px/1.5 ui-monospace, Menlo, monospace; color: #9aa0a8;
  white-space: pre-wrap; margin-bottom: 10px; }
.st-layer { border-top: 1px solid #33363c; padding: 8px 0; }
.st-head { display: flex; align-items: center; gap: 6px; }
.st-name { flex: 1; font-weight: 600; }
.st-kind { color: #8a9099; font-size: 11px; }
.st-label { color: #8a9099; font-size: 11px; width: 62px; flex: none; }
#stratum-tools { width: 40px; flex: none; background: #222429; border-right: 1px solid #33363c;
  display: flex; flex-direction: column; align-items: center; padding-top: 4px; gap: 1px;
  overflow-y: auto; user-select: none; }
.st-tool-btn { width: 30px; height: 26px; display: flex; align-items: center; justify-content: center;
  border-radius: 3px; cursor: default; color: #c9cdd3; }
.st-tool-btn:hover { background: #2c2f35; }
.st-tool-btn.st-on { background: #3b5f93; color: #fff; }
.st-tool-btn.st-disabled { color: #585d66; }
.st-tool-btn svg, .st-eye svg { width: 18px; height: 18px; fill: none; stroke: currentColor;
  stroke-width: 1.5; stroke-linecap: round; stroke-linejoin: round; }
.st-tool-gap { height: 7px; flex: none; }
.st-swatches { position: relative; width: 30px; height: 30px; margin-top: 10px; flex: none; }
.st-swatch { position: absolute; width: 17px; height: 17px; border: 1px solid #c9cdd3; cursor: pointer;
  box-sizing: border-box; }
.st-swatch.st-fg { left: 0; top: 0; z-index: 2; }
.st-swatch.st-bg { right: 0; bottom: 0; }
.st-layers-head { display: flex; justify-content: space-between; align-items: baseline;
  font-weight: 600; margin: 10px 0 4px; }
.st-lrow { display: flex; align-items: center; gap: 6px; padding: 3px 4px;
  border-top: 1px solid #2c2f35; cursor: default; }
.st-lrow.st-active { background: #3b5f93; }
.st-eye { width: 18px; height: 18px; flex: none; color: #c9cdd3; cursor: pointer; }
.st-eye.st-off { color: #585d66; }
.st-thumb { width: 40px; height: 24px; flex: none; border: 1px solid #3a3e45; box-sizing: content-box;
  background: repeating-conic-gradient(#8a8f98 0 25%, #c9cdd3 0 50%) 0 0 / 8px 8px;
  display: flex; align-items: center; justify-content: center; color: #d8dbe0; }
.st-thumb canvas { width: 40px; height: 24px; display: block; }
.st-thumb.st-target { outline: 2px solid #d8dbe0; outline-offset: 1px; }
.st-thumb.st-thumb-adj { background: #2c2f35; }
.st-thumb svg { width: 20px; height: 20px; fill: none; stroke: currentColor; stroke-width: 1.5; }
.st-lname { flex: 1; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.st-lkind { color: #8a9099; font-size: 11px; }
.st-lrow.st-active .st-lkind { color: #c9cdd3; }
.st-props { border-top: 1px solid #33363c; margin-top: 8px; padding-top: 6px; }
.st-props .st-name { font-weight: 600; }
.st-brush { margin-bottom: 10px; }
.st-brush input[type=color] { width: 36px; height: 22px; padding: 0; border: 1px solid #3a3e45;
  background: #2c2f35; }
.st-row { display: flex; align-items: center; gap: 8px; margin-top: 5px; }
.st-row input[type=range] { flex: 1; }
.st-row select { background: #2c2f35; color: inherit; border: 1px solid #3a3e45; }
.st-bench { font: 11px/1.5 ui-monospace, Menlo, monospace; color: #c9cdd3;
  margin: 4px 0 10px; white-space: pre-wrap; }
#stratum-panel button { background: #2c2f35; color: inherit; border: 1px solid #3a3e45;
  padding: 3px 8px; border-radius: 3px; cursor: pointer; }
.st-curve { display: block; margin-top: 6px; background: #17181b;
  border: 1px solid #3a3e45; cursor: crosshair; touch-action: none; }
#stratum-prompt { border-top: 1px solid #33363c; margin-top: 10px; padding-top: 6px; }
.st-prompt-box { width: 100%; box-sizing: border-box; background: #17181b; color: inherit;
  border: 1px solid #3a3e45; border-radius: 3px; padding: 4px 6px; font: inherit; resize: vertical; }
.st-prompt-log { font-size: 12px; margin-top: 6px; max-height: 40vh; overflow-y: auto; }
.st-prompt-turn { border-top: 1px solid #2c2f35; padding: 4px 0; }
.st-prompt-you { color: #8a9099; }
.st-prompt-reply { white-space: pre-line; }
.st-prompt-reply.st-declined { color: #e0b070; }
.st-prompt-forms { font: 11px/1.4 ui-monospace, Menlo, monospace; color: #c9cdd3;
  white-space: pre-wrap; margin: 3px 0 0; }
.st-prompt-forms .st-failed { color: #e07a7a; }
.st-prompt-busy { color: #8a9099; font-style: italic; }
.st-busy { position: fixed; top: 44px; left: 50%; transform: translateX(-50%); z-index: 50;
  display: flex; align-items: center; gap: 8px; padding: 6px 12px; border-radius: 6px;
  background: #2a2f36; color: #e8eaed; border: 1px solid #4a5058; font-size: 13px;
  box-shadow: 0 2px 8px rgba(0, 0, 0, .4); }
.st-spinner { width: 12px; height: 12px; border-radius: 50%; border: 2px solid #6a7078;
  border-top-color: #e8eaed; animation: st-spin .8s linear infinite; }
@keyframes st-spin { to { transform: rotate(360deg); } }
body.st-loading #stratum-menubar, body.st-loading #stratum-tools, body.st-loading #stratum-view,
body.st-loading #stratum-layers, body.st-loading #stratum-prompt { pointer-events: none; }
body.st-loading #stratum-menubar, body.st-loading #stratum-tools,
body.st-loading #stratum-layers, body.st-loading #stratum-prompt { opacity: .5; }
@media (max-width: 700px) {
  #stratum-app { flex-direction: column; }
  #stratum-view { height: 60vh; flex: none; width: 100%; }
  #stratum-panel { width: 100%; border-left: none; border-top: 1px solid #33363c; }
}
")

(define-object stratum-page (base-html-page)
  :input-slots
  ((title "Stratum")
   (use-x3dom? nil)
   (use-svgpanzoom? nil)
   (use-tailwind? nil)
   (use-ajax? nil)
   (additional-header-content
    (string-append (call-next-method)
                   (with-lhtml-string () (:style (str *page-css*))))))

  :computed-slots
  ((body (with-lhtml-string ()
           (:div :id "stratum-menubar" "")
           (:div :id "stratum-app"
             (:div :id "stratum-tools" "")
             (:div :id "stratum-view"
               (:canvas :id "stratum-canvas" "")
               (:canvas :id "stratum-overlay" ""))
             (:aside :id "stratum-panel"
               (:h1 "Stratum")
               (:div :id "stratum-status" "Loading...")
               (:div :id "stratum-layers" "")
               (:div :id "stratum-prompt" "")))
           ;; a version in each address: a cache between here and the
           ;; browser keeps a script by its address, and would hand a new
           ;; server's page the old one
           (:script :src (format nil "/stratum/dual.js?v=~a" (sxhash (dual-functions-js))) "")
           (:script :src (format nil "/stratum/viewer.js?v=~a" (viewer-version)) "")))))

(defun viewer-version ()
  (file-write-date (merge-pathnames "web/viewer.js" *stratum-dir*)))

(defvar *page* nil
  "(version . page): the page made for the viewer.js then on disk.")

(defun page-html ()
  (let ((version (viewer-version)))
    (unless (eql version (car *page*))
      (setf *page* (cons version (make-object 'stratum-page))))
    (the-object (cdr *page*) main-sheet)))

(defparameter *response-payload-too-large* (net.aserve::make-resp 413 "Payload Too Large"))

(defparameter *response-unsupported-media-type* (net.aserve::make-resp 415 "Unsupported Media Type"))

(defun reply-text (req ent response text &key (drain *drained-body-bytes*))
  "Answer TEXT with RESPONSE and close the connection, so a body the
door could not read is not read now, outside any handler.  A body
within DRAIN bytes is dropped first, in pieces, so the words arrive."
  (drain-body! req drain)
  (net.aserve:with-http-response (req ent :content-type "text/plain" :response response)
    (close-after-reply! req)
    (net.aserve:with-http-body (req ent)
      (write-string text (net.aserve:request-reply-stream req)))))

(defun form-body? (req)
  "True for a form-encoded body, which asking for any query value
reads whole, as a string.  No door here takes one."
  (and (member (net.aserve:request-method req) '(:post :put :patch))
       (search "application/x-www-form-urlencoded"
               (or (net.aserve:header-slot-value req :content-type) "")
               :test #'char-equal)))

(defun door-admits? (req ent limit &key upload?)
  "True when REQ may come in: a live invite, and a body within LIMIT
that an upload has room for.  Otherwise REQ is answered here, with its
body unread, and the answer is nil.  A form body is turned away before
the gate is asked, since the gate looks at the query: that holds in an
image whose compiled invite-gate reads it."
  (cond ((form-body? req)
         (reply-text req ent *response-unsupported-media-type* "This door takes no form posts."
                     :drain *refused-body-bytes*)
         nil)
        ((not (invite-allowed? req)) (refuse req ent) nil)
        (t (ensure-invite-cookie! req)
           (let ((refusal (body-refusal (request-content-length req) limit :upload? upload?)))
             (when refusal (reply-text req ent *response-payload-too-large* refusal))
             (not refusal)))))

(defun no-store! (req)
  "Tell every cache in the way to keep nothing of this reply.  What the
doors answer is a visitor's picture and changes with each edit, and a
proxy that keeps an address ending in .png serves it again from then
on: the document as it was, and past the invite gate, to whoever has
the address."
  (setf (net.aserve:reply-header-slot-value req :cache-control) "no-store"))

(defun reply (req ent content-type thunk &key pack)
  "Run THUNK under the lock, then send its string or octets; a failure
goes back as a 500 carrying the condition's text.  With PACK, a step,
the octets are pixels and go packed to a page that asks (?enc=d, see
pack-pixels): outside the lock, a second or so for a photograph's
layer."
  (let ((result nil) (failure nil))
    (bt:with-lock-held (*lock*)
      ;; serious-condition, not error: a heap exhausted inside a handler
      ;; is a storage-condition, and a worker that stopped in the
      ;; debugger on one would hold the lock against every request
      (handler-case (setf result (funcall thunk))
        (serious-condition (e) (setf failure (princ-to-string e)))))
    (when (and pack (not failure)
               (equal (net.aserve:request-query-value "enc" req :post nil) "d"))
      (handler-case (setf result (pack-pixels result pack))
        (serious-condition (e) (setf failure (princ-to-string e)))))
    (if failure
        (reply-text req ent net.aserve:*response-internal-server-error* failure)
        (net.aserve:with-http-response (req ent :content-type content-type)
          (no-store! req)
          (net.aserve:with-http-body (req ent)
            (let ((s (net.aserve:request-reply-stream req)))
              (if (stringp result)
                  (write-string result s)
                  (write-sequence result s))))))))

(defun serve-door (req ent content-type fn &key pack)
  (reply req ent content-type
         (lambda ()
           (let* ((*session* (request-session req))
                  (*documents* (sess-table *session*)))
             (funcall fn req)))
         :pack pack))

(defun serve-upload (req ent content-type fn)
  "Read the body, outside the lock since a slow link can take minutes
over it, then serve the door with it in *upload-octets*."
  (let ((octets nil) (failure nil) (began (get-internal-real-time)))
    (handler-case (setf octets (read-request-octets req *max-upload-bytes*))
      (serious-condition (e) (setf failure (princ-to-string e))))
    (cond (failure (reply-text req ent net.aserve:*response-internal-server-error* failure))
          ((null octets)
           (reply-text req ent *response-payload-too-large*
                       (body-refusal (1+ *max-upload-bytes*) *max-upload-bytes*)))
          (t (let ((*upload-octets* octets)
                   (*upload-read-ms* (elapsed-ms began)))
               (serve-door req ent content-type fn))))))

(defun publish-door (server host path content-type fn &key upload? pack)
  "Publish FN, a function of the request answering a string or octets,
at PATH: behind the invite gate, under the lock, the session's
documents bound.  With UPLOAD? the door takes a large binary body; with
PACK, a step, it answers pixels (see reply)."
  (net.aserve:publish :path path :server server :host host
                      :content-type content-type
                      :function (lambda (req ent)
                                  (when (door-admits? req ent
                                                      (if upload? *max-upload-bytes* *max-request-bytes*)
                                                      :upload? upload?)
                                    (if upload?
                                        (serve-upload req ent content-type fn)
                                        (serve-door req ent content-type fn :pack pack))))))

(defvar *stratum-publishers* nil
  "Functions of (server host) that publish doors of their own when
publish-stratum! runs, for files loaded after this one (prompt.lisp).
Not gwl:*publishers*, which the stratum package inherits: those are
the image's own, called with one argument at its start.")

(defun publish-prefix-doors (server host)
  "Two doors that read no body.  The invite link answers GET alone.
Everything else under /stratum, which the proxy sends here whether or
not it is published, gets a 404 from here: AllegroServe's own reads
the body of a POST first, as one string, which a large POST turns into
a worker lost for good."
  (let ((invite-prefix "/stratum/invite/"))
    (net.aserve:publish-prefix
     :prefix invite-prefix :server server :host host
     :function (lambda (req ent)
                 (if (eq (net.aserve:request-method req) :get)
                     (invite-gate:invite-door *gate* req ent
                                              (subseq (net.uri:uri-path (net.aserve:request-uri req))
                                                      (length invite-prefix)))
                     (refuse req ent)))))
  (net.aserve:publish-prefix
   :prefix "/stratum" :server server :host host
   :function (lambda (req ent)
               (reply-text req ent net.aserve:*response-not-found* "Not found."
                           :drain *refused-body-bytes*))))

(defun publish-stratum! (&key host)
  "Publish the page at /stratum/ and its API under /stratum/api/."
  (gwl:with-all-servers (server)
    (flet ((pub (path content-type fn &key upload? pack)
             (publish-door server host path content-type fn :upload? upload? :pack pack)))
      ;; the invite link of the private beta (invite.lisp): no gate
      ;; without an invites file
      (publish-prefix-doors server host)
      (pub "/stratum" "text/html" (lambda (req) (declare (ignore req)) (page-html)))
      (pub "/stratum/" "text/html" (lambda (req) (declare (ignore req)) (page-html)))
      (pub "/stratum/viewer.js" "application/javascript"
           (lambda (req) (declare (ignore req))
             (uiop:read-file-string (merge-pathnames "web/viewer.js" *stratum-dir*))))
      (pub "/stratum/dual.js" "application/javascript"
           (lambda (req) (declare (ignore req)) (dual-functions-js)))
      (pub "/stratum/api/doc" "application/json"
           (lambda (req) (json-string (document-json (request-doc req)))))
      (pub "/stratum/api/raster" "application/octet-stream"
           (lambda (req)
             (let* ((doc (request-doc req))
                    (pixels (the-object (nth (request-index req) (the-object doc raster-layers))
                                        pixels)))
               (flet ((q (name) (let ((v (net.aserve:request-query-value name req)))
                                  (and v (parse-integer v)))))
                 (if (q "w")
                     (rgba-rect pixels (the-object doc doc-width) (the-object doc doc-height)
                                (or (q "x") 0) (or (q "y") 0) (q "w") (q "h"))
                     pixels))))
           :pack 4)
      (pub "/stratum/api/thumb" "application/octet-stream"
           (lambda (req)
             (let* ((doc (request-doc req))
                    (i (request-index req)))
               (flet ((q (name) (parse-integer (net.aserve:request-query-value name req))))
                 (if (net.aserve:request-query-value "mask" req)
                     (rgba-thumb (the-object (nth i (the-object doc masked-layers)) mask-pixels)
                                 (the-object doc doc-width) (the-object doc doc-height) (q "w") (q "h") 1)
                     (rgba-thumb (the-object (nth i (the-object doc raster-layers)) pixels)
                                 (the-object doc doc-width) (the-object doc doc-height) (q "w") (q "h") 4))))))
      (pub "/stratum/api/mask" "application/octet-stream"
           (lambda (req)
             (let* ((doc (request-doc req))
                    (mask (the-object (nth (request-index req) (the-object doc masked-layers))
                                      mask-pixels)))
               (flet ((q (name) (let ((v (net.aserve:request-query-value name req)))
                                  (and v (parse-integer v)))))
                 (if (q "w")
                     (rgba-rect mask (the-object doc doc-width) (the-object doc doc-height)
                                (or (q "x") 0) (or (q "y") 0) (q "w") (q "h") 1)
                     mask))))
           ;; as they are: differences make a hard-edged mask larger
           :pack 0)
      (pub "/stratum/api/luts" "application/json"
           (lambda (req) (json-string (coerce (the-object (request-doc req) lut-vector) 'list))))
      (pub "/stratum/api/cpu" "application/octet-stream"
           (lambda (req) (the-object (request-doc req) (render-cpu))))
      (pub "/stratum/api/export.png" "image/png"
           (lambda (req)
             (let ((doc (request-doc req)))
               (png-octets (the-object doc (render-cpu))
                           (the-object doc doc-width) (the-object doc doc-height)))))
      (pub "/stratum/api/set" "application/json" #'handle-set)
      (pub "/stratum/api/stroke" "application/json" #'handle-stroke)
      (pub "/stratum/api/undo" "application/json" #'handle-undo)
      (pub "/stratum/api/redo" "application/json" #'handle-redo)
      (pub "/stratum/api/layers" "application/json" #'handle-layers)
      (pub "/stratum/api/adjust" "application/json" #'handle-adjust)
      (pub "/stratum/api/filter" "application/json" #'handle-filter)
      (pub "/stratum/api/gradient" "application/json" #'handle-gradient)
      (pub "/stratum/api/mode" "application/json" #'handle-mode)
      (pub "/stratum/api/documents" "application/json" #'handle-documents)
      (pub "/stratum/api/history" "application/octet-stream"
           (lambda (req) (history-pixels (request-doc req)))
           :pack 4)
      (pub "/stratum/api/history-set" "application/json" #'handle-history-set)
      (pub "/stratum/api/select" "application/json" #'handle-select)
      (pub "/stratum/api/selection" "application/octet-stream"
           (lambda (req)
             (let ((selection (the-object (request-doc req) selection)))
               (if selection (getf selection :mask) (make-array 0 :element-type '(unsigned-byte 8)))))
           :pack 0)
      (pub "/stratum/api/fill" "application/json" #'handle-fill)
      (pub "/stratum/api/move" "application/json" #'handle-move)
      (pub "/stratum/api/transform" "application/json" #'handle-transform)
      (pub "/stratum/api/clipboard" "application/json" #'handle-clipboard)
      (pub "/stratum/api/resize" "application/json" #'handle-resize)
      (pub "/stratum/api/open" "application/json" #'handle-open :upload? t)
      (pub "/stratum/api/place" "application/json" #'handle-place :upload? t)
      (pub "/stratum/api/drawing-sources" "application/json" #'handle-drawing-sources)
      (pub "/stratum/api/drawing" "application/json" #'handle-drawing)
      (pub "/stratum/api/close" "application/json" #'handle-close)
      (pub "/stratum/api/new" "application/json" #'handle-new)
      (pub "/stratum/api/save" "application/octet-stream"
           (lambda (req) (document-octets (request-doc req))))
      (pub "/stratum/api/load" "application/json" #'handle-load :upload? t)
      (pub "/stratum/api/save.ora" "image/openraster"
           (lambda (req) (document-ora-octets (request-doc req))))
      (dolist (publisher *stratum-publishers*) (funcall publisher server host)))))
