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

(defvar *documents* (make-hash-table :test 'equal)
  "The documents of the session being served, by name.  Bound around
every request to its session's table (request-session-table); the
global value is the shared pool, which a request without a session
(?s=) and Lisp callers use.")

(defvar *sessions* (make-hash-table :test 'equal)
  "Session id -> (documents-table . last-touched universal time).  A
browser keeps its id in its own storage and sends it as ?s= on every
call, so two browsers opening \"demo\" get two documents.")

(defparameter *session-idle-seconds* (* 4 3600)
  "A session untouched this long is forgotten, documents and all.")

(defvar *lock* (bt:make-lock "stratum")
  "Held around every request: one GDL tree, many server threads.")

(defun sweep-sessions! ()
  "Forget every session idle longer than *session-idle-seconds*."
  (let ((now (get-universal-time)) (gone '()))
    (maphash (lambda (id cell) (when (> (- now (cdr cell)) *session-idle-seconds*) (push id gone))) *sessions*)
    (dolist (id gone) (remhash id *sessions*))
    gone))

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

(defun relieve-memory! (keep-id)
  "When the heap is above *heap-high-water*: drop every document's undo
and redo lists, collect, and while still above it forget the sessions
touched longest ago, except KEEP-ID's.  Returns what it did, or nil."
  (let ((before (heap-used-fraction)))
    (when (and before (> before *heap-high-water*))
      (let ((done '()))
        (flet ((drop-history (table)
                 (maphash (lambda (name doc)
                            (declare (ignore name))
                            (the-object doc (set-slot! :undo-entries nil))
                            (the-object doc (set-slot! :redo-entries nil)))
                          table)))
          (drop-history *documents*)
          (maphash (lambda (id cell) (declare (ignore id)) (drop-history (car cell))) *sessions*)
          (push :undo-dropped done)
          (full-gc)
          (loop while (and (> (heap-used-fraction) *heap-high-water*)
                           (> (hash-table-count *sessions*) 0))
                do (let ((oldest nil) (when-touched nil))
                     (maphash (lambda (id cell)
                                (unless (equal id keep-id)
                                  (when (or (null when-touched) (< (cdr cell) when-touched))
                                    (setf oldest id when-touched (cdr cell)))))
                              *sessions*)
                     (unless oldest (return))
                     (remhash oldest *sessions*)
                     (push (list :session-forgotten oldest) done)
                     (full-gc))))
        (nreverse done)))))

(defun session-table (id)
  "The documents table of session ID, made on first sight and touched
now; the shared pool for no id.  A touch also sweeps idle sessions and,
when the heap is short, relieves it (relieve-memory!)."
  (if (or (null id) (string= id ""))
      *documents*
      (let ((cell (or (gethash id *sessions*)
                      (setf (gethash id *sessions*) (cons (make-hash-table :test 'equal) 0)))))
        (setf (cdr cell) (get-universal-time))
        (sweep-sessions!)
        (relieve-memory! id)
        (car cell))))

(defun request-session-table (req)
  (session-table (net.aserve:request-query-value "s" req)))

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
mode change, {mode, group: [one rectangle per layer]}."
  (if (eql (first entry) :mode)
      (json-object "mode" (string-downcase (the-object doc color-mode))
                   "group" (mapcar (lambda (e) (rect-json doc (the-object doc (layer (first e))) (second e) (cddr e)))
                                   (cdddr entry))
                   "undoDepth" (length (the-object doc undo-entries))
                   "redoDepth" (length (the-object doc redo-entries)))
      (rect-json doc (the-object doc (layer (first entry))) (second entry) (cddr entry))))

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
                 (cond ((equal m "add") :add) ((equal m "subtract") :subtract) (t :new)))))
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
            (json-object "clipboard" (json-object "w" (getf *clipboard* :w) "h" (getf *clipboard* :h))))
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

(defun request-octets (req)
  "The request body as octets.  aserve hands a binary body over as a
string of one character per byte."
  (let ((body (net.aserve:get-request-body req :external-format :octets)))
    (if (stringp body)
        (map '(simple-array (unsigned-byte 8) (*)) #'char-code body)
        (coerce body '(simple-array (unsigned-byte 8) (*))))))

(defun request-image (req)
  "(values pixels w h): the image in the body -- a PNG file, decoded
here (read-png), or raw RGBA8 with W x H from the query."
  (let ((octets (request-octets req)))
    (if (png-octets-p octets)
        (read-png octets)
        (let ((w (parse-integer (or (net.aserve:request-query-value "w" req) "0")))
              (h (parse-integer (or (net.aserve:request-query-value "h" req) "0"))))
          (unless (and (< 0 w) (< 0 h) (= (length octets) (* 4 w h)))
            (error "An image of ~a x ~a needs ~a bytes; ~a came." w h (* 4 w h) (length octets)))
          (values octets w h)))))

(defun unique-document-name (name)
  "NAME, or NAME-2, NAME-3 ... when a document or a sample already has it."
  (loop for n from 1
        for candidate = (if (= n 1) name (format nil "~a-~d" name n))
        unless (or (gethash candidate *documents*)
                   (assoc candidate *recipes* :test #'string=))
          return candidate))

(defun handle-open (req)
  "A new document from the image in the body (a PNG file, or raw RGBA8
with ?w=&h=; ?name=), one Background layer; answers {doc, width,
height}."
  (multiple-value-bind (pixels w h) (request-image req)
    (let ((name (unique-document-name (or (net.aserve:request-query-value "name" req) "untitled"))))
      (setf (gethash name *documents*) (make-file-document name w h pixels))
      (json-string (json-object "doc" name "width" w "height" h)))))

(defun handle-place (req)
  "The image in the body (a PNG file, or raw RGBA8 with ?w=&h=) as a new
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
  "File > Open of a .stratum file: the body is the file; a new document
under its name (?name=); answers {doc, width, height}."
  (let* ((name (unique-document-name (or (net.aserve:request-query-value "name" req) "untitled")))
         (doc (read-document-octets (request-octets req) name)))
    (setf (gethash name *documents*) doc)
    (json-string (json-object "doc" name
                              "width" (the-object doc doc-width)
                              "height" (the-object doc doc-height)))))

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
               (:div :id "stratum-layers" "")))
           (:script :src "/stratum/dual.js" "")
           (:script :src "/stratum/viewer.js" "")))))

(defvar *page* nil)

(defun page-html ()
  (the-object (or *page* (setf *page* (make-object 'stratum-page))) main-sheet))

(defun reply (req ent content-type thunk)
  "Run THUNK under the lock, then send its string or octets; a failure
goes back as a 500 carrying the condition's text."
  (let ((result nil) (failure nil))
    (bt:with-lock-held (*lock*)
      ;; serious-condition, not error: a heap exhausted inside a handler
      ;; is a storage-condition, and a worker that stopped in the
      ;; debugger on one would hold the lock against every request
      (handler-case (setf result (funcall thunk))
        (serious-condition (e) (setf failure (princ-to-string e)))))
    (if failure
        (net.aserve:with-http-response (req ent :content-type "text/plain"
                                                :response net.aserve:*response-internal-server-error*)
          (net.aserve:with-http-body (req ent)
            (write-string failure (net.aserve:request-reply-stream req))))
        (net.aserve:with-http-response (req ent :content-type content-type)
          (net.aserve:with-http-body (req ent)
            (let ((s (net.aserve:request-reply-stream req)))
              (if (stringp result)
                  (write-string result s)
                  (write-sequence result s))))))))

(defun publish-stratum! (&key host)
  "Publish the page at /stratum/ and its API under /stratum/api/."
  (gwl:with-all-servers (server)
    (flet ((pub (path content-type fn)
             (net.aserve:publish :path path :server server :host host
                                 :content-type content-type
                                 :function (lambda (req ent)
                                             (reply req ent content-type
                                                    (lambda ()
                                                      (let ((*documents* (request-session-table req)))
                                                        (funcall fn req))))))))
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
                     pixels)))))
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
                     mask)))))
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
           (lambda (req) (history-pixels (request-doc req))))
      (pub "/stratum/api/history-set" "application/json" #'handle-history-set)
      (pub "/stratum/api/select" "application/json" #'handle-select)
      (pub "/stratum/api/selection" "application/octet-stream"
           (lambda (req)
             (let ((selection (the-object (request-doc req) selection)))
               (if selection (getf selection :mask) (make-array 0 :element-type '(unsigned-byte 8))))))
      (pub "/stratum/api/fill" "application/json" #'handle-fill)
      (pub "/stratum/api/move" "application/json" #'handle-move)
      (pub "/stratum/api/transform" "application/json" #'handle-transform)
      (pub "/stratum/api/clipboard" "application/json" #'handle-clipboard)
      (pub "/stratum/api/resize" "application/json" #'handle-resize)
      (pub "/stratum/api/open" "application/json" #'handle-open)
      (pub "/stratum/api/place" "application/json" #'handle-place)
      (pub "/stratum/api/close" "application/json" #'handle-close)
      (pub "/stratum/api/new" "application/json" #'handle-new)
      (pub "/stratum/api/save" "application/octet-stream"
           (lambda (req) (document-octets (request-doc req))))
      (pub "/stratum/api/load" "application/json" #'handle-load))))
