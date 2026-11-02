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

(defvar *documents* (make-hash-table :test 'equal))

(defvar *lock* (bt:make-lock "stratum")
  "Held around every request: one GDL tree, many server threads.")

(defun find-document (name)
  (or (gethash name *documents*)
      (setf (gethash name *documents*) (make-sample-document name))))

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
  (let ((raster? (eql (the-object layer kind) :raster)))
    (json-object
     "index" (the-object layer index)
     "name" (the-object layer layer-name)
     "kind" (string-downcase (the-object layer kind))
     "opacity" (the-object layer opacity)
     "visible" (json-bool (the-object layer visible?))
     "mask" (json-bool (the-object layer has-mask?))
     "maskIndex" (the-object layer mask-index)
     "blend" (when raster? (string-downcase (the-object layer blend-mode)))
     "texIndex" (when raster? (the-object layer texture-index))
     "lutRow" (unless raster? (the-object layer lut-row))
     "curves" (unless raster?
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
   "shaderBuilds" (aref (the-object doc build-counts) 0)
   "kernelBuilds" (aref (the-object doc build-counts) 1)
   "rasterCount" (length (the-object doc raster-layers))
   "maskCount" (length (the-object doc masked-layers))
   "lutRows" (the-object doc lut-rows)
   "backgroundLinear" (mapcar (lambda (c) (float c 1d0)) (the-object doc background-linear))
   "layers" (mapcar #'layer-json (the-object doc layer-list))))

(defun handle-set (req)
  "Apply one edit, {layer, slot, value} or {layer, slot: \"curve\",
channel, points}, and answer the document's planVersion."
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
          ((string= slot "curve")
           (let ((channel (find (gethash "channel" body) '(:master :red :green :blue)
                                :test #'string-equal)))
             (unless channel (error "Unknown channel ~s." (gethash "channel" body)))
             (the-object layer (set-slot! channel (normalize-points (gethash "points" body))))))
          (t (error "Unknown slot ~s." slot)))
    (json-string (json-object "planVersion" (the-object doc plan-version)))))

(defparameter *page-css* "
html, body { margin: 0; height: 100%; background: #1b1d21; color: #d8dbe0;
  font: 13px/1.4 system-ui, -apple-system, Segoe UI, sans-serif; }
#stratum-app { display: flex; height: 100vh; }
#stratum-canvas { flex: 1; min-width: 0; height: 100%; display: block;
  cursor: grab; touch-action: none; }
#stratum-panel { width: 300px; flex: none; overflow-y: auto; box-sizing: border-box;
  padding: 12px 14px; border-left: 1px solid #33363c; background: #222429; }
#stratum-panel h1 { font-size: 15px; margin: 0 0 6px; letter-spacing: .02em; }
#stratum-status { font: 11px/1.5 ui-monospace, Menlo, monospace; color: #9aa0a8;
  white-space: pre-wrap; margin-bottom: 10px; }
.st-layer { border-top: 1px solid #33363c; padding: 8px 0; }
.st-head { display: flex; align-items: center; gap: 6px; }
.st-name { flex: 1; font-weight: 600; }
.st-kind { color: #8a9099; font-size: 11px; }
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
  #stratum-canvas { height: 60vh; flex: none; width: 100%; }
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
           (:div :id "stratum-app"
             (:canvas :id "stratum-canvas" "")
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
      (handler-case (setf result (funcall thunk))
        (error (e) (setf failure (princ-to-string e)))))
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
                                                    (lambda () (funcall fn req)))))))
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
             (the-object (nth (request-index req) (the-object (request-doc req) raster-layers))
                         pixels)))
      (pub "/stratum/api/mask" "application/octet-stream"
           (lambda (req)
             (the-object (nth (request-index req) (the-object (request-doc req) masked-layers))
                         mask-pixels)))
      (pub "/stratum/api/luts" "application/json"
           (lambda (req) (json-string (coerce (the-object (request-doc req) lut-vector) 'list))))
      (pub "/stratum/api/cpu" "application/octet-stream"
           (lambda (req) (the-object (request-doc req) (render-cpu))))
      (pub "/stratum/api/export.png" "image/png"
           (lambda (req)
             (let ((doc (request-doc req)))
               (png-octets (the-object doc (render-cpu))
                           (the-object doc doc-width) (the-object doc doc-height)))))
      (pub "/stratum/api/set" "application/json" #'handle-set))))
