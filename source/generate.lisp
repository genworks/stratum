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
;; document's size.  The generator is a separate service speaking the
;; WebUI API (POST /sdapi/v1/img2img and txt2img, JSON, base64 PNGs),
;; which stable-diffusion.cpp's sd-server and others serve; (configure-
;; generator! :url ...) says where.  There is none by default.
;;
;; A generator works at a megapixel or so and a photograph has many, so
;; generate-fill sends a crop: the selection and some of what surrounds
;; it, scaled to the generator's size, with the selection as its mask.
;; What comes back is scaled to the crop and lands as a new raster layer
;; whose alpha is the selection's coverage.  So a generator that strays
;; outside its mask changes nothing there, nothing below the layer is
;; touched, and the layer takes masks, blends and undo like any other.
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
  "GRAY, W x H bytes, as a PNG, inverted with INVERT?."
  (let ((rgba (make-rgba w h)))
    (dotimes (i (* w h))
      (let ((v (if invert? (- 255 (aref gray i)) (aref gray i)))
            (o (* 4 i)))
        (setf (aref rgba o) v (aref rgba (+ o 1)) v (aref rgba (+ o 2)) v (aref rgba (+ o 3)) 255)))
    (png-octets rgba w h)))

(defun data-url (png)
  (format nil "data:image/png;base64,~a" (sexp-agent:base64-encode png)))

(defun generator-body (generator prompt seed w h &optional image mask)
  "The request for a picture of W x H, as JSON text; with IMAGE and MASK,
PNG octets, a repainting of them."
  (let ((body (make-hash-table :test 'equal)))
    (flet ((put (key value) (setf (gethash key body) value)))
      (put "prompt" prompt)
      (put "negative_prompt" (generator-negative generator))
      (put "width" w)
      (put "height" h)
      (put "steps" (generator-steps generator))
      (put "cfg_scale" (generator-cfg-scale generator))
      (put "seed" seed)
      (put "batch_size" 1)
      (when image
        (put "init_images" (list (data-url image)))
        (put "mask" (data-url mask))
        (put "inpainting_mask_invert" 0)
        (put "denoising_strength" (float (generator-strength generator) 1d0))))
    (yason:with-output-to-string* () (yason:encode body))))

(defun ask-generator (generator path body)
  "BODY posted to the generator at PATH: the picture it answers, (values
pixels w h).  Errors say what a person can act on."
  (multiple-value-bind (status text)
      (handler-case (sexp-agent:post-json (format nil "~a~a" (generator-url generator) path) body
                                          :seconds (generator-seconds generator))
        (error (e)
          (error "The image generator could not be reached: ~a" e)))
    (unless (eql status 200)
      (error "The image generator answered ~a: ~a" status (subseq text 0 (min 200 (length text)))))
    (let* ((reply (handler-case (yason:parse text)
                    (error () (error "The image generator's answer was not JSON."))))
           (image (and (hash-table-p reply) (first (gethash "images" reply)))))
      (unless (stringp image)
        (error "The image generator answered with no image."))
      (read-png (cl-base64:base64-string-to-usb8-array
                 (subseq image (let ((comma (position #\, image))) (if comma (1+ comma) 0))))))))

(defun require-generator ()
  (or *generator* (error "This server has no image generator.")))

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
    (multiple-value-bind (pixels w h) (ask-generator generator "/sdapi/v1/img2img" body)
      (bt:with-lock-held (*lock*)
        (stratum-api:layer-info (generated-layer! doc pixels w h rect coverage name))))))

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
          (ask-generator generator "/sdapi/v1/txt2img" (generator-body generator prompt seed sw sh))
        (bt:with-lock-held (*lock*)
          (stratum-api:layer-info (generated-layer! doc pixels w h (list 0 0 dw dh) nil name)))))))

(defparameter *long-operators* '(stratum-api:generate-fill stratum-api:generate-layer)
  "The operations run-forms calls without the lock, which they take for
themselves: a generation would hold every other request up for a
minute.")
