;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :stratum)

;;
;; ONNX Runtime, through its C API, for the models Stratum runs in
;; process (select-subject's).  The C API is one table of function
;; pointers, handed out by OrtGetApiBase()->GetApi(version); the library
;; only ever adds to the end of it, so each function's place in the
;; table is fixed for good and is written down here (taken from the
;; header of 1.31.1 by offsetof, and the same in every version since the
;; table began).  With no library, or no model files, the operations that
;; need them say so in words and nothing else changes.
;;

(defparameter *onnxruntime-file*
  (or (uiop:getenv "ONNXRUNTIME_LIBRARY")
      (let ((there "/projects/.models/onnxruntime/libonnxruntime.so.1"))
        (and (probe-file there) there))
      "libonnxruntime.so.1")
  "The ONNX Runtime shared library.")

(defparameter *onnx-api-version* 16
  "The version of the function table asked for: any library from 1.16 on
hands it out, and every function used here is in it.")

(defparameter *onnx-slots*
  '((:get-error-message . 2) (:create-env . 3) (:create-session . 7) (:run . 9)
    (:create-session-options . 10) (:set-intra-op-threads . 24) (:set-inter-op-threads . 25)
    (:create-tensor . 49) (:tensor-data . 51) (:dimensions-count . 61) (:dimensions . 62)
    (:element-count . 64) (:type-and-shape . 65) (:create-cpu-memory-info . 69)
    (:release-env . 92) (:release-status . 93) (:release-memory-info . 94)
    (:release-session . 95) (:release-value . 96) (:release-type-and-shape . 99)
    (:release-session-options . 100))
  "Each function's place in the C API's table.")

(defconstant +ort-logging-level-error+ 3)
(defconstant +ort-arena-allocator+ 1)
(defconstant +ort-mem-type-default+ 0)
(defconstant +onnx-float+ 1 "ONNX's element type for 32-bit floats.")

(defvar *onnx* :unknown
  "The library's function table once loaded, nil when it cannot be, or
:unknown before the first asking.")

(defvar *onnx-env* nil "The runtime's one environment.")
(defvar *onnx-memory* nil "A description of plain CPU memory, for tensors made here.")

(defvar *onnx-lock* (bt:make-lock "stratum onnx")
  "Held to load the library and the models, which two sessions may ask
for at once.")

(defmacro with-foreign-floats-safe (&body body)
  "BODY with the floating-point traps that Lisp keeps and C code does not
expect turned off: a library's vector code makes infinities and
denormals on the way to ordinary answers."
  #+sbcl `(sb-int:with-float-traps-masked (:invalid :divide-by-zero :overflow :underflow :inexact)
            ,@body)
  #-sbcl `(progn ,@body))

(defun onnx-function (name)
  (cffi:mem-aref *onnx* :pointer (or (cdr (assoc name *onnx-slots*))
                                     (error "No ONNX Runtime function ~s." name))))

(defmacro onnx-call (name &rest arguments-and-return)
  "A call of the C API's function NAME: arguments as for
cffi:foreign-funcall-pointer, the return type last."
  `(cffi:foreign-funcall-pointer (onnx-function ,name) () ,@arguments-and-return))

(defun onnx-check (status)
  "Nothing when STATUS is null; otherwise its message as an error."
  (unless (cffi:null-pointer-p status)
    (let ((message (onnx-call :get-error-message :pointer status :string)))
      (onnx-call :release-status :pointer status :void)
      (error "ONNX Runtime: ~a" message))))

(defmacro onnx-out ((variable) &body call)
  "CALL, whose last argument is the address VARIABLE names, checked; the
pointer it put there."
  `(cffi:with-foreign-object (,variable :pointer)
     (onnx-check (progn ,@call))
     (cffi:mem-ref ,variable :pointer)))

(defun onnx? ()
  "Whether this server has ONNX Runtime."
  (bt:with-lock-held (*onnx-lock*)
    (when (eq *onnx* :unknown)
      (setf *onnx*
            (handler-case
                (progn
                  (cffi:load-foreign-library *onnxruntime-file*)
                  (let* ((base (cffi:foreign-funcall "OrtGetApiBase" :pointer))
                         (table (cffi:foreign-funcall-pointer (cffi:mem-aref base :pointer 0) ()
                                                              :uint32 *onnx-api-version* :pointer)))
                    (if (cffi:null-pointer-p table)
                        nil
                        (let ((*onnx* table))
                          (with-foreign-floats-safe
                            (setf *onnx-env*
                                  (onnx-out (env)
                                    (onnx-call :create-env :int +ort-logging-level-error+
                                               :string "stratum" :pointer env :pointer))
                                  *onnx-memory*
                                  (onnx-out (info)
                                    (onnx-call :create-cpu-memory-info :int +ort-arena-allocator+
                                               :int +ort-mem-type-default+ :pointer info :pointer))))
                          table))))
              (error () nil))))
    *onnx*))

(defun onnx-session (file &key (threads *render-threads*))
  "A session of the model in FILE, run on THREADS threads."
  (with-foreign-floats-safe
    (let ((options (onnx-out (options) (onnx-call :create-session-options :pointer options :pointer))))
      (unwind-protect
           (progn
             (onnx-check (onnx-call :set-intra-op-threads :pointer options :int threads :pointer))
             (onnx-check (onnx-call :set-inter-op-threads :pointer options :int 1 :pointer))
             (onnx-out (session)
               (onnx-call :create-session :pointer *onnx-env* :string (namestring file)
                          :pointer options :pointer session :pointer)))
        (onnx-call :release-session-options :pointer options :void)))))

(defun onnx-tensor (pointer dimensions)
  "A tensor over the floats at POINTER, of DIMENSIONS, which go on
belonging to the caller: they must outlive the tensor."
  (let ((rank (length dimensions)))
    (cffi:with-foreign-object (shape :int64 rank)
      (loop for d in dimensions for i from 0 do (setf (cffi:mem-aref shape :int64 i) d))
      (onnx-out (value)
        (onnx-call :create-tensor :pointer *onnx-memory* :pointer pointer
                   :size (* 4 (reduce #'* dimensions)) :pointer shape :size rank
                   :int +onnx-float+ :pointer value :pointer)))))

(defun onnx-release (value)
  (unless (or (null value) (cffi:null-pointer-p value))
    (onnx-call :release-value :pointer value :void)))

(defun onnx-run (session inputs outputs)
  "SESSION run on INPUTS, ((name . tensor) ...), asking for OUTPUTS, a
list of names: the runtime's tensors for them, in that order, which the
caller releases."
  (let ((n-in (length inputs)) (n-out (length outputs))
        (names '()))
    (unwind-protect
         (cffi:with-foreign-objects ((in-names :pointer n-in) (in-values :pointer n-in)
                                     (out-names :pointer n-out) (out-values :pointer n-out))
           (loop for (name . value) in inputs for i from 0
                 do (let ((s (cffi:foreign-string-alloc name)))
                      (push s names)
                      (setf (cffi:mem-aref in-names :pointer i) s
                            (cffi:mem-aref in-values :pointer i) value)))
           (loop for name in outputs for i from 0
                 do (let ((s (cffi:foreign-string-alloc name)))
                      (push s names)
                      (setf (cffi:mem-aref out-names :pointer i) s
                            (cffi:mem-aref out-values :pointer i) (cffi:null-pointer))))
           (with-foreign-floats-safe
             (onnx-check (onnx-call :run :pointer session :pointer (cffi:null-pointer)
                                    :pointer in-names :pointer in-values :size n-in
                                    :pointer out-names :size n-out :pointer out-values :pointer)))
           (loop for i below n-out collect (cffi:mem-aref out-values :pointer i)))
      (mapc #'cffi:foreign-string-free names))))

(defun onnx-floats (value)
  "The floats of the runtime's tensor VALUE and their dimensions:
(values pointer dimensions), the pointer valid while VALUE lives."
  (let ((info (onnx-out (info) (onnx-call :type-and-shape :pointer value :pointer info :pointer))))
    (unwind-protect
         (let* ((rank (cffi:with-foreign-object (n :size)
                        (onnx-check (onnx-call :dimensions-count :pointer info :pointer n :pointer))
                        (cffi:mem-ref n :size)))
                (dimensions (cffi:with-foreign-object (d :int64 rank)
                              (onnx-check (onnx-call :dimensions :pointer info :pointer d :size rank :pointer))
                              (loop for i below rank collect (cffi:mem-aref d :int64 i)))))
           (values (onnx-out (data) (onnx-call :tensor-data :pointer value :pointer data :pointer))
                   dimensions))
      (onnx-call :release-type-and-shape :pointer info :void))))
