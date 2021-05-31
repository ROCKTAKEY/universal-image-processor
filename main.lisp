(in-package :common-lisp-user)

(asdf:load-system '#:clim)
(asdf:load-system '#:clim-lisp)
(asdf:load-system '#:mcclim)
(asdf:load-system '#:opticl)

(defpackage #:universal-image-processor
  (:nicknames #:uip)
  (:use #:clim-lisp #:opticl)
  (:export :run-my-first-app))

(in-package :uip)

(defvar image-reader-alist
  '((png . read-png-file)
    (jpeg . read-jpeg-file)
    (tiff . read-tiff-file)
    (pbm . read-pbm-file)
    (pnm . read-pnm-file)
    (gif . read-gif-file))
  "")

;; Each element of IMAGE-PATHS should be class pathname
(defun create-blend-image (&rest image-paths)
  (let* ((images (mapcar #'read-tiff-file image-paths))
         (result-image (make-array (length (car images)))))
    (mapc
     (lambda (img)
       (typecase img
         (8-bit-rgb-image
          (locally
              (declare (type 8-bit-rgb-image img))
            (with-image-bounds (height width)
                img
              (time
               (loop for i below height
                     do (loop for j below width
                              do
                                 (multiple-value-bind (r g b)
                                     (pixel img i j)
                                   (declare (type (unsigned-byte 8) r g b))
                                   (setf (pixel img i j)
                                         (values (- 255 r) g b)))))))))))
     images)
    result-image))
