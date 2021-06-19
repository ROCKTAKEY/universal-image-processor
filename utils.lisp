(in-package :uip)

(defvar *default-color-list*
  '((1.0 0.0 0.0)
    (0.0 1.0 0.0)
    (0.0 0.0 1.0)
    (0.0 1.0 1.0)
    (1.0 0.0 1.0)
    (1.0 1.0 0.0)
    (1.0 1.0 1.0)))

(defmacro loop-with-image ((i j) image &body body)
  (let ((height '#:height)
        (width '#:width))
    `(with-image-bounds (,height ,width) ,image
       (loop for ,i below ,height
             do (loop for ,j below ,width
                      do (progn ,@body))))))

(defun opticl-image-to-clim-image (opticl-image)
  (let ((clim-image (make-instance
                     'clim-internals::image-pattern
                     :array
                     (make-array
                      (with-image-bounds (height width) opticl-image
                        (list height width))
                      :element-type '(unsigned-byte 32))))
        (clim-image-bit 8))
    (loop-with-image (i j)
        opticl-image
      (setf (pixel (clim-internals::pattern-array clim-image) i j)
            (let ((bit
                    (etypecase opticl-image
                      ((or 8-bit-rgb-image
                           8-bit-rgba-image
                           8-bit-gray-image
                           8-bit-gray-alpha-image)
                       8)
                      ((or 16-bit-rgb-image
                           16-bit-rgba-image
                           16-bit-gray-image
                           16-bit-gray-alpha-image)
                       16)
                      ((or 32-bit-rgb-image
                           32-bit-rgba-image
                           32-bit-gray-image
                           32-bit-gray-alpha-image)
                       32))))

              (multiple-value-bind (r g b a)
                  (pixel opticl-image i j)
                ;; r is bound by gray in case of gray image.
                (+ (ash (ash r (- clim-image-bit bit)) (* clim-image-bit 2))
                   (ash (ash (or g r) (- clim-image-bit bit)) (* clim-image-bit 1))
                   (ash (ash (or b r) (- clim-image-bit bit)) (* clim-image-bit 0))
                   (ash (ash (or a (1- (expt 2 bit)))
                             (- clim-image-bit bit))
                        (* clim-image-bit 3))
                   )))))
    clim-image))

(defmacro add-gray-image (result-image img &optional (color '(1.0 1.0 1.0)))
  (let ((pixel-size' #:pixel-size)
        (result-pixel-size '#:result-pixel-size))
    `(let ((,pixel-size (etypecase ,img
                          (16-bit-gray-image 16)
                          (8-bit-gray-image 8)))
           (,result-pixel-size (etypecase ,result-image
                                 (16-bit-rgb-image 16)
                                 (8-bit-rgb-image 8))))
       (with-image-bounds (height width)
           ,img
         (loop for i below height
               do (loop for j below width
                        do (let ((x (pixel ,img i j)))
                             (setf
                              (pixel ,result-image i j)
                              (values-list
                               (mapcar
                                (lambda (&rest v)
                                  (min (apply #'+ v) (1- (expt 2 ,result-pixel-size))))
                                (multiple-value-list (pixel ,result-image i j))
                                (mapcar
                                 (lambda (component)
                                   (round
                                    (* component x (/ (1- (expt 2 ,result-pixel-size))
                                                      (1- (expt 2 ,pixel-size))))))
                                 ,color)))))))))))

;; Each element of IMAGE-PATHS should be class pathname
(defun create-blend-image (&key images (color-list *default-color-list*) hidden)
  (let* ((hidden (or hidden (make-list (length images) :initial-element nil)))
         (result-image
           (with-image-bounds (height width) (car images)
             (make-8-bit-rgb-image height width :initial-element (values 0 0 0)))))
    (mapc
     (lambda (img color hide)
       (unless hide
         (add-gray-image result-image img color)))
     images color-list hidden)
    result-image))

(defun get-origin-image-file-name (file)
  (multiple-value-bind (_ match)
      (cl-ppcre:scan-to-strings "(^.*?)(_F([1-9][0-9]*))?\\.(png|jpe?g|tiff?|bmp|gif)$"
                                (namestring file))
    (declare (ignore _))
    (values (format nil "~A.~A" (aref match 0) (aref match 3))
            (vector (aref match 0) (aref match 3))
            (read-from-string (aref match 2)))))

(defun get-image-file-name-with-number (base extension number)
  (concatenate 'string
               base "_F"(write-to-string number) "." extension))
