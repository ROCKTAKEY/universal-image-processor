(eval-when (:compile-toplevel :load-toplevel :execute)
  (in-package :common-lisp-user)
  (asdf:load-system '#:cl-ppcre)
  (asdf:load-system '#:clim)
  (asdf:load-system '#:clim-lisp)
  (asdf:load-system '#:mcclim)
  (asdf:load-system '#:opticl)
  (asdf:load-system '#:select-file)
  (asdf:load-system '#:alexandria))

(defpackage #:universal-image-processor
  (:nicknames #:uip)
  (:use #:clim-lisp #:alexandria #:opticl)
  (:export :run))

(in-package :uip)

(clim:define-application-frame uip-frame ()
  ((image :initarg :image
          :accessor uip-frame/image
          :initform (make-8-bit-rgb-image 400 400))
   (images :initarg :images
           :accessor uip-frame/images
           :initform nil)
   (file :initarg :file
         :accessor uip-frame/file)
   (directory :initarg :directory
              :accessor uip-frame/directory
              :initform #P"~/"))
  (:menu-bar t)
  (:panes
   (main-image :application
               :display-function 'display-image
               :scroll-bars :both
               :incremental-redisplay t
               :height 1000
               :width 1300)
   (hideshow (clim:with-radio-box
                 (:type :some-of
                  :orientation :horizontal
                  :value-changed-callback #'hideshow-updated-callback)
               (clim:radio-box-current-selection "F1")
               (clim:radio-box-current-selection "F2")
               (clim:radio-box-current-selection "F3")
               (clim:radio-box-current-selection "F4"))))

  (:layouts
   (default (clim:vertically ()
              (18/20 (clim:labelling (:label "Test Image"
                                 :align-x :center
                                 :label-alignment :top)
                       main-image))
              (2/20 (clim:labelling (:label "HS")
                      hideshow))))))

(defmethod initialize-instance :after ((obj uip-frame) &key))

(defun hideshow-updated-callback (gadget value)
  (declare (ignore gadget))
  (setf (uip-frame/image clim:*application-frame*)
        (create-blend-image
         :images (uip-frame/images clim:*application-frame*)
         :hidden (mapcar
                  (lambda (str)
                    (not (some
                          (lambda (arg)
                            (string= (clim:gadget-label arg) str))
                          value)))
                  (mapcar
                   (lambda (arg)
                     (concatenate 'string "F" (write-to-string arg)))
                   (iota (length (uip-frame/images clim:*application-frame*))
                         :start 1)))))
  (clim:redisplay-frame-pane
   clim:*application-frame*
   (clim:find-pane-named clim:*application-frame* 'main-image)))

(defun main-image-update (frame &optional hidden)
  (setf (uip-frame/image frame)
        (create-blend-image
         :images (uip-frame/images frame)
         :hidden (or hidden
                     (mapcar
                      (lambda (str)
                        (not (some
                              (lambda (arg)
                                (string= (clim:gadget-label arg) str))
                              (clim:find-pane-named frame 'hideshow))))
                      (mapcar
                       (lambda (arg)
                         (concatenate 'string "F" (write-to-string arg)))
                       (iota (length (uip-frame/images frame))
                             :start 1))))))
  (clim:redisplay-frame-pane
   clim:*application-frame*
   (clim:find-pane-named clim:*application-frame* 'main-image)))

(defun display-image (frame stream)
  (clim:updating-output (stream)
    (clim:draw-design stream (opticl-image-to-clim-image (uip-frame/image frame)))))

(defun run ()
  (clim:run-frame-top-level (clim:make-application-frame 'uip-frame)))

(define-uip-frame-command (com-file :name t :menu t)
    ()
  (let ((file
          (setf
           (uip-frame/file clim:*application-frame*)
           (select-file:select-file
            :directory (uip-frame/directory clim:*application-frame*)))))
    (when file
      (setf (uip-frame/directory clim:*application-frame*)
            (directory-namestring file))
      (multiple-value-bind (_ match)
          (cl-ppcre:scan-to-strings "(^.*?)(_F[1-9][0-9]*)?\\.(png|jpe?g|tiff?|bmp|gif)$"
                                    (namestring file))
        (declare (ignore _))
        (setq file (format nil "~A.~A" (aref match 0) (aref match 2)))
        (setf
         (uip-frame/images clim:*application-frame*)
         (append
          (list (read-image-file file))
          (loop
            with c
            for x from 2
            while
            (ignore-errors
             (setq c (read-image-file
                      (concatenate 'string
                                   (aref match 0) "_F"(write-to-string x) "." (aref match 2)))))
            collect c))))
      (main-image-update
       clim:*application-frame*
       (make-list (length (uip-frame/images clim:*application-frame*)))))))

(define-uip-frame-command (com-new-frame :name t :menu t)
    ()
  (clim:run-frame-top-level
   (clim:make-application-frame
    'uip-frame
    :directory (uip-frame/directory clim:*application-frame*))))

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
