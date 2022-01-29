(defpackage :universal-image-processor/summary
  (:use :clim-lisp :alexandria :opticl
   :universal-image-processor/utils)
  (:import-from :clim)
  (:export :summary-directory :uip-frame-summary))

(in-package :universal-image-processor/summary)

(clim:define-application-frame uip-frame-summary ()
  ((image-plist-hashtable :initarg :image-plist-hashtable
                     :accessor uip-frame-summary/image-plist-hashtable)
   (directory :initarg :directory
              :accessor uip-frame-summary/directory
              :initform #P"~/")
   (image :initarg :image
          :accessor uip-frame-summary/image))
  (:menu-bar t)
  (:panes
   (main-image :application
               :display-function 'summary-display-image
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

(defun summary-display-image (frame stream)
  (clim:updating-output (stream)
    (clim:draw-design stream (opticl-image-to-clim-image (uip-frame-summary/image frame)))))

(defun hideshow-updated-callback (gadget value)
  )

(defvar *summary-max-width* 5000)

(defvar *summary-gap-between-images* 5)

(defun summary-directory (frame directory)
  (let* ((ht (make-hash-table)))
    (mapc
     (lambda (file)
       (let ((base-file-info
               (multiple-value-bind (base ext)
                   (get-origin-image-file-name file)
                 (list base ext))))
         (incf (getf (gethash base-file-info ht) :number 0))))
     (uiop:directory-files directory))

    (let (sorted-key)
      (setq sorted-key (sort (hash-table-keys ht)
                             (lambda (key1 key2)
                               (if (string= (car key1) (car key2))
                                   (string-lessp (cdr key1) (cdr key2))
                                   (string-lessp (car key1) (car key2))))))

      (mapc
       (lambda (base-file-info)
         (let* ((base (car base-file-info))
                (ext (cdr base-file-info))
                result)
           (dotimes (i (getf (gethash base-file-info ht) :number))
             (push (get-image-file-name-with-number base ext (1+ i)) result))
           (setf (getf (gethash base-file-info ht) :images) (nreverse result))))
       sorted-key)

      (mapc
       (lambda (base-file-info)
         (setf (getf (gethash base-file-info ht) :image)
               (create-blend-image :images (getf (gethash base-file-info ht) :images))))
       sorted-key)

      (let ((y-next 0))
        (mapc
         (lambda (base-file-info)
           (let ((x 0) (y 0))
            (with-image-bounds (height width) (car (getf (gethash base-file-info ht) :images))
             (when (< *summary-max-width* (+ x width))
               (setq x 0)
               (setq y (+ y-next *summary-gap-between-images*))
               (setq y-next 0))
             (setf (getf (gethash base-file-info ht) :x) x)
             (setf (getf (gethash base-file-info ht) :y) y)
             (setq x (+ x width *summary-gap-between-images*))
             (maxf y-next (+ y height)))))
         sorted-key)

        (let ((image (make-8-bit-rgb-image *summary-max-width* y-next
                                           :initial-element (values 0 0 0))))
          (mapc
           (lambda (base-file-info)
             (with-image-bounds (height width) (car (getf (gethash base-file-info ht) :images))
               (loop
                 for i from (getf (gethash base-file-info ht) :x)
                   to (+ (getf (gethash base-file-info ht) :x)
                         width)
                 do
                    (loop
                      for j from 1 to height do
                        (setf (pixel image
                                     (+ (getf (gethash base-file-info ht) :y) j)
                                     (+ (getf (gethash base-file-info ht) :x) i))
                              (pixel (getf (gethash base-file-info ht) :image) j i))))))
           sorted-key))))))
