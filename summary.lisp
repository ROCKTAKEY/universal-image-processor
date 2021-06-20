(defpackage :universal-image-processor/summary
  (:use :clim-lisp :alexandria :opticl
        :universal-image-processor/utils))

(in-package :universal-image-processor/summary)

(clim:define-application-frame uip-frame-summary ()
  ((image-plist-list :initarg :image-plist-list
                     :accessor uip-frame-summary/image-plist-list)
   (directory :initarg :directory
              :accessor uip-frame-summary/directory
              :initform #P"~/"))
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

(defvar *summary-max-width* 5000)

(defun summary-directory (frame directory)
  (let* ((ht (make-hash-table)))
    (mapc
     (lambda (file)
       (let ((base-file-info
               (multiple-value-bind (base ext)
                   (get-origin-image-file-name file)
                 (list base ext))))
         (incf (getf (gethash ht base-file-info) :number 0))))
     (uiop:directory-files directory))

    (mapc
     (lambda (base-file-info)
       (let* ((base (car base-file-info))
              (ext (cdr base-file-info))
              (file (get-image-file-name-with-number base ext)))
         (read-image-file )))
     (hash-table-keys ht))))
