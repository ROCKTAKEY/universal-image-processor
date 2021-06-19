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
          (get-origin-image-file-name file)
        (declare (ignore _))
        (setq file (format nil "~A.~A" (aref match 0) (aref match 1)))
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
                      (get-image-file-name-with-number
                       (aref match 0) (aref match 1) x))))
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

(define-uip-frame-command (com-summary-directory :name t :menu t)
    ()
  (clim:run-frame-top-level
   (clim:make-application-frame
    'uip-frame
    :directory (uip-frame/directory clim:*application-frame*)))
  (summary-directory clim:*application-frame* (uip-frame/directory clim:*application-frame*)))
