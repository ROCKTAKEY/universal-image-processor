(in-package :uip)

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
