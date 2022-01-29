(in-package :cl-user)

(defpackage #:universal-image-processor-asd
  (:nicknames #:uip-asd)
  (:use #:cl #:asdf))

(in-package :uip-asd)

(defsystem "universal-image-processor"
  :class :package-inferred-system
  :description "Process image universally."
  :version "0.0"
  :author "ROCKTAKEY"
  :depends-on ("universal-image-processor/main"))
