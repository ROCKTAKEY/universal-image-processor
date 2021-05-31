(in-package :cl-user)

(defpackage #:universal-image-processer-asd
  (:nicknames #:uip-asd)
  (:use #:cl #:asdf))

(in-package #:uip-asd)

(defsystem #:universal-image-processer-asd
  :class :package-inferred-system
  :description "Process image universally."
  :version "0.0"
  :author "ROCKTAKEY"
  :depends-on ("universal-image-processer/main"))
