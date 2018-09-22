(defpackage cl-fbg.config
  (:use :cl)
  (:export
   *access-token*
   *fb-graph-version*
   *fb-graph-base-url*))

(in-package :cl-fbg.config)

(defparameter *fb-graph-base-url* "https://graph.facebook.com")
(defparameter *fb-graph-version* "v2.11")
(defparameter *access-token* nil)
