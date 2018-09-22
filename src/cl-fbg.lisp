(defpackage cl-fbg
  (:nicknames :fbg)
  (:use :cl :iterate :cl-fbg.util)
  (:shadow :get)
  (:export
   :get
   :post
   :batch-request))

(in-package :cl-fbg)

(defun get (obj &key params (result-as :alist))
  (let ((res (multiple-value-list (dex:get (build-fb-url obj :params params)))))
    (setf (car res) (jonathan:parse (car res) :as result-as))
    (values-list res)))

(defun post (obj &key content (result-as :alist))
  (jonathan:parse (dex:post (build-fb-url obj) :content content) :as result-as))

(defun batch-request (batch &key (result-as :alist) (include_headers "true"))
  (jonathan:parse
   (dex:post cl-fbg.config:*fb-graph-base-url*
             :content (list
                       (cons "access_token" cl-fbg.config:*access-token*)
                       (cons "include_headers" include_headers)
                       (cons "batch" (jonathan:to-json batch))))
   :as result-as))
