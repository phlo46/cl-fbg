(defpackage cl-fbg
  (:nicknames :fbg)
  (:use :cl)
  (:shadow :get)
  (:export :get
           :post
           :batch-request
           :*access-token*
           :*fb-graph-version*))

(in-package :cl-fbg)

(defparameter *fb-graph-base-url* "https://graph.facebook.com")
(defparameter *fb-graph-version* "v2.11")
(defparameter *access-token* nil)

(defmacro build-fb-graph-query-url (obj params token &body body)
  `(let* ((uri (format nil "~a/~a/~a/?access_token=~a&~a"
                       *fb-graph-base-url* *fb-graph-version* ,obj
                       ,token (or ,params ""))))
     ,@body))

(defun get (obj &key params (result-as :alist) )
  (build-fb-graph-query-url obj params *access-token*
    (jonathan:parse (dex:get uri) :as result-as)))

(defun post (obj &key params (result-as :alist) )
  (build-fb-graph-query-url obj params *access-token*
    (jonathan:parse (dex:post uri) :as result-as)))

(defun batch-request (batch &key (result-as :alist) (include_headers "true"))
  (jonathan:parse
   (dex:post *fb-graph-base-url*
             :content (list
                       (cons "access_token" *access-token*)
                       (cons "include_headers" include_headers)
                       (cons "batch" (jonathan:to-json batch))))
   :as result-as))
