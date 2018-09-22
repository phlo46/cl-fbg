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
  (http-response-wrapper result-as (dex:get (build-fb-url obj :params params))))

(defun post (obj &key content (result-as :alist))
  (http-response-wrapper result-as (dex:post (build-fb-url obj) :content content)))

(defun batch-request (batch &key (result-as :alist) (include_headers "true"))
  (jonathan:parse
   (dex:post cl-fbg.config:*fb-graph-base-url*
             :content (list
                       (cons "access_token" cl-fbg.config:*access-token*)
                       (cons "include_headers" include_headers)
                       (cons "batch" (jonathan:to-json batch))))
   :as result-as))
