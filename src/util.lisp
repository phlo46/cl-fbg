(defpackage cl-fbg.util
  (:use :cl :iterate :cl-fbg.config)
  (:export
   :build-fb-graph-query-url
   :get-app-access-token
   :get-assoc-value
   :build-fb-url
   :with-token))

(in-package :cl-fbg.util)

(defun get-assoc-value (value resp)
  "A helper function to get value from an assoc list"
  (cdr (assoc value resp :test 'equal)))

(defmacro with-token (token &body body)
  `(let ((cl-fbg.config:*access-token* ,token))
     ,@body))

(defun build-fb-url (obj &key (params ()) (use-access-token t))
  "Build a Graph URL with input params"
  (let ((query (iter
                 (for (key . value) in params)
                 (collect (format nil "~a=~a" key value) into res)
                 (finally (return (format nil "~{~a~^&~}" res))))))
    (when use-access-token
      (setf query (concatenate 'string query (format nil "&access_token=~a" *access-token*))))
    (format nil "~a/~a/~a?~a" *fb-graph-base-url* *fb-graph-version* obj query)))

(defun get-app-access-token (app-id app-secret)
  (let ((url (build-fb-url "oauth/access_token"
                           :params (list
                                    (cons "client_id" app-id)
                                    (cons "client_secret" app-secret)
                                    (cons "redirect_uri" "https://lisp.org")
                                    (cons "grant_type" "client_credentials")))))
    (get-assoc-value "access_token" (jonathan:parse (dex:get url) :as :alist))))
