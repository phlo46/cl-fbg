(defpackage cl-fbg.util
  (:use :cl :iterate :cl-fbg.config)
  (:export
   :build-fb-graph-query-url
   :get-assoc-value
   :build-fb-url
   :with-token
   :http-response-wrapper))

(in-package :cl-fbg.util)

(defun get-assoc-value (value resp)
  "A helper function to get value from an assoc list"
  (cdr (assoc value resp :test 'equal)))

(defmacro http-response-wrapper (result-as &body body)
  "A wrapper for dexador http response. Parse response body as :result-as"
  `(let ((res (multiple-value-list ,@body)))
     (setf (car res) (jonathan:parse (car res) :as ,result-as))
     (values-list res)))

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
