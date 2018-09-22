(defpackage cl-fbg-test.test-util
  (:use :cl :cl-fbg.util :iterate)
  (:export
   :create-test-users
   :create-test-user-friends
   :clean-up-test-users
   :clean-up-test-users-from-app))

(in-package :cl-fbg-test.test-util)

(defparameter *test-user-ids* nil)

(defun create-test-users (app-id app-access-token &key (amount 1))
  "See: https://developers.facebook.com/docs/graph-api/reference/v3.1/app/accounts/test-users"
  (let ((url (build-fb-url (format nil "~a/accounts/test-users" app-id)
                           :params (list (cons "installed" "true")
                                         (cons "access_token" app-access-token))
                           :use-access-token nil)))
    (iter
      (for i from 1 to amount)
      (collect (jonathan:parse (dex:post url) :as :alist)))))

(defun create-test-user-friends (user friends)
  "See: https://developers.facebook.com/docs/graph-api/reference/v3.1/test-user/friends"
  (dolist (friend friends)
    (let ((user-id (get-assoc-value "id" user))
          (user-token (get-assoc-value "access_token" user))
          (f-id (get-assoc-value "id" friend))
          (f-token (get-assoc-value "access_token" friend)))
      (flet ((friend-request (from to token)
               (dex:post (build-fb-url (format nil "~a/friends/~a" from to))
                         :content (list (cons "access_token" token)))))
        (friend-request user-id f-id user-token)
        (friend-request f-id user-id f-token)))))

(defun clean-up-test-users (user-list app-access-token)
  (with-token app-access-token
    (let ((user-ids (mapcar (alexandria:curry #'get-assoc-value "id") user-list)))
      (cl-fbg:batch-request
       (iter
         (for user-id in user-ids)
         (collect (list :|method| "DELETE" :|relative_url| user-id)))))))

(defun clean-up-test-users-from-app (app-id app-access-token)
  (with-token app-access-token
    (let* ((url (format nil "~a/accounts/test-users" app-id))
           (resp (cl-fbg:get url :params (list (cons "limit" 100))))
           (app-test-users (get-assoc-value "data" resp))
           (app-test-user-ids (mapcar (lambda (u) (get-assoc-value "id" u))
                                      app-test-users)))
      (cl-fbg:batch-request
       (iter
         (for user-id in app-test-user-ids)
         (collect (list :|method| "DELETE" :|relative_url| user-id)))))))
