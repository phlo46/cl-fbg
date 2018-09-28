(defpackage cl-fbg-test
  (:use
   :cl
   :cl-fbg.util
   :iterate
   :prove)
  (:shadowing-import-from :cl :get)
  (:import-from :dexador :http-request-failed))

(in-package :cl-fbg-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-fbg)' in your Lisp.

(plan 4)

(defparameter *fb-app-id* (uiop:getenv "FB_APP_ID"))
(defparameter *fb-app-secret* (uiop:getenv "FB_APP_SECRET"))

(defparameter *fb-app-token* (cl-fbg:get-app-access-token *fb-app-id* *fb-app-secret*))

;; == helper functions ==
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


;; == tests ==
(subtest "Test FB Graph GET data"
  (let* ((friend-count 3)
         (user (car (create-test-users *fb-app-id* *fb-app-token*)))
         (friends (create-test-users *fb-app-id* *fb-app-token* :amount friend-count)))
    (create-test-user-friends user friends)

    (with-token (get-assoc-value "access_token" user)

      (subtest "GET resource(s) successfully"
        (multiple-value-bind (body code) (cl-fbg:get "me")
          (is 200 code)
          (ok body))

        (multiple-value-bind (body code) (cl-fbg:get "me/friends")
          (is 200 code)
          (is (get-assoc-value "total_count" (get-assoc-value "summary" body))
              friend-count)))

      (subtest "Failed if lacks resource(s)"
        (is-error (cl-fbg:get "") http-request-failed)))

    (clean-up-test-users (cons user friends) *fb-app-token*)))

(subtest "Test FB search api"
  ;; search api: https://developers.facebook.com/docs/places/search/
  (let ((params (list (cons "type" "place")
                      (cons "q" "cafe")
                      (cons "center" "40.7304,-73.9921")
                      (cons "distance" "1000")
                      (cons "fields" "name,checkins,picture"))))
    (subtest "Success"
      (multiple-value-bind (body code) (cl-fbg:get "search" :params params)
        (is 200 code)
        (ok (get-assoc-value "data" body))))

    (subtest "Wrong type search"
      (let ((wrong-type-params (copy-alist params)))
        (rplacd (assoc "type" wrong-type-params :test 'equal) "abcde")
        (is-error (cl-fbg:get "search" :params wrong-type-params) http-request-failed)))))


(subtest "Test FB Graph POST data"
  (let* ((user (car (create-test-users *fb-app-id* *fb-app-token*)))
         (user-id (get-assoc-value "id" user))
         (new-name "alibaba"))
    (isnt new-name (get-assoc-value "name" user))

    (subtest "Update test-user name successfully"
      (with-token *fb-app-token*
        (let ((resp (cl-fbg:post user-id :content (list (cons "name" new-name)))))
          (ok resp)

          (with-token (get-assoc-value "access_token" user)
            (is new-name (get-assoc-value "name" (cl-fbg:get "me")))))))

    (clean-up-test-users (list user) *fb-app-token*)))

(subtest "Test get app access token"
  (is-type (cl-fbg:get-app-access-token *fb-app-id* *fb-app-secret*) 'string))

(finalize)
