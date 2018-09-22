(defpackage cl-fbg-test
  (:use
   :cl
   :cl-fbg
   :cl-fbg.util
   :cl-fbg-test.test-util
   :prove)
  (:shadowing-import-from :cl :get)
  (:import-from :dexador :http-request-failed))

(in-package :cl-fbg-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-fbg)' in your Lisp.

(plan 4)

(defparameter *fb-app-id* (uiop:getenv "FB_APP_ID"))
(defparameter *fb-app-secret* (uiop:getenv "FB_APP_SECRET"))

(defparameter *fb-app-token* (get-app-access-token *fb-app-id* *fb-app-secret*))

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

    (clean-up-test-users (cons user friends))))

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

    (clean-up-test-users (list user))))

(subtest "Test get app access token"
  (is-type (get-app-access-token *fb-app-id* *fb-app-secret*) 'string))

(finalize)
