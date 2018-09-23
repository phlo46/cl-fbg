## cl-fbg

Common Lisp SDK for Facebook Graph API

#### Installation

#### Usage

##### Set access-token

``` common-lisp
;; set access-token
(setf cl-fbg.config:*access-token* "your-access-token")
(cl-fbg:get "me")

;; or use macro
(with-token *your-access-token*
  (cl-fbg:get "me"))
```

##### GET API
cl-fbg use [jonathan](https://github.com/Rudolph-Miller/jonathan) for parsing response value.

You can choose one of these formats: `:alist :plist :jsown`. Default value is `:alist`

``` common-lisp
;; GET node (https://graph.facebook.com/v2.11/me/)
(cl-fbg:get "me")
> (("id" . "1688973317770992") ("name" . "Long Nguyen"))

(cl-fbg:get "me" :result-as :jsown)
> (:OBJ ("id" . "1688973317770992") ("name" . "Long Nguyen"))

;; GET node with edge
(cl-fbg:get "me/feed")

;; GET node with fields (https://graph.facebook.com/v2.11/me?fields=id,name,email)
(cl-fbg:get "me" :params (list (cons "fields" "id,name,email")))

;; GET with limiting results
(cl-fbg:get "me/posts" :params (list (cons "limit" "3")))

;; GET response values
(multiple-value-bind (body code headers) (cl-fbg:get "me")
  (print code)
  (print headers))
```

##### POST API

``` common-lisp
;; publish a Comment on a Photo by using the Photo node's /comments edge
(cl-fbg:post "1809938745705498/comments" :content (list (cons "message" "Awesome!")))

;; update message field on an existing comment
(cl-fbg:post "1809938745705498_1810399758992730" :content (list (cons "message" "Happy-Holidays")))
```

##### Search API

``` common-lisp
;; ref: https://developers.facebook.com/docs/places/search/
(cl-fbg:get "search" :params (list (cons "type" "place")
                                   (cons "q" "cafe")
                                   (cons "center" "40.7304,-73.9921")
                                   (cons "distance" "1000")
                                   (cons "fields" "name,checkins,picture")))
```

##### Batch request

``` common-lisp
;; ref: https://developers.facebook.com/docs/graph-api/making-multiple-requests/#multiple_methods
(cl-fbg:batch-request
 (list
  (list :|method| "GET" :|relative_url| "me/feed?limit=1")
  (list :|method| "POST" :|relative_url| "me/feed" :|body| "message=Test")))
```

#### Author

Long Nguyen (ngphilong46@gmail.com)

#### License

MIT
