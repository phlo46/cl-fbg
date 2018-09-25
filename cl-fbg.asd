(defsystem "cl-fbg"
  :version "0.1.0"
  :author "Long Nguyen (ngphilong46@gmail.com)"
  :license "MIT"
  :depends-on ("dexador" "jonathan" "iterate")
  :serial t
  :components ((:module "src"
                :components
                ((:file "config")
                 (:file "util")
                 (:file "cl-fbg"))))
  :description "Common Lisp SDK for Facebook Graph API"
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "cl-fbg-test"))))
