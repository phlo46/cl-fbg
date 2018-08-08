(defsystem "cl-fbg"
  :version "0.1.0"
  :author "Long Nguyen (ngphilong46@gmail.com)"
  :license "MIT"
  :depends-on ("dexador" "jonathan")
  :components ((:module "src"
                :components
                ((:file "cl-fbg"))))
  :description "Common Lisp SDK for Facebook Graph API"
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "cl-fbg-test"))))
