#|
  This file is a part of cl-fbg project.
  Copyright (c) 2018 Long Nguyen (ngphilong46@gmail.com)
|#

#|
  Author: Long Nguyen (ngphilong46@gmail.com)
|#

(defsystem "cl-fbg"
  :version "0.1.0"
  :author "Long Nguyen"
  :license "MIT"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "cl-fbg"))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "cl-fbg-test"))))
