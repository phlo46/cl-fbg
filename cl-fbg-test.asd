#|
  This file is a part of cl-fbg project.
  Copyright (c) 2018 Long Nguyen (ngphilong46@gmail.com)
|#

(defsystem "cl-fbg-test"
  :defsystem-depends-on ("prove-asdf")
  :author "Long Nguyen"
  :license "MIT"
  :depends-on ("cl-fbg"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "cl-fbg"))))
  :description "Test system for cl-fbg"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
