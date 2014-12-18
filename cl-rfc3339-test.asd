(asdf:defsystem cl-rfc3339-test
    :author "Gregory Tod <lisp@gtod.net>"
    :version "0.1.0"
    :license "MIT"
    :description "Tests for cl-rfc3339."
    :depends-on (#:cl-rfc3339 #:1am)
    :serial t
    :components ((:module "test"
                  :serial t
                  :components ((:file "package")
                               (:file "test")))))
