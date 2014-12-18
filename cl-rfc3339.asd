(asdf:defsystem cl-rfc3339
    :author "Gregory Tod <lisp@gtod.net>"
    :version "0.1.0"
    :license "MIT"
    :description "Canonicalize time formats to a specifc RFC 3339 compliant string."
    :depends-on (#:local-time)
    :components ((:file "package")
                 (:file "rfc3339")))
