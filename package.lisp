(defpackage :cl-rfc3339
  (:use #:cl #:local-time)
  (:export #:rfc3339-string)
  (:documentation "Canonicalize LOCAL-TIME timestamps and other time
formats to a specific RFC 3339 compliant string."))
