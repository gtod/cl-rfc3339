(in-package :cl-rfc3339)

(defparameter *rfc3339-format* '((:year 4) #\- (:month 2) #\- (:day 2)
                                 #\T
                                 (:hour 2) #\: (:min 2) #\: (:sec 2) #\. (:msec 3)
                                 :gmt-offset-or-z)
  "This is our canonical format for RFC 3339 strings.  The timezone is
always UTC and the granularity is milliseconds.  All times represented
in this format will always have the same length and thus may be sorted
and compared.")

(defgeneric rfc3339-string (date-time)
  (:documentation "Convert DATE-TIME to the an RFC 3339 section 5.6
'date-time' format, a string, as specified by local-time format
*RFC3339-FORMAT*."))

(defmethod rfc3339-string ((stamp timestamp))
  "Convert local-time timestamp STAMP to our canonical RFC 3339 UTC
string format."
  (format-timestring nil stamp :format *rfc3339-format* :timezone +utc-zone+))

(defmethod rfc3339-string ((string string))
  "Normalize the STRING, which must already be in an RFC 3339 UTC
format, to our canonical RFC 3339 string format."
  (rfc3339-string (parse-rfc3339-timestring string)))

(defmethod rfc3339-string ((integer integer))
  "Convert the INTEGER, which must be a whole number of seconds since
the Unix epoch, to our canonical RFC 3339 string format."
  (rfc3339-string (unix-to-timestamp integer)))

(defmethod rfc3339-string ((real double-float))
  "Convert the double float REAL, which must be a number of seconds
since the Unix epoch, to our canonical RFC 3339 string format.  Some
lisp implementations may not support subsecond granularity."
  (multiple-value-bind (unix remainder) (floor real)
    (let ((nsec (round (* (expt 10 9) remainder))))
      (rfc3339-string (unix-to-timestamp unix :nsec nsec)))))

;; See http://naggum.no/lugm-time.html
;; But also local-time:valid-timestamp-p
(defvar *least-rfc3339-string* (rfc3339-string (make-timestamp :day -146097))
  "A somewhat arbitrary 'earliest' or least RFC 3339 string in our
canonical format.  Note well its value before using it for anything.")
