(in-package :cl-rfc3339-test)

(test format-test
  (let ((now (local-time:now)))
    (let ((rfc (rfc3339-string now)))
      (is (= 24 (length rfc)))

      (is (char= #\T (elt rfc 10)))
      (is (char= #\. (elt rfc 19)))
      (is (char= #\Z (elt rfc 23)))

      (dolist (i '(4 7))
        (is (char= #\- (elt rfc i))))
      (dolist (i '(13 16))
        (is (char= #\: (elt rfc i)))))))

(test timestamp-test
  (is (string= "1970-01-01T00:00:00.000Z" (rfc3339-string (unix-to-timestamp 0))))
  (is (string= "2015-01-01T01:01:01.001Z"
               (rfc3339-string (encode-timestamp (expt 10 6) 1 1 1 1 1 2015
                                                 :timezone +utc-zone+))))
  (is (string= "2000-03-02T00:00:10.007Z"
               (rfc3339-string (make-timestamp :day 1 :sec 10 :nsec (* 7 (expt 10 6))))))
  (let ((rfc (parse-rfc3339-timestring "2014-12-18T06:56:45.200Z")))
    (is (string= "2014-12-18T06:56:45.533Z"
                 (rfc3339-string (adjust-timestamp rfc (offset :nsec (* 333 (expt 10 6)))))))))

(test string-test
  (let ((rfc "2014-12-18T06:56:45.899Z"))
    (is (string= rfc (rfc3339-string rfc)))
    (is (string= rfc (rfc3339-string "2014-12-18T06:56:45.8990Z")))
    (is (string= rfc (rfc3339-string "2014-12-18T06:56:45.89900Z")))
    (is (string= rfc (rfc3339-string "2014-12-18T06:56:45.899000Z"))))

  (let ((rfc "2014-12-18T06:56:45.000Z"))
    (is (string= rfc (rfc3339-string rfc)))
    (is (string= rfc (rfc3339-string "2014-12-18T06:56:45Z")))
    (is (string= rfc (rfc3339-string "2014-12-18T06:56:45.000Z")))
    (is (string= rfc (rfc3339-string "2014-12-18T06:56:45.000000Z")))
    (is (string= rfc (rfc3339-string "2014-12-18T06:56:45.000000000Z")))))

(test integer-test
  (is (string= "1970-01-01T00:00:00.000Z" (rfc3339-string 0)))
  (let ((rfc "2004-02-01T00:00:00.000Z"))
    (let ((unix (+ (* 3600 24 29) (timestamp-to-unix (parse-rfc3339-timestring rfc)))))
      (is (string= "2004-03-01T00:00:00.000Z" (rfc3339-string unix))))))

(test double-float-test
  (is (string= "1970-01-01T00:00:00.001Z" (rfc3339-string 1d-3)))
  (let ((rfc "2014-12-18T06:56:45.000Z"))
    (let ((unix (+ 173d-3 (timestamp-to-unix (parse-rfc3339-timestring rfc)))))
      (is (string= "2014-12-18T06:56:45.173Z" (rfc3339-string unix)))))

  ;; Negative epoch time
  (let ((str "1960-12-18T06:56:45.000Z"))
    (let ((unix (timestamp-to-unix (parse-rfc3339-timestring str))))
      (is (string= "1960-12-18T06:56:45.500Z" (rfc3339-string (+ 500d-3 unix)))))))

(test expected-failures
  ;; local-time does not handle leap seconds
  (signals local-time::invalid-timestring
    (rfc3339-string "1990-12-31T23:59:60Z")))

(test many ()
  (dolist (year '(2015))
    (dolist (month '(1 3 4 7 11))
      (dolist (day (loop for i from 1 to 28 by 3 collect i))
        (dolist (hour (loop for i from 0 to 23 by 3 collect i))
          (dolist (min '(0 1 59))
            (dolist (sec '(0 1 59))
              (dolist (nsec (list 0 (* 1 (expt 10 6)) (* 999 (expt 10 6))))
                (let ((stamp (local-time:encode-timestamp nsec sec min hour day month year
                                                          :timezone +utc-zone+)))
                  (is (string= (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d.~3,'0dZ"
                                       year month day hour min sec (/ nsec (expt 10 6)))
                               (rfc3339-string stamp))))))))))))
