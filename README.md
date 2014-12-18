CL-RFC3339
==========

A trivial Common Lisp library to convert
[local-time](https://github.com/dlowe-net/local-time) timestamps,
existing RFC 3339 strings or Unix epoch times (integer or double
float) to a canonical [RFC 3339](https://www.ietf.org/rfc/rfc3339.txt)
compliant format in UTC with millisecond granularity.  For example

```
2015-12-28T02:59:00.000Z
```

This is the same format returned by the JavaScript function

```JavaScript
Date.prototype.toISOString()
```

which was [standardized in ECMA-262 5th edition]
(https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toISOString).

To be clear, RFC 3339 leave some wriggle room.  For example, the
literal `T` above may be a space, something the PostgreSQL project
tends to use.  But, to paraphrase section 5.1 of the above RFC 3339,
by standardizing on a **canonical** format viz:

* always using timezeone `Z` (that is, UTC)
* always using millisecond granularity

the resulting time strings are *all the same length* and *all of the
same format* and so may be sorted and compared *as strings* and yet
provide the expected result as times.

For those people storing times without access to a first class time
type (eg. in JSON) such a standard format is desireable.

Of course, using Unix time in milliseconds is easier still (and
smaller and faster) but has the drawback that `1451271540000` is
rather opaque compared to the above string.

## Documentation

There is but one exported symbol, the generic function `rfc3339-string`:

```common-lisp
CL-USER> (defpackage :example
           (:use :cl :cl-rfc3339 :local-time))
(in-package :example)

(rfc3339-string (now))
"2014-12-18T20:31:35.743Z"

(rfc3339-string (timestamp-to-unix (now)))
"2014-12-18T20:31:50.000Z"

(rfc3339-string *)
"2014-12-18T20:31:50.000Z"

(rfc3339-string "2014-12-18T20:24:46.000001Z")
"2014-12-18T20:24:46.000Z"

(rfc3339-string "2014-12-18T20:24:46.499001Z")
"2014-12-18T20:24:46.499Z"

(rfc3339-string "2014-12-18T20:24:46.4999Z")
"2014-12-18T20:24:46.499Z"

(let ((str "2014-12-18T06:56:45.000Z"))
  (rfc3339-string (+ 173d-3 (timestamp-to-unix (parse-rfc3339-timestring str)))))
"2014-12-18T06:56:45.173Z"
```

## Install

Navigate to your `~/quicklisp/local-projects` directory and do
`git clone https://github.com/gtod/cl-rfc3339.git`.  Then at your
REPL evaluate:

```common-lisp
(ql:register-local-projects)
(ql:quickload :cl-rfc3339)
```

## Tests

```common-lisp
(ql:quickload :cl-rfc3339-test)
(cl-rfc3339-test:run)
```
