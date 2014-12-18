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

To be clear, RFC 3339 leaves some wriggle room.  For example, the
literal `T` above may be a spcae, something the PostgreSQL project
tends to use.  But, to paraphrase section 5.1 of the above RFC 3339,
by standardizing on a **canonical** format with timezeone `Z` for UTC
and millisecond granularity the resulting strings may be sorted and
compared *as strings* and yet provide the expected result as times.

For those people storing times without access to a first class date
time type (eg. JSON) such a standard format is desireable.
