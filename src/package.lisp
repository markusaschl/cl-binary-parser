(uiop:define-package :cloudless/libraries/binary-parser
  (:use :common-lisp)
  (:nicknames :binary-parser)
  (:export #:+max-octet-array-length+
           #:octet
           #:octet-vector
           #:unsigned-integer
           #:unsigned-integer
           #:read-value
           #:write-value
           #:define-binary-type
           #:define-binary-enum
           #:define-binary-struct))
