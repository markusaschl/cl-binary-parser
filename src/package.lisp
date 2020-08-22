(uiop:define-package :cloudless/libraries/binary-parser
  (:use :common-lisp)
  (:nicknames :binary-parser)

  ;; builders.lisp
  (:export #:binary-enum
           #:read-value
           #:write-value
           #:define-binary-type
           #:define-binary-enum
           #:define-binary-struct)

  ;; types.lisp
  (:export #:+max-octet-array-length+
           #:unsigned-integer
           #:signed-integer
           #:unsigned-integer-16
           #:unsigned-integer-32
           #:unsigned-integer-64
           #:signed-integer-16
           #:signed-integer-32
           #:signed-integer-64
           #:binary-boolean
           #:binary-float
           #:binary-double
           #:octet
           #:fixed-array
           #:octet-array
           #:binary-string))


(uiop:define-package :cloudless/libraries/binary-parser/generated
  (:nicknames :binary-parser/generated))
