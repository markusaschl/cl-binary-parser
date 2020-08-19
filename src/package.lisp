(uiop:define-package :cloudless/libraries/binary-parser
  (:use :common-lisp)
  (:nicknames :binary-parser)

  ;; constants
  (:export #:+max-octet-array-length+
           #:define-binary-struct)

  ;; pure lisp types
  (:export
   #:octet
   #:octet-vector
   #:unsigned-integer)

  ;; accessors
  (:export #:read-value
           #:write-value)

  ;; macros
  (:export #:define-binary-type
           #:define-binary-enum
           #:define-binary-struct)

  ;; binary types
  (:export #:unsigned-integer16
           #:integer16
           #:unsigned-integer32
           #:integer32
           #:unsigned-integer64
           #:integer64
           #:binary-boolean
           #:real32
           #:real64
           #:octet
           #:binary-string
           #:fixed-array
           #:octet-array
           #:binary-enum))
