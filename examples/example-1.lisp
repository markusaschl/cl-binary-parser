(uiop:define-package :cloudless/libraries/binary-parser/examples
  (:use :common-lisp)
  (:use :binary-parser)
  (:nicknames :binary-parser/examples))


(in-package :binary-parser/examples)


(binary-parser:define-binary-enum protocol (:foo (:bar 3000) :baz))


(binary-parser:define-binary-struct
    message
    (signature 123 :type unsigned-integer-16 :read-only t)
  (protocol :foo :type protocol)
  (version 0 :type unsigned-integer-16)
  (id 0 :type unsigned-integer-16)
  (nonce 0 :type unsigned-integer-16)
  (chunk (make-array 0 :element-type 'binary-parser:octet) :type octet-array))


(with-open-file (out "message.bin"
                     :direction :output
                     :if-exists :supersede
                     :element-type '(unsigned-byte 8))

  (write-value 'message out (make-message :signature 123
                                          :protocol :baz
                                          :version 1
                                          :id 435)))

(with-open-file (in "message.bin"
                     :direction :input
                     :element-type '(unsigned-byte 8))

  (format t "~S~%" (read-value 'message in)))



(binary-parser:define-binary-struct
    float-and-doubles
    (my-float 12.3 :type binary-float)
  (my-double 12.3 :type binary-double))



(with-open-file (out "message.bin"
                     :direction :output
                     :if-exists :supersede
                     :element-type '(unsigned-byte 8))

  (write-value 'float-and-doubles out (make-float-and-doubles)))

(with-open-file (in "message.bin"
                     :direction :input
                     :element-type '(unsigned-byte 8))

  (format t "~S~%" (read-value 'float-and-doubles in)))


(binary-parser:define-binary-struct
    string-test
    (my-string "Hello, world, ~%ößÖÄäüÜ" :type binary-string))



(with-open-file (out "message.bin"
                     :direction :output
                     :if-exists :supersede
                     :element-type '(unsigned-byte 8))

  (write-value 'string-test out (make-string-test)))

(with-open-file (in "message.bin"
                     :direction :input
                     :element-type '(unsigned-byte 8))

  (format t "~S~%" (read-value 'string-test in)))
