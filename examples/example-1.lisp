(uiop:define-package :cloudless/libraries/binary-parser/examples
  (:use :common-lisp)
  (:use :binary-parser)
  (:nicknames :binary-parser/examples))


(in-package :binary-parser/examples)


(binary-parser:define-binary-struct
    (message
     (:conc-name message--)
     (:constructor %make-message))

    (signature 0 :type (unsigned-integer 16) :binary-type :unsigned-integer16)
  (protocol 0 :type (unsigned-integer 16) :binary-type :unsigned-integer16)
  (version 0 :type (unsigned-integer 16) :binary-type :unsigned-integer16)
  (id 0 :type (unsigned-integer 16) :binary-type :unsigned-integer16)
  (nonce 0 :type (unsigned-integer 64) :binary-type :unsigned-integer64)
  (chunk (make-array 0 :element-type 'octet) :type (octet-vector *) :binary-type :octet-array))


(defparameter *message* (%make-message :signature 123
                                       :protocol 3000
                                       :version 1
                                       :id 435))

(with-open-file (out "message.bin"
                     :direction :output
                     :if-exists :supersede
                     :element-type '(unsigned-byte 8))

  (write-value :message out *message*))


(with-open-file (in "message.bin"
                     :direction :input
                     :element-type '(unsigned-byte 8))

  (format t "~S~%" (read-value :message in)))
