(binary-parser:define-binary-struct
    (message
     (:conc-name message--)
     (:constructor %make-message))

    (signature 0 :type (unsigned-integer 16) :binary-type unsigned-integer16)
  (protocol 0 :type (unsigned-integer 16) :binary-type unsigned-integer16)
  (version 0 :type (unsigned-integer 16) :binary-type unsigned-integer16)
  (id 0 :type (unsigned-integer 16) :binary-type unsigned-integer16)
  (nonce 0 :type (unsigned-integer 64) :binary-type unsigned-integer64)
  (chunk nil :type (octet-vector *) :binary-type octet-array))
