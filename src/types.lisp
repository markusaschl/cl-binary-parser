(in-package :cloudless/libraries/binary-parser)



(defconstant +max-octet-array-length+ (* 50 1024 1024))


(deftype unsigned-integer (bit-length)
  (let ((from 0)
        (to (- (expt 2 bit-length) 1)))
    `(integer ,from ,to)))

(deftype signed-integer (bit-length)
  (let ((range (- (expt 2 bit-length) 1)))
    (let ((from (* range -1))
          (to range))
      `(integer ,from ,to))))


(define-binary-type (unsigned-integer-16 . `(unsigned-integer 16))
    (((stream &key) (nibbles:read-ub16/be stream))
     ((stream obj &key) (nibbles:write-ub16/be obj stream))))


(define-binary-type (unsigned-integer-32 . `(unsigned-integer 32))
    (((stream &key) (nibbles:read-ub32/be stream))
     ((stream obj &key) (nibbles:write-ub32/be obj stream))))


(define-binary-type (unsigned-integer-64 . `(unsigned-integer 64))
    (((stream &key) (nibbles:read-ub64/be stream))
     ((stream obj &key) (nibbles:write-ub64/be obj stream))))


(define-binary-type (signed-integer-16 . `(signed-integer 16))
    (((stream &key) (nibbles:read-sb16/be stream))
     ((stream obj &key) (nibbles:write-sb16/be obj stream))))


(define-binary-type (signed-integer-32 . `(signed-integer 32))
    (((stream &key) (nibbles:read-sb32/be stream))
     ((stream obj &key) (nibbles:write-sb32/be obj stream))))


(define-binary-type (signed-integer-64 . `(signed-integer 64))
    (((stream &key) (nibbles:read-sb64/be stream))
     ((stream obj &key) (nibbles:write-sb64/be obj stream))))


(define-binary-type (binary-boolean . `boolean)
    (((stream &key) (not (zerop (read-value 'signed-integer-16 stream))))
     ((stream obj &key) (write-value 'signed-integer-16 stream (if obj 1 0)))))


;; WARN: not sure how correct this is, more specifically, the type declaration
(define-binary-type (binary-float . `single-float)
    (((stream &key) (nibbles:read-ieee-single/be stream))
     ((stream obj &key) (nibbles:write-ieee-single/be obj stream))))


;; WARN: not sure how correct this is
(define-binary-type (binary-double .`double-float)
    (((stream &key) (nibbles:read-ieee-double/be stream))
     ((stream obj &key) (nibbles:write-ieee-double/be obj stream))))


(define-binary-type (octet . `(unsigned-byte 8))
    (((stream &key) (read-byte stream))
     ((stream obj &key) (write-byte obj stream))))


(define-binary-type (fixed-array . `simple-array)
    (((stream &key reader length)
       (let ((arr (make-array length)))
         (dotimes (i length)
           (setf (aref arr i) (funcall reader stream)))
         arr))

     ((stream array &key (writer))
       (let ((length (length array)))
         (dotimes (i length)
           (funcall writer stream (aref array i)))))))


(define-binary-type (octet-array . `(array octet (*)))
    (((stream &key buffer)
       (let ((len (read-value 'unsigned-integer-32 stream)))
         (when (> len +max-octet-array-length+)
           (error "Attempted to read a silly size array ~DMB" (truncate len (* 1024 1024))))
         (let ((sequence (or buffer (nibbles:make-octet-vector len))))
           (dotimes (i len)
             (setf (elt sequence i)
                   (read-value 'octet stream)))
           sequence)))

     ((stream sequence &key (start 0) end)
       (unless end (setf end (length sequence)))
       (let ((len (- end start)))
         (write-value 'unsigned-integer-32 stream len)
         (do ((i start (1+ i)))
             ((= i end) len)
           (write-value 'octet stream (aref sequence i)))
         (write-array-padding stream len))
       nil)))


(define-binary-type (binary-string . `string)
    (((stream &key)
       (let ((len (read-value 'unsigned-integer-32 stream)))
         (when (> len +max-octet-array-length+)
           (error "Attempted to read a silly size array ~DMB" (truncate len (* 1024 1024))))
         (let ((buff (nibbles:make-octet-vector (pad-index len))))
           (read-sequence buff stream)
           (flexi-streams:octets-to-string buff :start 0 :end len))))
     ((stream obj &key)
       (let* ((octets (flexi-streams:string-to-octets obj))
              (length (length octets)))
         (write-value 'unsigned-integer-32 stream length)
         (write-sequence octets stream)
         (write-array-padding stream length)))))
