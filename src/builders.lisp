(in-package :cloudless/libraries/binary-parser)



(defconstant +max-octet-array-length+ (* 50 1024 1024))


(deftype octet ()
  '(unsigned-byte 8))

(deftype octet-vector (&optional (length '*))
  `(array octet (,length)))

(deftype unsigned-integer (bit-length)
  `(integer 0 ,(- (expt 2 bit-length) 1)))


(defgeneric read-value (type stream &key)
  (:documentation "Read from a stream of the given type and parse it into a LISP
  structure."))


(defgeneric write-value (type stream value &key)
  (:documentation "Write an object into a stream of the given type."))


(defmacro define-binary-type (name ((reader-args &body reader-body)
                                    (writer-args &body writer-body)))
  `(progn
     (intern (symbol-name ',name))
     (defmethod read-value ((type (eql ,name)) ,@reader-args)
       ,@reader-body)

     (defmethod write-value ((type (eql ,name)) ,@writer-args)
       ,@writer-body)))


(defmacro define-binary-enum ((name &key (version 0)) slots)
  (let ((map-name (symbolicate "*GENERATED-" name "-MAP*")))
    (with-gensyms ((enum-map-keys "ENUM-MAP-KEYS-")
                   (enum-map-values "ENUM-MAP-VALUES-"))

      `(labels ((map-contains-key (table key)
                  (multiple-value-bind (value exists-p) (gethash key table)
                    (declare (ignore value))
                    exists-p))
                (insert-key-value-into-map (table key value)
                  (if (map-contains-key table key)
                      (error (format nil "The key \"~s\" would be a duplicate. In an enum those are not allowed." key))
                      (setf (gethash key table) value))))

         (defvar ,map-name (make-hash-table))
         (let ((,enum-map-keys (make-hash-table))
               (,enum-map-values (make-hash-table)))
           (declare (special ,map-name))
           ,@(loop for slot in slots
                   with max = -1
                   for key = (if (listp slot) (car slot) slot)
                   for value integer = (or (and (listp slot) (cadr slot)) (+ 1 max))
                   if (< max value)
                     do (setf max value)
                   end

                   collect `(insert-key-value-into-map ,enum-map-keys ,key ,value)
                   collect `(insert-key-value-into-map ,enum-map-values ,value ,key))
           (setf (gethash ,version ,map-name) (cons ,enum-map-keys ,enum-map-values)))))))


(defmacro define-binary-struct (definition &rest slots)
  (let* ((definition (if (listp definition) definition (list definition)))
         (definition-type (intern (symbol-name (car definition))))
         (definition-type-keyword (intern (symbol-name (car definition)) "KEYWORD"))
         (type (intern "TYPE"))
         (object (intern "OBJECT"))
         (stream (intern "STREAM"))
         (result (intern "RESULT"))
         (read-value-function (symbolicate "READ-VALUE"))
         (write-value-function (symbolicate "WRITE-VALUE")))

    (labels
        ((slot-specifier (slot)
           (destructuring-bind (name default-value &key type binary-type)
               (if (listp slot) slot (list slot))
             (declare (ignore binary-type))
             `(,name ,default-value ,@(when type `(:type ,type)))))

         (slot-reader (slot)
           (destructuring-bind (name default-value &key type binary-type)
               (if (listp slot) slot (list slot))
             (declare (ignore default-value type))
             `(setf (slot-value ,result ',name)
                    (,read-value-function ',binary-type ,stream))))

         (slot-writer (slot)
           (destructuring-bind (name default-value &key type binary-type)
               (if (listp slot) slot (list slot))
             (declare (ignore default-value type))
             `(,write-value-function ',binary-type ,stream (slot-value ,object ',name)))))

      `(progn
         (import 'cloudless/libraries/binary-parser:read-value)
         (import 'cloudless/libraries/binary-parser:write-value)
         (defstruct (,definition-type ,@(cdr definition))
           ,@(mapcar #'slot-specifier slots))

         (defmethod ,read-value-function
             ((,type (eql ,definition-type-keyword))  ,stream &key)
           (let ((,result (make-instance ',definition-type)))
             ,@(mapcar #'slot-reader slots)))

         (defmethod ,write-value-function
             ((,type (eql ,definition-type-keyword))  ,stream ,object &key)
           (assert (typep ,object ',definition-type))
           ,@(mapcar #'slot-writer slots))))))
