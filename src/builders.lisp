(in-package :cloudless/libraries/binary-parser)



(deftype binary-enum (&rest keys) `(member ,@keys))


(defvar *types* (make-hash-table :test 'equalp))


(defgeneric read-value (type stream &key)
  (:documentation "Read from a stream of the given type and parse it into a LISP
  structure."))


(defgeneric write-value (type stream value &key)
  (:documentation "Write an object into a stream of the given type."))


(defmacro define-binary-type ((name . lisp-spec) ((reader-args &body reader-body)
                                                  (writer-args &body writer-body)))
  (let ((read-value (intern "READ-VALUE"))
        (write-value (intern "WRITE-VALUE")))
    `(progn
       (deftype ,name () ,lisp-spec)


       (defmethod ,read-value ((type (eql ',name)) ,@reader-args)
         ,@reader-body)

       (defmethod ,write-value ((type (eql ',name)) ,@writer-args)
         ,@writer-body))))


(declaim (inline get-binary-enum-value))
(defun get-binary-enum-value (type key)
  (let ((map (symbol-value
              (intern (concatenate 'string "*" (symbol-name type) "-MAP*")
                      :binary-parser/generated))))
    (gethash key (car map))))


(declaim (inline get-binary-enum-key))
(defun get-binary-enum-key (type value)
  (let ((map (symbol-value
              (intern (concatenate 'string "*" (symbol-name type) "-MAP*")
                      :binary-parser/generated))))
    (gethash value (cdr map))))


(defmacro define-binary-enum (name slots)
  (let ((map-name (intern (concatenate 'string "*" (symbol-name name) "-MAP*")
                         :binary-parser/generated)))
    (with-gensyms ((enum-map-keys "ENUM-MAP-KEYS-")
                   (enum-map-values "ENUM-MAP-VALUES-"))

      `(labels ((map-contains-key (table key)
                  (multiple-value-bind (value exists-p) (gethash key table)
                    (declare (ignore value))
                    exists-p))
                (insert-key-value-into-map (table key value)
                  (if (map-contains-key table key)
                      (error "The key \"~s\" would be a duplicate. In an enum those are not allowed." key)
                      (setf (gethash key table) value))))

         (let ((,enum-map-keys (make-hash-table))
               (,enum-map-values (make-hash-table)))
           ,@(loop for slot in slots
                   with max = -1
                   for key = (if (listp slot) (car slot) slot)
                   for value integer = (or (and (listp slot) (cdr slot)) (+ 1 max))
                   if (< max value)
                     do (setf max value)
                   end

                   collect `(insert-key-value-into-map ,enum-map-keys ,key ,value)
                   collect `(insert-key-value-into-map ,enum-map-values ,value ,key))
           (defparameter ,map-name (cons ,enum-map-keys ,enum-map-values))


           (define-binary-type (,name . `(binary-enum ,@(hash-table-keys ,enum-map-keys)))
               (((stream &key)
                  (get-binary-enum-key ',name (read-value 'unsigned-integer-32 stream)))
                ((stream enum-key &key)
                  (write-value 'unsigned-integer-32 stream (get-binary-enum-value ',name enum-key))))))))))


(defmacro define-binary-struct (definition &rest slots)
  (let* ((definition (if (listp definition) definition (list definition)))
         (definition-type (intern (symbol-name (car definition))))
         (type (intern "TYPE"))
         (object (intern "OBJECT"))
         (stream (intern "STREAM"))
         (result (intern "RESULT"))
         (read-value-function (symbolicate "READ-VALUE"))
         (write-value-function (symbolicate "WRITE-VALUE")))

    (labels
        ((slot-specifier (slot)
           (destructuring-bind (name default-value &rest args)
               (if (listp slot) slot (list slot nil))
             `(,name ,default-value ,@args)))

         (slot-reader (slot)
           (destructuring-bind (name default-value &key type &allow-other-keys)
               (if (listp slot) slot (list slot nil))
             (declare (ignore default-value))
             `(setf (slot-value ,result ',name)
                    (,read-value-function ',type ,stream))))

         
         (slot-writer (slot)
           (destructuring-bind (name default-value &key type &allow-other-keys)
               (if (listp slot) slot (list slot nil))
             (declare (ignore default-value))
             `(,write-value-function ',type ,stream (slot-value ,object ',name)))))

      `(progn
         (defstruct (,definition-type ,@(cdr definition))
           ,@(mapcar #'slot-specifier slots))

         (defmethod ,read-value-function
             ((,type (eql ',definition-type)) ,stream &key)
           (let ((,result (make-instance ',definition-type)))

             ,@(mapcar #'slot-reader slots)
             ,result))

         (defmethod ,write-value-function
             ((,type (eql ',definition-type))  ,stream ,object &key)
           (assert (typep ,object ',definition-type))
           ,@(mapcar #'slot-writer slots))))))
