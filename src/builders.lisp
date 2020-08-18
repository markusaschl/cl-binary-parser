(in-package :cloudless/libraries/binary-parser)



(defconstant +max-octet-array-length+ (* 50 1024 1024))


(deftype octet ()
  '(unsigned-byte 8))

(deftype octet-vector (&optional (length '*))
  `(array octet (,length)))

(deftype unsigned-integer (bit-length)
  `(integer 0 ,(- (expt 2 bit-length) 1)))


(defgeneric read-value (type stream &key))
(defgeneric write-value (type stream value &key))


(defmacro define-binary-type (name ((reader-args &body reader-body)
                                    (writer-args &body writer-body)))
  `(progn
     (defmethod read-value ((type (eql ',name)) ,@reader-args)
       ,@reader-body)

     (defmethod write-value ((type (eql ',name)) ,@writer-args)
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
  (let ((definition (if (listp definition) definition (list definition)))
        (type (intern "TYPE"))
        (object (intern "OBJECT"))
        (stream (intern "STREAM"))
        (result (intern "RESULT"))
        (read-value-function (intern "READ-VALUE"))
        (write-value-function (intern "WRITE-VALUE")))

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
         (defstruct (,(car definition) ,@(cdr definition))
           ,@(mapcar #'slot-specifier slots))
         (intern "READ-VALUE" :cloudless/libraries/binary-parser)
         (defmethod ,read-value-function
             ((,type (eql ',(car definition)))  ,stream &key)
           (let ((,result (make-instance ',(car definition))))
             ,@(mapcar #'slot-reader slots)))


         (intern "WRITE-VALUE" :cloudless/libraries/binary-parser)
         (defmethod ,write-value-function
             ((,type (eql ',(car definition)))  ,stream ,object &key)
           (assert (typep ,object ',(car definition)))
           ,@(mapcar #'slot-writer slots))))))


(defun read-array-padding (stream array-length)
  (let ((m (mod array-length 4)))
    (unless (zerop m)
      (read-sequence (loop :for i :below (- 4 m) :collect 0) stream)))
  nil)


(defun write-array-padding (stream array-length)
  (let ((m (mod array-length 4)))
    (unless (zerop m)
      (write-sequence (subseq #(0 0 0) 0 (- 4 m)) stream))))


(defun pad-index (index)
  (let ((m (mod index 4)))
    (if (zerop m)
        index
        (+ index (- 4 m)))))
