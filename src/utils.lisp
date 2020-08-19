(in-package :cloudless/libraries/binary-parser)



;; from alexandria
(defun symbolicate (&rest things)
  "Concatenate together the names of some strings and symbols, producing a
symbol in the current package."
  (let* ((length (reduce #'+ things
                         :key (lambda (x) (length (string x)))))
         (name (make-array length :element-type 'character)))
    (let ((index 0))
      (dolist (thing things (values (intern name)))
        (let* ((x (string thing))
               (len (length x)))
          (replace name x :start1 index)
          (incf index len))))))


;; from alexandria
(deftype string-designator ()
  "A string designator type. A string designator is either a string, a symbol,
or a character."
  `(or symbol string character))


;; from alexandria
(defmacro with-gensyms (names &body forms)
  "Binds a set of variables to gensyms and evaluates the implicit progn FORMS.

Each element within NAMES is either a symbol SYMBOL or a pair (SYMBOL
STRING-DESIGNATOR). Bare symbols are equivalent to the pair (SYMBOL SYMBOL).

Each pair (SYMBOL STRING-DESIGNATOR) specifies that the variable named by SYMBOL
should be bound to a symbol constructed using GENSYM with the string designated
by STRING-DESIGNATOR being its first argument."
  `(let ,(mapcar (lambda (name)
                   (multiple-value-bind (symbol string)
                       (etypecase name
                         (symbol
                          (values name (symbol-name name)))
                         ((cons symbol (cons string-designator null))
                          (values (first name) (string (second name)))))
                     `(,symbol (gensym ,string))))
          names)
     ,@forms))


;; from frpc
(defun read-array-padding (stream array-length)
  (let ((m (mod array-length 4)))
    (unless (zerop m)
      (read-sequence (loop :for i :below (- 4 m) :collect 0) stream)))
  nil)


;; from frpc
(defun write-array-padding (stream array-length)
  (let ((m (mod array-length 4)))
    (unless (zerop m)
      (write-sequence (subseq #(0 0 0) 0 (- 4 m)) stream))))


;; from frpc
(defun pad-index (index)
  (let ((m (mod index 4)))
    (if (zerop m)
        index
        (+ index (- 4 m)))))


;; from alexandria
(defun remove-from-plist (plist &rest keys)
  "Returns a propery-list with same keys and values as PLIST, except that keys
in the list designated by KEYS and values corresponding to them are removed. The
returned property-list may share structure with the PLIST, but PLIST is not
destructively modified. Keys are compared using EQ."
  (declare (optimize (speed 3)))
  ;; FIXME: possible optimization: (remove-from-plist '(:x 0 :a 1 :b 2) :a)
  ;; could return the tail without consing up a new list.
  (loop for (key . rest) on plist by #'cddr
        do (assert rest () "Expected a proper plist, got ~S" plist)
        unless (member key keys :test #'eq)
        collect key and collect (first rest)))
