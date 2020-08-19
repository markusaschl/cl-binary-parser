(defsystem "cl-binary-parser"
  :version "0.5"
  :author "Markus Aschl <development@markusaschl.com>"
  :description "Transparent lisp structures <-> binary parser."
  :license "Public Domain"
  :depends-on (:nibbles
               :flexi-streams)
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "utils")
               (:file "builders")
               (:file "types")))
