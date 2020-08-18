(defsystem "cl-binary-parser"
  :version "0.0.1"
  :author "Markus Aschl <development@markusaschl.com>"
  :description ""
  :depends-on (:nibbles
               :flexi-streams)
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "utils")
               (:file "builders")
               (:file "types")))
