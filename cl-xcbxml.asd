(in-package :asdf-user)
(defsystem "cl-xcbxml"
  :description "Parser for xorg protocol in xml format."
  :version "0.0.1"
  :author "Johannes Martinez Calzada"
  :licence "MIT"
  :depends-on ("s-xml")
  :components ((:file "package")
	       (:file "utils")
	       (:file "parse")
	       (:file "generate")
	       (:file "clx-print")
	       (:file "load")))
