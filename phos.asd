;;;; phos.asd

(asdf:defsystem #:phos
  :description "An experimental Gemini client library"
  :author "Omar Polo <op@omarpolo.com>"
  :license "ISC"
  :version "0.0.1"
  :serial t
  :depends-on ("quri" "cl-ppcre" "trivia" "usocket" "cl+ssl")
  :components ((:file "package")
               (:file "phos")
               (:file "gemtext")
               (:file "gemini")))

(asdf:defsystem #:phos/nodgui
  :description "An experimental GUI Gemini client"
  :depends-on ("phos" "nodgui")
  :components ((:file "ui")))

(asdf:defsystem #:phos/test
  :description "Test suite for phos."
  :depends-on ("phos" "clunit2")
  :serial t
  :pathname "t"
  :components ((:file "package")
               (:file "all-tests")
               (:file "gemtext-tests")))
