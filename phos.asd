;;;; phos.asd

(asdf:defsystem #:phos
  :description "An experimental Gemini client library"
  :author "Omar Polo <op@omarpolo.com>"
  :license  "ISC"
  :version "0.0.1"
  :serial t
  :depends-on ("quri" "cl-ppcre" "trivia" "usocket" "cl+ssl" "cl-mime")
  :components ((:file "package")
               (:file "phos")
               (:file "gemtext")
               (:file "gemini")))

(asdf:defsystem #:phos/nodgui
  :description "An experimental GUI Gemini client"
  :depends-on ("phos" "nodgui")
  :components ((:file "ui")))
