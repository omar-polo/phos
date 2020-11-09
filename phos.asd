;;;; phos.asd

(asdf:defsystem #:phos
  :description "An experimental Gemini client"
  :author "Omar Polo <op@omarpolo.com>"
  :license  "ISC"
  :version "0.0.1"
  :serial t
  :depends-on ("quri" "cl-ppcre" "trivia" "ltk" "usocket" "cl+ssl" "cl-mime")
  :components ((:file "package")
               (:file "phos")
               (:file "gemtext")
               (:file "gemini")))
