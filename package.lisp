;;;; package.lisp

(defpackage #:phos
  (:use #:cl))

(defpackage #:phos/gemtext
  (:documentation "Gemtext (text/gemini) parsing")
  (:nicknames :gemtext)
  (:use #:cl #:trivia)
  (:export :element :title :link :item :blockquote :paragraph :verbatim
           :text :url :alt :level
           :parse :parse-string :unparse))

(defpackage #:phos/gemini
  (:documentation "Gemini (the protocol) implementation")
  (:nicknames :gemini)
  (:use #:cl #:trivia)
  (:export :request))

(defpackage #:phos/ui
  (:documentation "User Interface for phos")
  (:use #:cl #:nodgui))
