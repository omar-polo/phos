(in-package #:phos/gemtext)

(defclass element ()
  ((text :initarg :text
         :initform "")))

(defclass title (element)
  ((level :initarg :level)))

(defclass link (element)
  ((url :initarg :url)))

(defclass item (element)
  ())

(defclass paragraph (element)
  ())

(defclass blockquote (element)
  ())

(defclass verbatim (element)
  ((alt :initarg :alt)))

(defun make-link (url &optional text)
  (make-instance 'link :url (quri:uri url)
                       :text text))

(defun parse-link (s)
  "Parse a line into link."
  (match (cl-ppcre:split "\\s+" s :limit 2)
    ((list url)      (make-instance 'link :url (quri:uri url)))
    ((list url text) (make-instance 'link :url (quri:uri url)
                                          :text text))))

(defun parse-line (s)
  (flet ((strim (s n)
           (string-trim '(#\Space #\Tab) (subseq s n)))
         (prefix-p (prfx str)
           (uiop:string-prefix-p prfx str)))
    (cond ((prefix-p "###" s) (make-instance 'title :level 3
                                                    :text (strim s 3)))
          ((prefix-p "##" s)  (make-instance 'title :level 2
                                                    :text (strim s 2)))
          ((prefix-p "#" s)   (make-instance 'title :level 1
                                                    :text (strim s 1)))
          ((prefix-p "=>" s)  (parse-link (strim s 2)))
          ((prefix-p "* " s)  (make-instance 'item :text (strim s 1)))
          ((prefix-p ">" s)   (make-instance 'blockquote :text (strim s 1)))
          (t (make-instance 'paragraph :text (strim s 0))))))

(defmacro markerp (line)
  `(uiop:string-prefix-p "```" ,line))

(defun parse (in)
  "Parse gemtext from the stream IN."
  (loop with doc = nil
        for line = (read-line in nil)
        unless line
          return (nreverse doc)
        do (push
            (if (markerp line)
                (loop with label = (subseq line 3)
                      with content = nil
                      for line = (read-line in nil)
                      unless line
                        do (error "non-closed verbatim")
                      when (markerp line)
                        return (make-instance 'verbatim
                                              :alt label
                                              :text (format nil "~{~A~%~^~}"
                                                            (nreverse content)))
                      do (push line content))
                (parse-line line))
            doc)))

(defun parse-string (str)
  "Parse the string STR as gemtext."
  (with-input-from-string (s str)
    (parse s)))

(defgeneric unparse (obj stream)
  (:documentation "Print a textual representation of OBJ onto STREAM."))

(defmethod unparse ((l list) stream)
  (dolist (item l)
    (unparse item stream)))

(defmethod unparse ((title title) stream)
  (with-slots (text level) title
    (dotimes (_ level)
      (format stream "#"))
    (format stream " ~a~%" text)))

(defmethod unparse ((link link) stream)
  (with-slots (url text) link
    (format stream "=> ~a ~a~%" url text)))

(defmethod unparse ((item item) stream)
  (with-slots (text) item
    (format stream "* ~a~%" text)))

(defmethod unparse ((p paragraph) stream)
  (with-slots (text) p
    (format stream "~a~%" text)))

(defmethod unparse ((v verbatim) stream)
  (with-slots (alt text) v
    (format stream "```~a~%~a```~%" alt text)))

(defmethod unparse ((b blockquote) stream)
  (with-slots (text) b
    (format stream "> ~a~%" text)))

(defgeneric line-eq (a b)
  (:documentation "t if the lines A and B are equals.")
  (:method-combination and))

(defmethod line-eq and ((a element) (b element))
  (and (eq (type-of a)
           (type-of b))
       (equal (slot-value a 'text)
              (slot-value b 'text))))

(defmethod line-eq and ((a title) (b title))
  (eq (slot-value a 'level)
      (slot-value b 'level)))

(defmethod line-eq and ((a link) (b link))
  (quri:uri-equal (slot-value a 'url)
                  (slot-value b 'url)))

(defmethod line-eq and ((a verbatim) (b verbatim))
  (equal (slot-value a 'alt)
         (slot-value b 'alt)))
