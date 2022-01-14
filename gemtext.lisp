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

(defun parse-title (s)
  "Parse a line into a title."
  (destructuring-bind (h text)
      (cl-ppcre:split "\\s+" s :limit 2)
    (make-instance 'title :level (length h)
                          :text text)))

(defun make-link (url &optional text)
  (make-instance 'link :url (quri:uri url)
                       :text text))

(defun parse-link (s)
  "Parse a line into link."
  (match (cl-ppcre:split "\\s+" s :limit 3)
    ((list _ url)      (make-link url))
    ((list _ url text) (make-link url text))))

(defun parse-item (s)
  "Parse a line into an item"
  (match (cl-ppcre:split "\\s+" s :limit 2)
    ((list _ text) (make-instance 'item :text text))))

(defun parse-blockquote (s)
  "Parse a line into a blockquote."
  (match (cl-ppcre:split "\\s+" s :limit 2)
    ((list _ text) (make-instance 'blockquote :text text))))

(defun parse-line (s)
  (if (string= s "")
      (make-instance 'paragraph :text s)
      (case (char s 0)
        (#\# (parse-title s))
        (#\= (parse-link s))
        (#\* (parse-item s))
        (#\> (parse-blockquote s))
        (otherwise (make-instance 'paragraph :text s)))))

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
