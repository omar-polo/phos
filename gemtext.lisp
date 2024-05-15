(in-package #:phos/gemtext)

(defclass element ()
  ((text :initform ""
         :initarg :text
         :accessor text
         :type string)))

(defclass title (element)
  ((level :initarg :level
          :accessor level
          :type integer
          :documentation "The nesting level of the title.

Synonymous to the HTML heading levels, i.e. level 1 is <h1> tag, level 2 is <h2> tag etc.")))

(defclass link (element)
  ((url :initarg :url
        :accessor url
        :type quri:uri)))

(defclass item (element)
  ())

(defclass paragraph (element)
  ())

(defclass blockquote (element)
  ())

(defclass verbatim (element)
  ((alt :initform nil
        :initarg :alt
        :accessor alt
        :type (or null string)
        :documentation "The alternative text for the verbatim block.

Is usually put at the same line as the opening backquotes.

Can be a programming language name or alternative text for, e.g., ASCII art.")))

(defun element-p (element) (typep element 'element))
(defun title-p (title) (typep title 'title))
(defun link-p (link) (typep link 'link))
(defun item-p (item) (typep item 'item))
(defun paragraph-p (paragraph) (typep paragraph 'paragraph))
(defun blockquote-p (blockquote) (typep blockquote 'blockquote))
(defun verbatim-p (verbatim) (typep verbatim 'verbatim))


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
          ((prefix-p "=>" s)  (let ((s (strim s 2)))
                                (if (string-equal s "")
                                    (make-instance 'paragraph :text "=>")
                                    (parse-link s))))
          ((prefix-p "* " s)  (make-instance 'item :text (strim s 1)))
          ((prefix-p ">" s)   (make-instance 'blockquote :text (strim s 1)))
          (t (make-instance 'paragraph :text (strim s 0))))))

(defmacro markerp (line)
  `(uiop:string-prefix-p "```" ,line))

(defun join-lines (doc)
  (labels ((blankp (string)
             (uiop:emptyp
              (string-trim '(#\Space #\Tab) string)))
           (join (rest)
             (let ((line (first rest)))
               (cond
                 ((null line)
                  nil)
                 ((and (or (paragraph-p line)
                           (blockquote-p line))
                       (not (blankp (text line))))
                  (let ((next-lines
                          (loop for next in (rest rest)
                                while (and (subtypep (type-of line) (type-of next))
                                           (not (uiop:emptyp
                                                 (string-trim '(#\Space #\Tab) (text next)))))
                                collect next)))
                    (if next-lines
                        (cons (make-instance (type-of line)
                                             :text (reduce (lambda (prev next)
                                                             (uiop:strcat prev " " next))
                                                           next-lines
                                                           :initial-value (text line)
                                                           :key #'text))
                              (join (nthcdr (1+ (length next-lines)) rest)))
                        (cons line (join (cdr rest))))))
                 (t
                  (cons line (join (cdr rest))))))))
    (join doc)))

(defun parse (in &key join-lines)
  "Parse gemtext from the stream IN."
  (let ((doc (loop with doc = nil
                   for line = (read-line in nil)
                   unless line
                     return (nreverse doc)
                   do (push
                       (if (markerp line)
                           (loop with label = (subseq line 3)
                                 with content = nil
                                 for line = (read-line in nil)
                                 when (or (not line)
                                          (markerp line))
                                   return (make-instance 'verbatim
                                                         :alt (unless (string-equal label "")
                                                                label)
                                                         :text (format nil "~{~A~%~^~}"
                                                                       (nreverse content)))
                                 do (push line content))
                           (parse-line line))
                       doc))))
    (if join-lines
        (join-lines doc)
        doc)))

(defun parse-string (str &key join-lines)
  "Parse the string STR as gemtext."
  (with-input-from-string (s str)
    (parse s :join-lines join-lines)))

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
       (equal (text a) (text b))))

(defmethod line-eq and ((a title) (b title))
  (eq (level a) (level b)))

(defmethod line-eq and ((a link) (b link))
  (quri:uri-equal (url a) (url b)))

(defmethod line-eq and ((a verbatim) (b verbatim))
  (equal (alt a) (alt b)))
