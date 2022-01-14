(defpackage #:phos/ui
  (:documentation "User Interface for phos")
  (:use #:cl #:nodgui))

(in-package #:phos/ui)

(defparameter *title-1-font* "serif 22"
  "Font for the level 1 title.")

(defparameter *title-2-font* "serif 19"
  "Font for the level 2 title.")

(defparameter *title-3-font* "serif 16"
  "Font for the level 3 title.")

(defparameter *verbatim-font* "monospace 12"
  "Font for the verbatim element.")

(defparameter *item-font* "serif 12"
  "Font for the item.")

(defparameter *link-font* "serif 12"
  "Font for the links.")

(defparameter *blockquote-font* "serif 12 italic"
  "Font for the quotations.")

(defparameter *paragraph-font* "serif 12"
  "Font for the normal text")

(defparameter *url-bar* nil)
(defparameter *window-content* nil)

(defparameter *current-url* nil)

(defun join-paths (url path query)
  (setf (quri:uri-query url) query)
  (if (uiop:string-prefix-p "/" path)
      (setf (quri:uri-path url) path)
      (let ((p (quri:uri-path url)))
        (setf (quri:uri-path url)
              (format nil "~a~a"
                      (if (uiop:string-suffix-p "/" p)
                          p
                          (directory-namestring p))
                      path))))
  url)

(defun navigate-to (uri)
  (let ((hostname (quri:uri-host uri))
        (path (quri:uri-path uri))
        (query (quri:uri-query uri)))
    (if hostname
        (do-render uri)
        (do-render (join-paths (quri:copy-uri *current-url*)
                               path
                               query)))))

(defgeneric render (obj frame)
  (:documentation "Render OBJ in the nodgui FRAME"))

(defmethod render ((l list) f)
  (dolist (el l)
    (render el f)))

(defmethod render ((title gemtext:title) f)
  (with-slots ((text  phos/gemtext:text)
               (level phos/gemtext:level))
      title
    (let ((w (make-instance 'label
                            :master f
                            :font (case level
                                    (1 *title-1-font*)
                                    (2 *title-2-font*)
                                    (3 *title-3-font*))
                            :text (format nil "~v{~A~:*~} ~a" level '("#") text))))
      (pack w :side :top :fill :both :expand t))))

(defmethod render ((link gemtext:link) f)
  (with-slots ((text phos/gemtext:text)
               (url phos/gemtext:url))
      link
    (let ((w (make-instance 'button
                            :master f
                            ;; :font *link-font*
                            :text (format nil "~a" (or text url))
                            :command (lambda ()
                                       (navigate-to url)))))
      (pack w :side :top :fill :both :expand t))))

(defmethod render ((item gemtext:item) f)
  (with-slots ((text phos/gemtext:text)) item
    (let ((w (make-instance 'label
                            :master f
                            :font *item-font*
                            :text (format nil "* ~a" text))))
      (pack w :side :top :fill :both :expand t))))

(defmethod render ((q gemtext:blockquote) f)
  (with-slots ((text gemtext:text)) q
    (let ((w (make-instance 'message
                            :master f
                            :font *blockquote-font*
                            :justify :left
                            :text text
                            :width 600)))
      (pack w :side :top :fill :both :expand t))))

(defmethod render ((par gemtext:paragraph) f)
  (with-slots ((text phos/gemtext:text)) par
    (let ((w (make-instance 'message
                            :master f
                            :font *paragraph-font*
                            :justify "left"
                            :text text
                            :width 600)))
      ;; (setf (text w) text)
      ;; (configure w :state "disabled")
      (pack w :side :top :expand t :anchor "w"))))

(defmethod render ((v gemtext:verbatim) f)
  (with-slots ((text phos/gemtext:text)
               (alt phos/gemtext:alt))
      v
    (let ((w (make-instance 'label
                            :master f
                            :font *verbatim-font*
                            :text text)))
      (pack w :side :top :fill :both :expand t)
      (when alt
        (pack (make-instance 'label
                             :master f
                             :text alt)
              :side :top
              :fill :both
              :expand t)))))

(defun clear-window ()
  (pack-forget-all *window-content*))

(defun do-render (url)
  (let* ((uri (quri:uri url)))
    (setf *current-url* uri)
    (setf (text *url-bar*) url)
    (clear-window)
    (render (gemtext:parse-string "# loading...")
            *window-content*)
    (multiple-value-bind (status body) (gemini:request url)
      (declare (ignore status))
      (clear-window)
      (render (gemtext:parse-string body)
              *window-content*))))

(defun navigate-button-cb ()
  (navigate-to (quri:uri (string-trim '(#\newline #\space) (text *url-bar*)))))

(defun main (url)
  (with-nodgui (:title "phos")
    (set-geometry *tk* 800 600 0 0)
    (let* ((nav      (make-instance 'frame))
           (back-btn (make-instance 'button :text "←"   :master nav))
           (forw-btn (make-instance 'button :text "→"   :master nav))
           (go-btn   (make-instance 'button :text "GO!" :master nav :command #'navigate-button-cb))
           (url-bar  (make-instance 'text :height 1     :master nav))
           (sf       (make-instance 'scrolled-frame :padding 10 :takefocus nil)))
      (setf *url-bar* url-bar)
      (setf *window-content* (interior sf))
      (setf (text url-bar) "about:phos")
      (pack nav :fill :both)
      (pack back-btn :side :left)
      (pack forw-btn :side :left)
      (pack url-bar  :side :left :expand t :fill :both)
      (pack go-btn   :side :left)
      (pack sf :side :top :fill :both :expand t)
      (do-render url))))

;; (nodgui.demo:demo)

;; (main "gemini://gemini.omarpolo.com/")
