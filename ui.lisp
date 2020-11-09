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

(defparameter *paragraph-font* "serif 12"
  "Font for the normal text")

(defparameter *window-content* nil)

(defparameter *current-url* nil)

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
      (pack w :side :top))))

(defmethod render ((link gemtext:link) f)
  (with-slots ((text phos/gemtext:text)
               (url phos/gemtext:url))
      link
    (let ((w (make-instance 'button
                            :master f
                            ;; :font *link-font*
                            :text (format nil "~a" (or text url))
                            :command (lambda ()
                                       (format t "before rendering ~a~%" url)
                                       (do-render url)))))
      (pack w :side :top))))

(defmethod render ((item gemtext:item) f)
  (with-slots ((text phos/gemtext:text)) item
    (let ((w (make-instance 'label
                            :master f
                            :font *item-font*
                            :text (format nil "* ~a" text))))
      (pack w :side :top))))

(defmethod render ((par gemtext:paragraph) f)
  (with-slots ((text phos/gemtext:text)) par
    (let ((w (make-instance 'label
                            :master f
                            :font *paragraph-font*
                            :text text)))
      (pack w :side :top))))

(defmethod render ((v gemtext:verbatim) f)
  (with-slots ((text phos/gemtext:text)
               (alt phos/gemtext:alt))
      v
    (let ((w (make-instance 'label
                            :master f
                            :font *verbatim-font*
                            :text text)))
      (pack w :side :top)
      (when alt
        (pack (make-instance 'label
                             :master f
                             :text alt)
              :side :top)))))

(defun clear-window ()
  (pack-forget-all *window-content*))

(defun do-render (url)
  (let* ((uri           (quri:uri url))
         (*current-url* uri)
         (base          (quri:copy-uri uri)))
    (setf (quri:uri-path base) nil)
    (clear-window)
    (render (gemtext:parse-string "# loading...")
            *window-content*)
    (clear-window)
    (multiple-value-bind (status body) (gemini:request url)
      (declare (ignore status))
      (render (gemtext:parse-string body base)
              *window-content*))))

(defun main (url)
  (with-nodgui (:title "phos")
    (let ((sf (make-instance 'scrolled-frame)))
      (setf *window-content* (interior sf))
      (pack sf :side :top :fill :both :expand t)
      (do-render url))))

;; (nodgui.demo:demo)

;; (main "gemini://gemini.omarpolo.com/")
