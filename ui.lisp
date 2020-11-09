(in-package #:phos/ui)

(defgeneric render (obj frame)
  (:documentation "Render OBJ in the ltk FRAME"))

(defmethod render ((l list) f)
  (dolist (el l)
    (render el f)))

(defmethod render ((title gemtext:title) f)
  (with-slots ((text phos/gemtext:text)
               (level phos/gemtext:level))
      title
    (let ((w (make-instance 'label
                            :master f
                            :text (format nil "#(level ~a) ~a" level text))))
      (pack w :side :top))))

(defmethod render ((link gemtext:link) f)
  (with-slots ((text phos/gemtext:text)
               (url phos/gemtext:url))
      link
    (let ((w (make-instance 'button
                            :master f
                            :text (format nil "~a" (or text url)))))
      (pack w :side :top))))

(defmethod render ((item gemtext:item) f)
  (with-slots ((text phos/gemtext:text)) item
    (let ((w (make-instance 'label
                            :master f
                            :text (format nil "* ~a" text))))
      (pack w :side :top))))

(defmethod render ((par gemtext:paragraph) f)
  (with-slots ((text phos/gemtext:text)) par
    (let ((w (make-instance 'label
                            :master f
                            :text text)))
      (pack w :side :top))))

(defmethod render ((v gemtext:verbatim) f)
  (with-slots ((text phos/gemtext:text)
               (alt phos/gemtext:alt))
      v
    (let ((w (make-instance 'label
                            :master f
                            :text (format nil "```~a~%~a~&```"
                                          (or alt "")
                                          text))))
      (pack w :side :top))))

(defun render-page (page)
  (with-ltk ()
    (let ((frame (make-instance 'frame)))
      (pack frame)
      (render page frame))))

(defvar doc (with-open-file (stream #P"~/quicklisp/local-projects/phos/test.gmi")
              (gemtext:parse stream)))

;; (render-page doc)
