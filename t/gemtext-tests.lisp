(in-package :gemtext-tests)

(defsuite gemtext-suite (all-suite))

(defun cmp-lines (xs ys)
  (cond ((and (null xs) (null ys)) t)
        ((and (null xs) ys) nil)
        ((and xs (null ys)) nil)
        ((not (gemtext:line-eq (car xs) (car ys))) nil)
        (t (cmp-lines (cdr xs) (cdr ys)))))

(deftest test-parse-elements (gemtext-suite)
  (loop with suite = `(("# title" . ,(make-instance 'gemtext:title
                                                    :text "title"
                                                    :level 1))
                       ("#title" . ,(make-instance 'gemtext:title
                                                   :text "title"
                                                   :level 1))
                       ("## title" . ,(make-instance 'gemtext:title
                                                     :text "title"
                                                     :level 2))
                       ("###title" . ,(make-instance 'gemtext:title
                                                     :text "title"
                                                     :level 3))
                       ("=>/foo" . ,(make-instance 'gemtext:link
                                                   :url (quri:uri "/foo")))
                       ("=> /foo bar" . ,(make-instance 'gemtext:link
                                                        :url (quri:uri "/foo")
                                                        :text "bar"))
                       ("* list item " . ,(make-instance 'gemtext:item
                                                         :text "list item"))
                       ("*text" . ,(make-instance 'gemtext:paragraph
                                                  :text "*text"))
                       (">cit" . ,(make-instance 'gemtext:blockquote
                                                 :text "cit")))
        for (str . exp) in suite
        do (assert-true (cmp-lines (gemtext:parse-string str)
                                   (list exp)))))
