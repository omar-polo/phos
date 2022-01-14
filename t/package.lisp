(defpackage :all-tests
  (:use :cl :clunit)
  (:export :all-suite :run-all-tests))

(defpackage :gemtext-tests
  (:use :cl :clunit :all-tests)
  (:export))
