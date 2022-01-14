(in-package :all-tests)

(defsuite all-suite ())

(defun run-all-tests (&key (use-debugger t))
  (clunit:run-suite 'all-suite :use-debugger use-debugger
                               :report-progress t))

;; (run-all-tests)
