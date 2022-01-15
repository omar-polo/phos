(in-package #:phos/gemini)

(defparameter *default-port* 1965
  "The default port for gemini URL.")

(defparameter *code-to-keyword* '((10 :input)
                                  (11 :sensitive-input)
                                  (20 :success)
                                  (30 :redirect)
                                  (31 :permanent-redirect)
                                  (40 :temporary-failure)
                                  (41 :server-unavailable)
                                  (42 :cgi-error)
                                  (43 :proxy-error)
                                  (44 :slow-down)
                                  (50 :permanent-failure)
                                  (51 :not-found)
                                  (52 :gone)
                                  (53 :proxy-request-refused)
                                  (59 :bad-request)
                                  (60 :client-certificate-required)
                                  (61 :certificate-not-authorised)
                                  (62 :certificate-not-valid))
  "Maps status code to keyword name.")

(define-condition malformed-response (error)
  ((reason :initarg :reason :reader reason)))

(defun parse-status (s)
  (let* ((n (parse-integer s))
         (x (cadr (assoc n *code-to-keyword*))))
    (when x
      (return-from parse-status x))
    (let* ((l (* (floor (/ n 10))
                 10))
           (x (cadr (assoc l *code-to-keyword*))))
      (if x
          x
          (error 'malformed-response :reason (format nil "unknown response number ~a" s))))))

(defun parse-response (res)
  (unless (uiop:string-suffix-p res (format nil "~c~c" #\return #\newline))
    (error 'malformed-response :reason "response doesn't and with CRLF"))
  (unless (< (length res) 1024)
    (error 'malformed-response :reason "response is longer than 1024 bytes"))
  (setf res (string-trim '(#\return #\newline) res))
  (destructuring-bind (status &optional meta) (cl-ppcre:split "\\s+" res :limit 2)
    (when (and (< (parse-integer status) 40) (not meta))
      (error 'malformed-response :reason "missing meta"))
    (list (parse-status status) meta)))

(defun read-all-string (in)
  (with-output-to-string (out)
    (loop with buffer = (make-array 1024 :element-type 'character)
          for n-chars = (read-sequence buffer in)
          while (< 0 n-chars)
          do (write-sequence buffer out :start 0
                                        :end n-chars))))

(defun read-all-bytes (in)
  (let ((data (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop with buffer = (make-array 1024 :element-type '(unsigned-byte 8))
          for n-bytes = (read-sequence buffer in)
          for data-size = (array-dimension data 0)
          while (< 0 n-bytes)
          do (adjust-array data (+ data-size n-bytes))
          do (incf (fill-pointer data) n-bytes)
          do (replace data buffer :start1 data-size :end2 n-bytes))
    data))

(defun read-until (in char)
  (with-output-to-string (out)
    (loop for ch = (read-char in)
          when (char= ch char)
            return nil
          do (write-char ch out))
    (write-char char out)))

(defun do-request (host port req)
  "Perform the request REQ to HOST on PORT, blocking until the
response is fetched, then return the meta and the (decoded) body."
  (usocket:with-client-socket (socket stream host port)
    (let ((ssl-stream (cl+ssl:make-ssl-client-stream
                       stream :unwrap-stream-p t
                              :external-format '(:utf8 :eol-style :lf)
                              :verify nil
                              :hostname host)))
      (format ssl-stream "~a~c~c" req #\return #\newline)
      (force-output ssl-stream)
      (let ((resp (parse-response (read-until ssl-stream #\newline))))
        (values resp (if (and (eq (first resp) :success)
                              (second resp)
                              (string= (subseq (second resp) 0 5) "text/"))
                         (read-all-string ssl-stream)
                         (read-all-bytes ssl-stream)))))))

(defgeneric request (url)
  (:documentation "Perform a request for the URL"))

(defmethod request ((url string))
  (request (quri:uri url)))

(defmethod request ((url quri:uri))
  (let* ((u (quri:uri url))
         (port (or (quri:uri-port u) 1965))
         (host (quri:uri-host u)))
    (do-request host port url)))
