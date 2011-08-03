(in-package :formlets)

;;;;;;;;;;;;;;;general shortcuts
(defmacro html-to-stout (&body body)
  `(with-html-output (*standard-output* nil :indent t) ,@body))

(defmacro html-to-str (&body body)
  "Returns HTML as a string, as well as printing to standard-out"
  `(with-html-output-to-string (*standard-output*) ,@body))

(defun split-validation-list (validation-list)
  (loop for (fn msg) on validation-list by #'cddr
	collect fn into list-of-fn
	collect msg into list-of-msg
	finally (return (values list-of-fn list-of-msg))))

(defun file-size (f-name)
  (with-open-file (stream f-name :direction :input :if-does-not-exist nil) (file-length stream)))