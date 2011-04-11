(in-package :formlets)

;;;;;;;;;;;;;;;general shortcuts
(defmacro html-to-stout (&body body)
  `(with-html-output (*standard-output* nil :indent t) ,@body))

(defmacro html-to-str (&body body)
  "Returns HTML as a string, as well as printing to standard-out"
  `(with-html-output-to-string (*standard-output*) ,@body))

(defun sym->keyword (s) (intern (symbol-name s) :keyword))

(defun file-size (f-name)
  (with-open-file (stream f-name :direction :input :if-does-not-exist nil) (file-length stream)))

;;;;;;;;;;;;;;;view-related shortcuts
(defmacro error-html (key tag class)
  `(html-to-stout
     (if (getf e-list ,key) (htm (,tag :class ,class (str (getf e-list ,key)) (str ""))))))

(defun show-general-error (e-list) (error-html :general-error :div "general-error"))
(defun show-error (e-list key) (error-html key :span "inline-error"))

(defun name->label (field-name) (string-capitalize (regex-replace-all "-" (string field-name) " ")))

;;;;;;;;;;;;;;;validation-related shortcuts
(defun all-valid? (results)
  (loop for (k v) on results by #'cddr if v return nil else collect v))