(in-package :formlets)

(defmacro html-to-stout (&body body)
  `(with-html-output (*standard-output* nil :indent t) ,@body))

(defun name->label (field-name)
  (string-capitalize (regex-replace-all "-" (string field-name) " ")))

(defmacro error-html (key tag class)
  `(html-to-stout
     (if (getf e-list ,key) (htm (,tag :class ,class (str (getf e-list ,key)) (str ""))))))

(defun show-general-error (e-list) (error-html :general-error :div "general-error"))
(defun show-error (e-list key) (error-html key :span "inline-error"))

(defun sym->keyword (s) (intern (symbol-name s) :keyword))

(defun list->plist (a-list &optional acc) 
  (if (not a-list) 
      acc 
      (let ((e (car a-list))) 
	(list->plist (cdr a-list) (append acc `(,(sym->keyword e) ,e))))))