(in-package :formlets)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Predicates
;;;;;;;;;;;;;;;basic field predicates
(defun longer-than? (num) (lambda (f) (> (length f) num)))
(defun shorter-than? (num) (lambda (f) (< (length f) num)))
(defun matches? (regex) (lambda (f) (scan regex f)))
(defun mismatches? (regex) (lambda (f) (not (scan regex f))))

;;;;;;;;;;;;;;;file-related
;; a hunchentoot file tuple is '(temp-filename origin-filename file-mimetype)
(defun file-type? (&rest accepted-types)
  (lambda (hunchentoot-file-tuple) 
    (member (third hunchentoot-file-tuple) accepted-types :test #'equal)))

(defun file-smaller-than? (byte-size)
  (lambda (hunchentoot-file-tuple) 
    (> byte-size (file-size (car hunchentoot-file-tuple)))))

;;;;;;;;;;;;;;;recaptcha
(defun validate-recaptcha ()
  (recaptcha-passed? (post-parameter "recaptcha_challenge_field") (post-parameter "recaptcha_response_field") (real-remote-addr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;View related functions
(defun show-form-field (name type form-values form-errors)
  (let* ((s-name (string name)) (l-name (string-downcase s-name)))
    (html-to-stout
      (:li (:span :class "label" (str (name->label name)))
	   (case type 
	     (:textarea (htm (:textarea :name l-name (str (getf form-values (sym->keyword name))))))
	     (:password (htm (:input :name l-name :class "text-box" :type (string type))))
	     (:file (htm (:input :name l-name :class "file" :type (string type))))
	     (:recaptcha (htm (recaptcha)))
	     (otherwise (htm (:input :name l-name 
				     :value (getf form-values (sym->keyword name))
				     :class "text-box" :type (string type)))))
	   (show-error form-errors (sym->keyword name))))))

(defmacro show-form ((form-name values errors &key (submit "Submit") (enctype "application/x-www-form-urlencoded")) &body fields)
  (let ((n (string-downcase (string form-name))))
    `(html-to-stout
       (show-general-error ,errors)
       (:form :name ,n :id ,n :action ,(string-downcase (format nil "/validate-~a" n)) :enctype ,enctype :method "post"
	      (:ul :class "form-fields"
		   ,@(mapcar (lambda (field) 
			       `(show-form-field ',(car field) ',(cadr field) ,values ,errors))
			     fields)
		   (:li (:span :class "label") (:input :type "submit" :class "submit" :value ,submit)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Validation related functions
(defmacro validate-form ((origin-fn &key fields general) &body on-success)
  `(let ((results (list ,@(loop for field in fields
			     collect (sym->keyword (car field))
			     when (equalp :recaptcha (cadr field))
			     collect `(unless (validate-recaptcha) "You seem to have mistyped the recaptcha")
			     when (caddr field)
			     collect `(unless (funcall ,(caddr field) ,(car field)) ,(or (cadddr field) general))
			     else collect nil))))
     (if (all-valid? results)
	 (progn ,@on-success)
	 (,origin-fn :form-values (list ,@(loop for field in fields 
					     unless (member (cadr field) '(:password :file))
					     collect (sym->keyword (car field)) and collect (car field)))
		     :form-errors ,(if general `(list :general-error ,general) 'results)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Formlet definition
(defmacro def-formlet (formlet-name (source-fn fields &key general submit) &body on-success)
  (let* ((enctype "application/x-www-form-urlencoded")
	 (name+type (loop for f in fields collecting (list (car f) (cadr f)) when (equalp (cadr f) :file) do (setf enctype "multipart/form-data")))
	 (f-names (mapcar #'car fields)))
    `(progn (defun ,(intern (format nil "SHOW-~a-FORM" formlet-name)) (values errors)
	      (show-form (,formlet-name values errors :submit ,submit :enctype ,enctype) ,@name+type))
	    (define-easy-handler (,(intern (format nil "VALIDATE-~a" formlet-name)) :uri ,(format nil "/validate-~(~a~)" formlet-name)) ,f-names
	      (validate-form (,source-fn :fields ,fields :general ,general) ,@on-success)))))