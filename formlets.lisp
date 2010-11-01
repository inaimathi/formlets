(in-package :formlets)

;;View related functions
(defun show-form-field (name type form-values form-errors)
  (let* ((s-name (string name)) (l-name (string-downcase s-name)))
    (html-to-stout
      (:li (:span :class "label" (str (name->label name)))
	   (cond ((equalp type :textarea) (htm (:textarea :name l-name)))
		 ((equalp type :password) (htm (:input :name l-name :class "text-box" :type (string type))))
		 ((equalp type :recaptcha) (htm (recaptcha)))
		 (t (htm (:input :name l-name 
				 :value (getf form-values (sym->keyword name))
				 :class "text-box" :type (string type)))))
	   (show-error form-errors (sym->keyword name))))))

(defmacro show-form ((form-name values errors &key (submit "Submit")) &body fields)
  (let ((n (string-downcase (string form-name))))
    `(html-to-stout
       (show-general-error ,errors)
       (:form :name ,n :id ,n :action ,(string-downcase (format nil "/validate-~a" n)) :method "post"
	      (:ul :class "form-fields"
		   ,@(mapcar (lambda (field) 
			       `(show-form-field ',(car field) ',(cadr field) ,values ,errors))
			     fields)
		   (:li (:span :class "label") (:input :type "submit" :class "submit" :value ,submit)))))))

;;Validation related functions
(defmacro validate-field ((field-name fail-message errors) test-fn)
  (if test-fn
      `(if (,test-fn ,field-name) 
	   ,errors
	   (append ,errors '(,(sym->keyword field-name) ,fail-message)))
      `,errors))

(defmacro validate-form ((origin-fn &key fields general-val general-message) &body on-success)
  (let ((field-names (mapcar (lambda (f) (car f)) fields)))
    `(let ((results '()))
       ,(if (not general-val)
	    `(progn ,@(mapcar 
		       (lambda (field) 
			 (let ((test-field (caddr field))
			       (field-type (cadr field)))
			   (cond (test-field 
				  `(setq results (validate-field (,(car field) ,(cadddr field) results) ,test-field)))
				 ((equalp :recaptcha field-type)
				  `(setq results (validate-field (,(car field) "You seem to have mistyped the recaptcha" results) validate-recaptcha))))))
		       fields))
	    `(unless (apply ,general-val (list ,@field-names)) (setq results (append results (list :general-error ,general-message)))))
       (if (not results) 
	   (progn ,@on-success)
	   (,origin-fn :form-values (list ,@(list->plist field-names)) :form-errors results)))))

;;Predicates
(defun validate-recaptcha (f)
  (recaptcha-passed? (post-parameter "recaptcha_challenge_field") (post-parameter "recaptcha_response_field") (real-remote-addr)))

;;Formlet definition
(defmacro def-formlet (formlet-name (source-fn fields &key general submit) &body on-success)
  (let ((name+type (mapcar (lambda (f) (list (car f) (cadr f))) fields))
	(f-names (mapcar #'car fields))
	(general-form (if general `(:general-val ,(car general) :general-message ,(cadr general)) nil)))
    `(progn (defun ,(intern (format nil "SHOW-~a-FORM" formlet-name)) (values errors)
	      (show-form (,formlet-name values errors :submit ,submit) ,@name+type))
	    (define-easy-handler (,(intern (format nil "VALIDATE-~a" formlet-name)) :uri ,(format nil "/validate-~(~a~)" formlet-name)) ,f-names
	      (validate-form (,source-fn :fields ,fields ,@general-form) ,@on-success)))))