(in-package :formlets)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HELPER MACROS
;; NOTE: If you REALLY want, you can define your own raw formlet instance and validator as in define-formlet.
;;       The same can be said about show-formlet. If you go this route, pay attention to how they communicate;
;;       currently they use Hunchentoots' session (which is the main reason :formlets isn't portable)
(defun define-field (field-name field-type &key size value-set default-value validation)
  "Takes a terse declaration and expands it into a make-instance for macro purposes"
  (let ((final-value-set (when value-set `(:value-set ,value-set)))
	(final-size (when size `(:size ,size))))
    (multiple-value-bind (functions messages) (split-validation-list validation)
      `(make-instance ',field-type :name ,(format nil "~(~a~)" field-name) 
		      :default-value ,default-value ,@final-value-set ,@final-size
		      :validation-functions (list ,@functions) :error-messages (list ,@messages)))))

(defmacro define-formlet ((name &key general-validation (submit "Submit")) (&rest fields) &rest on-success)
  "Converts a terse declaration form into the corresponding object and validation handler."
  ;;; the flet function converts a terse declaration into the corresponding make-instance declaration
  (let* ((field-names (mapcar #'car fields))
	 (field-objects (mapcar (lambda (f) (apply #'define-field f)) fields))
	 (enctype (if (every (lambda (f) (not (eq (cadr f) 'file))) fields)
		      "application/x-www-form-urlencoded" 
		      "multipart/form-data")))
    (multiple-value-bind (gen-val-fn gen-err-msg) (split-validation-list general-validation) 
      `(progn 
         ;;; declare formlet instance
	 (defparameter ,name
	   (make-instance 'formlet
			  :name ',name :submit ,submit :enctype ,enctype
			  :validation-functions ,(when general-validation `(list ,@gen-val-fn)) 
			  :error-messages ,(when general-validation `(list ,@gen-err-msg))
			  :fields (list ,@field-objects)
			  :on-success (lambda ,field-names (progn ,@on-success))))
	 
         ;;; declare validation handler
	 (define-easy-handler (,(intern (format nil "VALIDATE-~a" name)) :uri ,(format nil "/validate-~(~a~)" name)) ()
	   (let* ((formlet-values (post-value ,name (post-parameters*)))
		  (formlet-return-values (loop for f in (formlets::fields ,name) ;;the values list, less password values
					       for v in formlet-values
					       unless (eq (type-of f) 'password) collect v
						 else collect nil)))
	     (multiple-value-bind (result errors) (validate ,name formlet-values)
	       (if result
		   (apply (formlets::on-success ,name) formlet-values) ;; if everything's ok, send the user on
		   (progn
		     (setf (session-value :formlet-values) formlet-return-values
			   (session-value :formlet-errors) errors
			   (session-value :formlet-name) ',name)
		     (redirect (referer)))))))))))

(defun ensure-list-length (list desired-length)
  (assert (and (integerp desired-length) (< 0 desired-length)))
  (cond ((= (length list) desired-length) list)
	((> (length list) desired-length) (butlast list (- (length list) desired-length)))
	((< (length list) desired-length)
	 (append list (make-list (- desired-length (length list)))))))

(defmacro show-formlet (formlet-name &key default-values)
  "Shortcut for displaying a formlet.
   It outputs the formlet HTML to standard-out (with indenting).
   If this is the last submitted formlet in session, display field values and errors, then clear out the formlet-related session information."
  `(let* ((default-val ,default-values)
	  (val (cond ((eq (session-value :formlet-name) ',formlet-name) 
		      (session-value :formlet-values))
		     (default-val (ensure-list-length default-val (length (formlets::fields ,formlet-name))))
		     (t (make-list (length (formlets::fields ,formlet-name))))))
	  (err (if (eq (session-value :formlet-name) ',formlet-name)
		   (session-value :formlet-errors)
		   (make-list (length (formlets::fields ,formlet-name))))))
     (show ,formlet-name val err)
     (when (eq (session-value :formlet-name) ',formlet-name)
       (delete-session-value :formlet-name)
       (delete-session-value :formlet-values)
       (delete-session-value :formlet-errors))))