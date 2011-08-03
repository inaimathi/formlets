(in-package :formlets-test)

(setf formlets:*public-key* "your public key"
      formlets:*private-key* "your private key"
      *show-lisp-errors-p* t)

(defmacro page-template ((&key title) &body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t :indent t)
     (:html :xmlns "http://www.w3.org/1999/xhtml" :xml\:lang "en" :lang "en"
	    (:head (:meta :http-equiv "Content-Type" :content "text/html;charset=utf-8")
		   (:title ,@title))
	    (:body ,@body))))

(define-formlet (test-form) 
    ((one text 
	  :validation
	  ((not-blank?) "This field cannot be left empty"
	   (mismatches? "blah") "You can't write \"blah\" here"
	   (longer-than? 5) "You need to enter more than 5 characters"))
     (two textarea)
     (three password)
     (recaptcha recaptcha))
  (page-template (:title "Results")
    (:h4 "Fuck yeah!")
    (:p (str (write-to-string (post-parameters*))))
    (:p (str one))
    (:p (str two))
    (:p (str recaptcha))))

(define-formlet (test-form-two)
    ((one text :validation ((lambda (v) (> 6 (length v))) "You can't enter more than 5 characters here"))
     (two file)
     (pull-down select :value-set ("one" "two" "three" "four"))
     (radio-set radio-set :value-set ("one" "two" "three" "four"))
     (password password)
     (confirm-password password :validation ((same-as? "password") "You have to enter the same thing here and in the 'Password' field")))
  (page-template (:title "Results TWO")
    (:h4 "Wooo!")
    (:p (str (write-to-string (post-parameters*))))
    (:p (str one))
    (:p (str pull-down))
    (:p (str radio-set))
    (:p (str two))
    (:p (str password))))

(define-formlet (test-form-three)
    ((pull-down multi-select 
		:value-set ("a" "b" "c" "d")
		:validation ((lambda (val) (= 2 (length val))) "Please pick exactly two options"))
     (radio-set radio-set :value-set ("one" "two" "three" "four"))
     (single-check checkbox)
     (checking checkbox-set :value-set ("aye" "bee" "sea" "dee")))
  (page-template (:title "Results TWO")
    (:h4 "Yay!")
    (:p (str (write-to-string (post-parameters*))))
    (:p (str pull-down))
    (:p (str radio-set))
    (:p (str single-check))
    (:p (str checking))))

(define-formlet 
    (faux-login-form 
     :submit "Login" 
     :general-validation ((lambda (user-name password) (and (string= "blah" user-name) (string= "pass" password)))
			  "I see what you did there. &#3232;_&#3232;"))
    ((user-name text) (password password))
  (page-template (:title "You got it")
    (:p (str (write-to-string (post-parameters*))))
    (:p (str user-name))
    (:p (str password))))

(define-easy-handler (test-page :uri "/") ()
  (page-template (:title "Formlets Test Page")
    (:p (str (session-value :formlet-name)))
    (:p (str (session-value :formlet-values)))
    (:p (str (session-value :formlet-errors)))
    (:hr) (show-formlet test-form)
    (:hr) (show-formlet test-form-two)
    (:hr) (show-formlet test-form-three)
    (:hr) (show-formlet faux-login-form)))

(defvar *web-server* (start (make-instance 'acceptor :port 4141)))