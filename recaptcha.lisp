(in-package :formlets)

(defvar *public-key* nil)
(defvar *private-key* nil)

(defclass recaptcha (formlet-field) ())

(defun recaptcha-passed? (challenge response ip &optional (private-key *private-key*))
  (string= "true" 
	   (car (split #\Newline
		       (http-request "http://api-verify.recaptcha.net/verify" 
				     :method :post 
				     :parameters `(("privatekey" . ,private-key)
						   ("remoteip" . ,ip)
						   ("challenge" . ,challenge)
						   ("response" . ,response)))))))

(defmethod validate ((field recaptcha) values)
  "A reCaptcha, being a foreign API call, is validated in a completely different way"
  (declare (ignore values))
  (let* ((result (recaptcha-passed? (post-parameter "recaptcha_challenge_field")
				    (post-parameter "recaptcha_response_field")
				    *private-key* ))
	 (errors (unless result (list "You seem to have mistyped the reCaptcha"))))
    (values result errors)))

(defmethod show ((field recaptcha) &optional v error)
  (declare (ignore v))
  (html-to-str 
    (:li :class (string-downcase (name field))
	 (:span :class "label")
	 (when error 
	   (htm (:span :class "formlet-error"
		       (dolist (s error) 
			 (htm (:p (str s)))))))
	 (:script :type "text/javascript" :src (format nil "http://api.recaptcha.net/challenge?k=~a" *public-key*))
	 (:noscript (:iframe :src (format nil "http://api.recaptcha.net/noscript?k=~a" *public-key*)
			     :height "300" :width "500" :frameborder "0")
		    (:br)
		    (:textarea :name "recaptcha_challenge_field" :rows "3" :cols "40")
		    (:input :type "hidden" :name "recaptcha_response_field" :value "manual_challenge")))))