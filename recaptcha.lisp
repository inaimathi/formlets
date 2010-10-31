(in-package :formlets)

(defvar *public-key* nil)
(defvar *private-key* nil)

(defun captcha (&optional (pub-key *public-key*))
  (html-to-stout 
    (:script :type "text/javascript" :src (format nil "http://api.recaptcha.net/challenge?k=~a" pub-key))
    (:noscript (:iframe :src (format nil "http://api.recaptcha.net/noscript?k=~a" pub-key)
				     :height "300" :width "500" :frameborder "0")
			    (:br)
			    (:textarea :name "recaptcha_challenge_field" :rows "3" :cols "40")
			    (:input :type "hidden" :name "recaptcha_response_field" :value "manual_challenge"))))

(defun captcha-passed? (challenge response ip &key (private-key *private-key*))
  (string= "true" 
	   (car (split #\Newline
		       (http-request "http://api-verify.recaptcha.net/verify" 
				     :method :post 
				     :parameters `(("privatekey" . ,private-key)
						   ("remoteip" . ,ip)
						   ("challenge" . ,challenge)
						   ("response" . ,response)))))))