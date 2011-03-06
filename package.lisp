(defpackage :formlets 
  (:use :cl :cl-who :hunchentoot)
  (:import-from :cl-ppcre :regex-replace-all :split)
  (:import-from :drakma :http-request)
  (:export :def-formlet
	   :recaptcha :recaptcha-passed? :*private-key* :*public-key*
	   :file-type? :file-smaller-than?)
  (:documentation "A package implementing auto-validating formlets for Hunchentoot"))