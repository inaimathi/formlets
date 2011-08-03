(defpackage :formlets 
  (:use :cl :cl-who :hunchentoot)
  (:import-from :cl-ppcre :regex-replace-all :split :scan)
  (:import-from :drakma :http-request)
  (:export :formlet :formlet-field 
	   :text :textarea :password :file :checkbox :select :radio-set :checkbox-set :multi-select
 	   :*public-key* :*private-key* :recaptcha
	   :validate :show :post-value :show-formlet :define-formlet
	   :longer-than? :shorter-than? :matches? :mismatches? :file-type? :file-smaller-than? :not-blank? :same-as? :picked-more-than? :picked-fewer-than? :picked-exactly?)
  (:documentation "A package implementing auto-validating formlets for Hunchentoot"))