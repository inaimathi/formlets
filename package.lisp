(defpackage :cl-formlets 
  (:nicknames "formlets")
  (:use :cl :cl-who :hunchentoot)
  (:import-from :cl-ppcre :regex-replace-all)
  (:export :def-formlet)
  (:documentation "A package implementing auto-validating formlets for Hunchentoot"))