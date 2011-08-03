(defpackage :formlets-test-system (:use :cl :asdf))
(in-package :formlets-test-system)
(defsystem formlets-test
  :version "0.1"
  :author "leo.zovic@gmail.com"
  :maintainer "leo.zovic@gmail.com"
  :licence "MIT-style"
  :description "Testing system to simplify development of the validating formlets system for Hunchentoot"
  :components ((:file "test-package")
	       (:file "test" :depends-on ("test-package")))
  :depends-on (:cl-who :drakma :hunchentoot :cl-ppcre :formlets))