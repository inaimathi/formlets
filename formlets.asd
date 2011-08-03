;;; -*- Mode: Lisp -*-

;; Copyright (c) 2010 Leo Zovic

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

(defpackage :formlets-system (:use :cl :asdf))
(in-package :formlets-system)
(defsystem formlets
  :version "0.3"
  :author "leo.zovic@gmail.com"
  :maintainer "leo.zovic@gmail.com"
  :licence "MIT-style"
  :description "Validating formlets for Hunchentoot"
  :components ((:file "package")
	       (:file "utility" :depends-on ("package"))
	       (:file "formlets" :depends-on ("package" "utility"))
	       (:file "recaptcha" :depends-on ("package" "utility" "formlets"))
	       (:file "macros" :depends-on ("package" "utility" "formlets")))
  :depends-on (:cl-who :drakma :hunchentoot :cl-ppcre))