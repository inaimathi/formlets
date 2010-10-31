Formlets
========

An implementation of self-validating formlets for Hunchentoot.

Goals
-----

### Boilerplate elimination
At the high level, form interaction in HTML requires 
1. Showing the user a form
2. Get the response back
3. Run a validation function per form field (or run a single validation function on all of the fields)
4. If the validation passed, send them on, otherwise, show them the form again (but annotated to highight errors they need to correct)
and I don't want to have to type it all the time.

### Simplicity
A declaration and `show-formlet` call is all that should be required to display, validate and potentially re-display a form.

### Style
Automatically wraps the generated form in a UL and provides CSS classes and ids as hooks for the designers, making the look and feel easily customizable.

### Completeness
The system will eventually support the full complement of HTML form elements, including `select`, `checkbox` and `radio`, as well as higher-level inputs like `date` or `slider`. Currently, it only supports `password`, `text`, `textarea` and `recaptcha`.

Non-Goals
---------

### Portability
The system assumes Hunchentoot + cl-who. This allows the internal code to take advantage of HTML generation, as opposed to tag `format`ting.

### Run-time efficiency
The module is aimed at simplifying HTML form use for the developer. This is a place that's by definition bound to user and network speeds. Furthermore, a single form is very rarely more than 20 inputs long in practice. Pieces will be made efficient where possible, but emphasis will not be placed on it.

### Markup customization
While there are no assumptions about the CSS, formlet HTML markup is fixed by the implementation. A user can modify the `show-form-field` function to change how inputs are output, but this will not be made customizable by external variables.

Usage
-----

An example form declaration using general validation:

	(def-formlet login 
	    (login-page ((user-name :text) (password :password)) :submit "Login"
			:general (#'check-password "You did not enter the correct user name or password"))
	  (start-session)
	  (setf (session-value :user-name) user-name)
	  (setf (session-value :user-id) (check-password user-name password))
	  (redirect "/profile"))

In this case, the `check-password` function will be evaluated. If it returns `t`, a session is started and the user is redirected to `/profile`. If it returns `nil`, the user will be sent to `login-page`. The fields in this formlet are `user-name` (a standard text input), and `password` (a password input). The submit button will read "Login" (by default, it reads "Submit").

You would display the above formlet as follows:

	(define-easy-handler (login-page :uri "/") (form-values form-errors)
	  (form-template (show-login-form form-values form-errors)))

The function `show-login-formlet` is declared as part of the `def-formlet` call above. Calling it with `form-values` and `form-errors` causes the full HTML of the formlet to be generated. If `form-values` contains any appropriate values, they will be displayed as default form values (this doesn't happen for passwords or recaptcha fields). If `form-errors` contains any appropriate values, they will be displayed alongside the associated input.

An example form using individual input validation:

	(def-formlet register 
	    (register-page 
	     ((user-name :text (lambda (f) (and (not (equalp "" f)) (not (user-exists? f)))) "That name has already been taken")
	      (password :password (lambda (f) (< 4 (length f))) "Your password must be longer than 4 characters")
	      (confirm-password :password (lambda (f) (equalp f password)) "You must enter the same password in 'confirm password'")
	      (captcha :recaptcha))
	     :submit "Register")
	  (let ((id (register user-name password)))
	    (start-session)
	    (setf (session-value :user-name) user-name)
	    (setf (session-value :user-id) id)
	    (redirect "/profile")))

You'd display this the same way as above, and the same principles apply. The only difference is that, instead of a single function needing to pass, there is now a series. In this case, it's a series of 4 (recaptchas are always validated the same way, so that was coded in the formlet module itself). If all of them pass, the user is redirected to `/profile`, otherwise a list of errors and user inputs is returned to `register-page`.

A single field looks like this
	(field-name :field-type validation-function "Error message")

+ The field name is used to generate a label, CSS id and name for the form field. 
+ The type signifies what kind of input will be displayed (currently, the system supports only `:text`, `:password`, `:textarea` or `:recaptcha`. A special note, in order to use the `:recaptcha` input type, you need to `setf` the `*private-key*` and `*public-key*` as appropriate for your recaptcha account.
+ A validation function and error message can be provided optionally. If they aren't, the field won't be validated. If they are, then the function will be applied to the users' response. If the application fails, the error message will be pushed onto `form-errors`.

A formlet declaration breaks down as

	(def-formlet [formlet name]
	    ([source function]
	     ([list of fields])
	     :submit [submit button caption]
	     :general ([general validation function] 
                       [error message])
	     [on success])

+ Formlet name is used to generate the CSS id and name of the form, as well as determine the final name of this formlets' `show-[name]-formlet` function.
+ If the formlet fails validation, it will redirect the user to `[source function]` (the provided function must be expecting the `form-values` and `form-errors` arguments)
+ The list of fields should be one or more form fields as defined above
+ Submit button caption is just the text that will appear on this formlets' submit button. By default, it is "Submit"
+ The general validation function will attempt to validate the form as a whole, instead of field-by-field. If it fails, it will display `[error message]` as a field-independant error at the top of the formlet.`:general` defaults to `NIL`, and if it is provided, the individual field validation functions will be ignored.
+ Finally, `[on success]` is a body parameter that determines what to do if the form validates properly
