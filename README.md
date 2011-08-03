Formlets
========

An implementation of self-validating formlets for Hunchentoot.

News
----

- big codebase re-organization (it now makes liberal use of classes and methods to reduce complexity)
  - the declaration format has changed (for the better, hopefully, but you still need to change it slightly)
  - `show-formlet` is now a function that takes a formlet name and no magic variables
  - validator functions now automatically redirect to the referer, so you don't need to specify a `source-function`
  - validator functions now communicate via the `session` instead of calling back to the previous function
- support added for inputs of type `checkbox`, `checkbox-set`, `radio-set`, `select` and `multi-select`
- added new predicates `not-blank?`, `same-as?`, `picked-more-than?`, `picked-fewer-than?` and `picked-exactly?`
- multiple formlets can now co-exist on one page

Goals
-----

### Boilerplate elimination
At the high level, form interaction in HTML requires 

1. Showing the user a form
2. Getting the response back
3. Running a validation function per form field (or run a single validation function on all of the fields)
4. If the validation passed, sending them on, otherwise, showing them the form again (annotating to highight errors)

and I don't want to have to type it out all the time.

### Simplicity
A `define-formlet` and `show-formlet` call is all that should be required to display, validate and potentially re-display a form as many times as necessary.

### Style
Automatically wraps the generated form in a UL and provides CSS classes and ids as hooks for the designers, making the look and feel easily customizable with an external stylesheet.

### Completeness
The system will eventually support the full complement of HTML form elements, like `hidden`, as well as higher-level inputs, like `date` or `slider`. Currently, it only supports `password`, `text`, `textarea`, `file`, `checkbox` (and `checkbox-set`), `radio-set` (a stand-alone radio button is kind of pointless), `select` (and `multi-select`) and `recaptcha`.

Semi-Goals
---------

### Portability
The system assumes Hunchentoot + cl-who. This allows the internal code to take advantage of HTML generation, as opposed to tag `format`ting, and make use of `post-parameters*` and the Hunchentoot `session`. That said, porting away from cl-who would only involve re-defining the `show` methods, and porting away from Hunchentoot would involve re-writing the `define-formlet` and `show-formlet` macros to accomodate another `session` and `POST` model. I have no experience with this, but patches welcome.

### Run-time efficiency
The module is aimed at simplifying HTML form use for the developer. This is a place that's by definition bound by the slower of user speed or network speed. Furthermore, a single form is very rarely more than 20 inputs long in practice. Pieces will be made efficient where possible, but emphasis will not be placed on it.

### Markup customization
While there are no assumptions about the CSS, formlet HTML markup is fixed in the `show` methods. You can go in and re-define all the `show`s, but that's about as easy as markup customization is going to get.


All that said, I have no experience working with CL servers other than hunchentoot, and `formlets` is as fast as I need it to be at the moment, so if you'd like to change any of the above things, patches welcome.

Usage
-----

### Predicates

Formlets now includes a number of predicate generators for external use. These cover the common situations so that you won't typically have to pass around raw `lambdas`. They all return predicate functions as output.

The following four are pretty self explanatory. Longer/shorter checks the length of a string. `matches?` passes if the given regex returns a result for the given input, and `mismatches?` is the opposite. `not-blank?` makes sure that a non-"" value was passed, and `same-as?` checks that the field value is `string=` to the specified value.

+ `longer-than?` :: Num -> (String -> Bool)
+ `shorter-than?` :: Num -> (String -> Bool)
+ `matches?` :: regex -> (String -> Bool)
+ `mismatches?` :: regex -> (String -> Bool)
+ `not-blank?` :: (String -> Bool)
+ `same-as?` :: field-name-string -> (String -> Bool)

The file predicates expect a [hunchentoot file tuple](http://weitz.de/hunchentoot/#upload) instead of a string, but act the same from the users' perspective. `file-type?` takes any number of type-strings and makes sure that the given files' content type matches one of them. You can find a list of common mimetypes [here](http://www.utoronto.ca/web/htmldocs/book/book-3ed/appb/mimetype.html). It doesn't rely on file extensions. `file-smaller-than?` takes a number of bytes and checks if the given file is smaller.

+ `file-type?` :: [File-type-string] -> (FileTuple -> Bool)
+ `file-smaller-than?` :: Size-in-bytes -> (FileTuple -> Bool)

Finally, the newly added set-predicates expect a list of values as input from the given field (these can only be used on `multi-select` boxes and `checkbox-set`s). They ensure that the number of returned values is (greater than|less than|equal to) a specified number.

+ `picked-more-than?` Num -> ([String] -> Bool)
+ `picked-fewer-than?` Num -> ([String] -> Bool)
+ `picked-exactly?` Num -> ([String] -> Bool)

### Tutorial

To see some example code, check out the `test.lisp` file (to see it in action, load the `formlets-test` system). An example form declaration using a general validation message:

	(define-formlet (login :submit "Login" :general-validation (#'check-password "I see what you did there. &#3232;_&#3232;"))
	    ((user-name text) (password password))
	  (start-session)
	  (setf (session-value :user-name) user-name)
	  (setf (session-value :user-id) (check-password user-name password))
	  (redirect "/profile"))

If the validation function returns `t`, a session is started and the user is redirected to `/profile`. Otherwise, the user will be sent back to the previous page, and a general error will be displayed just above the form. The fields in this formlet are `user-name` (a standard text input), and `password` (a password input). The submit button will read "Login" (by default, it reads "Submit").

You would display the above formlet as follows:

	(define-easy-handler (login-page :uri "/") ()
	  (form-template (show-formlet login)))

An instance of the `formlet` named `login` is created as part of the `define-formlet` call above. Calling `show-formlet` with the appropriate formlet name causes the full HTML of the formlet to be generated. If any values appropiate for this formlet are found in session, they will be displayed as default form values (passwords and recaptcha fields are never stored in session, so even if you redefine the `password` `show` method to display its value, it will not). If any errors appropriate for this formlet are present, they are `show`n alongside the associated input.

An example form using individual input validation:

	(def-formlet (register :submit "Register")
	     ((user-name text :validation ((not-blank?) "You can't leave this field blank"
                                           #`unique-username? "That name has already been taken"))
	      (password password :validation (longer-than? 4) "Your password must be longer than 4 characters")
	      (confirm-password password :validation ((same-as? "password") "You must enter the same password in 'confirm password'"))
	      (captcha recaptcha))
	  (let ((id (register user-name password)))
	    (start-session)
	    (setf (session-value :user-name) user-name)
	    (setf (session-value :user-id) id)
	    (redirect "/profile")))

You'd display this the same way as above, and the same principles apply. The only difference is that, instead of a single error being displayed on a validation failure, one is displayed next to each input. In this case, it's a series of 4 (recaptchas are the odd duck; they have their very own `validate` method, which you can see in `recaptcha.lisp`, so no additional declaration is needed). If all of them pass, the user is redirected to `/profile`, otherwise a list of errors and user inputs is returned to `register-page`.


A single field declaration looks like this (the `validation` parameter is a list of `((predicate-function error-message) ...)`

        (field-name field-type &key size value-set default-value validation)

+ The field name is used to generate a label and name for the form field. 
+ The type signifies what kind of input will be displayed (currently, the system supports `text`, `textarea`, `password`, `file`, `checkbox`, `select`, `radio-set`, `multi-select`, `checkbox-set` or `recaptcha`. A special note, in order to use the `recaptcha` input type, you need to `setf` the `formlets:*private-key*` and `formlets:*public-key*` as appropriate for your recaptcha account.

A formlet declaration breaks down as

        ((name &key general-validation (submit "Submit")) (&rest fields) &rest on-success)

+ `name` is used to generate the CSS id and name of the form, as well as determine the final name of this formlets' instance and validation handler.
+ `fields` should be one or more form fields as defined above
+ `submit` is just the text that will appear on this formlets' submit button
+ If the `general-validation` is present, it will be displayed above the form in the event of an error (and none of the individual warnings will be shown). This is useful for places like login forms, where you don't want to tell potential attackers which fields they got wrong.
+ Finally, `on-success` is a body parameter that determines what to do if the form validates properly
