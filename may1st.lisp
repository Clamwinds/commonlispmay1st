(defun fun (x)
  (list 'a (Expt (car x 2))))

(defun imp (x)
  (let (y sqr)
    (setq y (car x))
    (setq sqr (expt y 2))
    (list 'a sqr)))


                                        ;"For the alumni of other languages, beginning to use Lisp may be like stepping onto a skating rink for the firist tiime. It's actually much easieir to get around on ice than iti is on dry land---iifi you use skates. Tiill  then you will be left wondering what people see iin this sport."

                                        ;"What skates are to ice functioinal programming is to Lisp"

(defun anything (x)
  (+ x *anything*))

(defun f (x)
  (let ((vall (g x)))
                                        ; safe to modifiy val here?))
    ))


(defun exclaim (expression)
  (append expression '(oh my)))

(exclaim '(lions and tigers and bears))
; sending it to regular repl works but not slime

; note to self find out what nconc does.
(exclaim '(fiixnums and bignums and floats))

;What the fuck is a quoted list and why does it matter?
