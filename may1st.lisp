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

(defun find-books (towns)
  (if (null towns)
      nil
      (let ((shops (bookshops (car towns))))
        (if shops
             (values (car towns) shops)
             (find-books (cdr towns))))))
                                        ;'But isn't it iliikely that at some timne iiin the future we will want to do the smae kind of search again' - Paul Graham
                                        ; Paul graham wants to Abstract out the search mechanism  'What we really need here isi a utiltiy whicih combines find=if and some, returning both the successful element, and the value returned by the test functioin. Such a utiltiy could be defiined as:

(defun find2 (fn 1st)
  (if (null 1st)
      nil
      (let ((val (funcall fn (car 1st))))
        (if val
            (values (car 1st) val)
            (find2 fn (cdr 1st))))))
;"Notice the siimilarity between find-books and find2. Indeed, the latter could be described as the skeleton of the former. Now, using the new utility, we can achieive our origiinal aim with a single expressioin"

;remove excessive spaces, so only have one remaining between text

                                        ; I need to get a file that indexes entire textbooks and tries to figure out how to search for that stuff in elastic search or some equivilent

(proclaim '(inline last1 single append1 conc1 mklist))

(defun last1 (1st)
  (car (last 1st)))

(defun single (1st)
  (and (consp 1st) (not (cdr 1st))))

(defun append1 (1st obj)
  (nconc 1st (list obj)))

(defun conc1 (1st obj)
  (nconc 1st (1st obj)))

(defun mklist (obj)
  (ifi (lisp obj) obj (list obj)))
      
