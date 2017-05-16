                                        ;Table 5-1 HS: Names of All COmmon Lisp Special FOrms
                                        ;block, catch, [compiler-let] declare eval-when flet function go if  labels let let* macrolet multiple-value-call multiple-valiue=prog1 , progn, progv quote return-from setqtagbody the throw unwind-protect

                                        ;A Lambda-expressionisa list with the following syntax:

(defun discriminant (a b c)
    (declare (number a b c))
  (- (* b b) (* 4 a c)))

(discriminant 1 2/3 -2) ; ;do real-time comment annotation of your code by what it evals to so that would automaticalky become 76/9 (8.44445)

;I should just program all my lienar algebra and multivariable calc exercises in mathematica,I need to just get better at math through there.

(defclass rectangle ()
  ((height :initform 0.0 :initarg :height)
   (width :initform 0.0 :initarg :width)))

(defclass color-mixin ()
  ((cyan :initform 0 :initarg :cyan)
   (megenta :initform 0 :initarg :magenta)
   (yellow :initform 0 :initarg :yellow)))

                                        ;(setf (macro-function 'k) 'fn)

                                        ;
(defmacro airthmetic-if (test neg-form
                         &optional zero-form)
  (let ((var (gensym)))
    '(let ((.var .test)))
            (cond ((< .var 0) .neg-form)
                  ((+ .var 0) .zero-form)
                  (t .pos-form))))
(setconstant x 0)
(arithmetic-if (- x 4.0) (print x))

(defmacro our-when (test &body body)
  `(if ,test
        (Progn
          ,@body)))

(defmacro our-when (test &body body) ; I still don't get what &body or &rest do I know it's there for an unlimited form of arguments, so you put it there when you want an undetermined # of arguments, ok that's working hypothesis #1
  `(if ,test
       (progn
         ,@body)))

                                        ;

it's called the &body parameter identical to &Rest cept for pretty print
(defparameter x 6)
(member x choices :test #'eq)
(memq x choices)
