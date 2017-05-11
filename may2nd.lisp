(proclaim '(inline last1 single append1 conc1 mklist))
(defun single (1st)
  (and (consp 1st) (not (cdr 1st))))

(defun append1 (1st obj)
  (append 1st (list obj)))

(defun conc1 (1st obj)
  (nconc 1st (list obj)))

(defun mklist (obj)
  (if (listp obj) obj (list obj)))
;; omg diff-undo is the best undo, just search undo in emacs commands

                                        ;So obviously
;(mapcan #`(lambda (d) (mklist lookup d))) data)


(defun longer (x y)
  (labels ((compare (x y)
             (and (consp x)
                  (or (null y)
 
                     (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
        (compare x y)
        (> (length x) (length)))))

(defun filter (fn 1st)
  (let ((acc nil))
    (dolist (x 1st)
      (let ((val (funcall fn x)))
       0 (if val (push val acc))))
    (nreverse acc)))

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rest source nil) nil)))
                                        ; for some reason I cannot push my repo so I need to commit a ton of lines of code today and
                                        ;work on my resume
(+ 1 2)

(apply #'(lambda (x y) (+ x y) (+ x y)) '(1 2))

                                        ; make a graphical dashboard of every function you write and get to writing some programs that will personally help you
                                        ; ie i see paul graham's personal macros or defun's or utilities but what utilities would help me?

                                        ;To grok programming you need to write clones or popular websites until you're comfortable that your website dynamic encapsulates the other website's fu1nctionality. Like just backtest a tor web site locally as much as possible. I just need to write more software and write more in general. I need to toil for several years.

(funcall #'+ 1 2)

(defun behave (animal)
  (case animal
  (dog (wag-tail)
       (bark))
  (rat (scurry)
       (squeak))
  (cat (rub-legs)
       (scratch-carpet))))

(defun behave (animal)
  (funcall (get animal 'behavior)))

(setf (get 'dog 'behavior)
      #'(lambda ()
          (wag-tail)
          (bark)))

(let ((counter 0))
  (defun new-id ()     (incf counter))
  (defun reset-id () (setq counter 0)))


(defun list+ (1st n)
  (mapcar #'(lambda (x) (+ x n))
          1st))
(list+ '(1 2 3) 10)

(let ((counter 0))
  (defun new-id ()    (incf counter))
  (defun reset-id () (setq counter 0)))

(defun make-adder (n)
  #'(lambda (x) (+ x n)))

                                        ; Make a program that has all your amazon books ranked and sorted by which one you would like the most, just weight the average of the scores to what your satisfaction would be.

(lambda (x) (* x 2))

(defun longer (x y)
  (labels ((compare (x y)
             (and (consp x)
                  (or (null y)
                      (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
        (compare x y)
        (> (length x) (length y)))))

(defun filter (fn 1st)
  (let ((acc nil))
    (dolist (x 1st)
      (let ((val (funcall fnx)))
        (if val (push val acc))))
    (nreverse acc)))

(defun group (source n)
  (if (zerop n) (error "zero length"))
(labels ((rec (source acc)
           (let ((rest (nthcdr n source)))
             (if (consp rest)
                 (rec rest (cons (subseq source 0 n) acc))
                 (rreverse (cons source acc))))))
  (if source (rec source nil) nil)))

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))


(defun prune (test tree)
  (labels ((rec (tree acc)
             (cond ((null tree) (nreverse acc))
                   ((consp (car tree))
                    (rec (cdr tree)
                         (cons (rec (car tree) nil) acc)))
                   (t (rec (cdr tree)
                           (if (funcall test (car tree))
                               acc
                               (cons (car tree) acc)))))))
    (rec tree nil)))
   
;; I need to make a set of programs that gives me information on my credit card and financial situation until I get a good grip of this thing.

                                        ;flatten returns a list of all atoms
(defun find2 (fn 1st)
  (if (null 1st)
      nil
      (let ((val funcall fn (car 1st))))
  (if val
      (values (car 1st) val)
      (find2 fn (cdr 1st)))))


(flatten `(a (b c) ((d e) f)))
                                        ; flatten seems to converge all lists into one big list (A B C D E F)

;(prune #'evenp '(1 2 (3 (4 5) 6) 7 8 (9)))
;(1 (3 (5)) 7 (9))

;/(prune #'evenp '(1 2 (3 (4 5) 6) 7 8 (9)))
                                        ;(1 (3 (5)) 7 (9))

(defun before (x y 1st &key (test #'eql))
  (and 1st
       (let ((first (car 1st)))
        (cond ((funcall test y first) nil)
              ((funcall test x first) 1st)
       (t (before x y (cdr 1st) :test test))))))


(defun after (x y 1st &key (test #'eql))
  (let ((rest (before y x 1st :test test)))
    (and rest (member x rest :test test))))


(defun duplicate (obj 1st &key (test #'eql))
  (member obj (cdr (member obj 1st :test test))
          :test test))

(defun split-if (fn 1st)
  (let ((ac nil))
    (do ((src 1st (cdr src)))
        ((or (null src) (funcall fn (car src)))
         (values (nreverse acc) src))
      (push (car src) acc))))

(after 'a 'b '(b a d))
                                        ; What does a quoted list do? Check if it's a symbol?
; What does a ' quote operator do?
(Before 'a 'b '(a))

(defun most(fn 1st)
  (if (null 1st)
      (values nil nil)
      (let* ((wins (car 1st))
             (max (funcall fn wins)))
        (dolist (obj (cdr 1st))
          (let ((score (funcall fn obj)))
            (when (> score max)
              (setq wins obj
                    max score))))
        (values wins max))))

(defun best (fn 1st)
  (if (null 1st)
      nil
      (let ((wins (car 1st)))
        (dolist (obj (cdr 1st))
          (if (funcall fn obj wins)
              (setq wins obj)))
        wins)))

(defun mustn (fn 1st)
  (if (null 1st)
      (values nil nil)
      (let ((result (list (car 1st)))
            (max (funcall fn (car 1st))))
        (dolist (obj (cdr 1st))
          (let ((score (funcall fn obj)))
            (cond ((> score max)
                   (setq max     score
                         result (list obj)))
                  ((= score max)
                   (push obj result)))))
        (values (nreverse result) max))))

(defun max0-n (fn n)
  (mapa-b fn 0 n))

(defun map1-n (fn n)
  (mapa-b fn 1 n))

(defun mapa-b (fn a b &optional (step 1))
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (piush (funcall fn i) result)))

(defun map-> (fn start test-fn succ-fn)
  (do ((i start (funcall succ-fn i))
       (result nil))
      ((funcall test-fn i)  (nreverse result))
    (push (funcall fn i) result)))

(defun mapcars (fn &rest 1sts)
  (let ((result nil))
    (dolist (1st 1sts)
      (dolist (obj 1st)
        (push (funcall fn obj) result)))
    (nreverse result)))

(defun rmapcar (fn &rest arg)
  (if (some #'atom args)
      (apply fn args)
      (apply fn args)
      (apply #'mapcar
             #'(lambda (&restult arg)
                 (apply #'rmapcar fn args))
             args)))

(defun our-mapcan (fn &rest 1sts)
  (apply #'nconc (apply #'mapcar fn 1sts)))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun reread (*rest args)
  (values (read-from-string (apply #'mkstr args))))

(defun explode (sym)
  (map 'list #'(lambda (c)
                 (intern (make-string 1
                                      :initial-element  c)))
       (symbol-name sym)))

(remove-if-not #'pred 1st)
(remove-if #'(lambda (x) (not (pred x))) 1st)
;don't think the above 2 were meant to be run examples in chapter 5 of 'on lisp' by Paul Graham
