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

(mapcan #'(lambda (d) (mklist (lookup d)))
        data)

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
        (if val (push val acc))))
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
