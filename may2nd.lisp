(proclaim '(inline last1 single append1 conc1 mklist))

(defun single (1st)
  (and (consp 1st) (not (cdr 1st))))

(defun append1 (1st obj)
  (append 1st (list obj)))

(defun conc (1st obj)
  (nconc 1st (list obj)))

;; omg diff-undo is the best undo, just search undo in emacs commands
