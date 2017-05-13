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
     
(remove-if-not #'pred 1st)
(remove-if #'(lambda (x) (not (pred x))) 1st)   (compare x y)
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

;(remove-if-not #'pred 1st)
;(remove-if #'(lambda (x) (not (pred x))) 1st)
;don't think the above 2 were meant to be run examples in chapter 5 of 'on lisp' by Paul Graham
(defvar *!equivs* (make-hash-table))

(defun ! (fn)
(or (gethash fn *!equivs*) fn))

                                        ;slime-repl sbcl* is active right now
(defun def! (fn fn!)
  (setf (gethash fn *!equivs*) fn!))

(setf (get 'ball 'color) 'red)
                                        ;Are we making a hash table out of functions? We're declaring a viriable *!equivs*
(defun joiner (obj)
  (typecase obj
    (cons #'append)
    (number #'+)))
                                        ; 'It takes an object, and, depending on its ty[e, returns a function to add suvh objects together. We could use it to define a polymorphic join function that worked for nunbers or lists:]
(defun join (&rest args)
  (apply (joiners (car args)) args))

(defun make-adder (n) ; 'Calling make-addder will yield a closure whose behavior depends on the value originally given as an argument' - On Lisp Chapter 5
  #'(lambda (x) (+x n))) ;So the value of a function is merely the value assigned to the symbols lexical environment, or perhaps merely the value of the lexical closure, (as opposed to a series of values?)
(funcall add3 2) ; --> 5
                                        ;'Under lexical scope, instead of merely choosing among a group of constant functions, we can build new closures at runtime. With dynamic scope this technique is impossible. Of we consider how complement would be written, we see that it too must return a closure' - On Lisp, Chapter 5

(defun complement (fn)
  #'(lambda (&rest args) (not (apply fn args))))

(defun complement (fn) ;this is a complement of a function directly related to math'*
  #'(lambda (&rest args) (not (apply fn args))))
                                        ;so complement is directly in the source code?
; "Te function returned by complement uses the value of the parameter fn when complement was called. So instead of just choosing from a gorup of constant functions. complemenet can custom build the inverse of any function:"

                                        ;5.2 Orthogonality 'AN orthogonal language is one in which you can express a lot by combining a small number number of operators in a lot of different ways. The main advantage of com,plemenet is that it makes a language more orthogonal. Before complement, COmmon Lisp had pairs of functuions like remove-if and remove-if-not, subst-if, subst-if-not. WIth complement we can do without half of them. The setf macro also improves Lisp's orthogonality. Earlier dialects of Lisp would often have pairs of functions for reading writing data. WIth propertylists, for example, there would be one function to establish properties and another function to ask about them. In Common Lisp, we have only the latter, get. To

(defun compose (&rest fns) ;An operator for functional composition
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
        #'(lambda (*rest args)
            (reduce #'funcall fns
                    :from-end t
                    :initial-value (apply fn1 args))))
      #'identity))
                                        ;Just as the function operator
                                        ;Closures make it possible to define the function composition operator as a lisp function (in real analysis/math)

(compose #'list #'1+);returns a function equivilent to #'(lambda (x) (list (1+x)))

(defun fif (if then *optional else)
  #'(lambda (x)
      (if (funcall if x)
          (funcall then x)
          (if else (funcall else x)))))
(defun fint (fn *rest fns)
  (if (nill fns)
      fn
      (let ((chain (apply #'fint fns)))
        #'(lambda (x)
            (and (funcall fn x) (funcall chain x))))))

(defun fun (fn *rest fns)
  (if (null fns)
      fj
      (let ((chain (apply #'fint fns)))
        #'(lambda (x)))))

                                        ;More function builders
                                        ;Since not is a Lisp function, complement is a special case of compose. It could be defined as:
(defun complement (pred)
  (compose #'not pred))
;Is predicate merely a stand in for any inbound function? Or any function taken as input arguments

                                        ; We can combine functions in other ways than by composing them, FOr example we often see expressions like
(mapcar #'(lambda (x)
            (if (slave x)
                (owner x)
                (employer x)))
        people)
                                        ;We could define an operator to build functions like this one automatically. Using fif from Figure 5. we could get the same effect with:
(mapcar (fif #'slave #'owner #'employer)
        people)

(find-if #'(lambda (x)
             (and (signed x) (sealed x) (delivered x)))
         docs)

(find-if (fint #'signed #'sealed #'delivered) docs)

(defun our-length (1st)
  (if (null 1st)
      0
      (1+ (our-length (cdr 1st)))))
                                        ;'Repeated patterns in a program are a sign that it could have been written at a higher level of abstraction. What pattern seen in Lisp programs than this

(defun our-every (fn 1st)
  (if (null 1st)
      t
      (and (funcall fn  (car 1st))
           (our-every fn (cdr 1st)))))

(defun lrec (rec &optional base)
  (labels ((self (1st)
             (if (null 1st)
                 (if (functionp base)
                     (funcall base)
                     base)
                 
                 (funcall rec (car 1st)
                          #'(lambda ()
                              (self (cdr 1st)))))))
    #'self))

 (lrec #'(lambda (x f) (1+ (funcall f))) 0) ;I need all evals of regions to just print the output, I know something like that exists, I have to go through the entire spheal I think, build up a massive lisp codebase is the only way to learn it and make it interact or do whatever by yourself, it seems impossible to communicate otherwise, and programmer's tend to have low IQ's for some stuff that matters. (Locate a jewish writer for programming)
(lrec #'(lambda (x f) (and (oddp x) (funcall f))) t)
                                        ;copy-list
(lrec #'(lambda (x f) (adjoin x (funcall f))))

                                        ;some, for some reason fn
(lrec #'(lambda (x f) (or (fn x) (funcall f))))
(lrec #'(lambda (x f) (cons x (funcall f))))

                                        ;remove-duplicates
(lrec #'(lambda (x f) (adjoin x (funcall f))))
                                        ;find-if, for some reason fn
(lrec #'(lambda (x f) (if (fn x) x (funcall f))))

                                        ; some, for some function fn
(lrec #'(lambda (x f) (or (fn x) (funcall f))))
                                        ;Right on it worked.

                                        ;Lists can represent sequences,sets,mappings, arrays, trees' - On Lisp

                                        ;Anyways for now I should just program my own lisp editor functions that help with the affair,
                                        ;first I Need to learn how to do the inline-evals
(defun our-copy-tree (tree)
  (if (atom tree)
      tree
      (comns (our-copy-tree (car trree))
             (if cdr tree) (our-copy-tree (cdr tree)))))

(defun count-leaves (tree)
  (if (atom tree)
      1
      (+ (count-leaves (car tree))
         (or (if (cdr tree) (count-leaves (cdr tree)))
             1))))

                                        ;(defun flatten '((a b (c d)) (e) f ()))
(defun flatten (tree)
  (if (atom tree)
      (mklist tree)
      (nconc (flatten (car tree))
             (if (cdr tree) (flatten (cdr tree))))))

(flatten '((a b (c d)) (e) f ()))

(defstruct person (name 007 :type string))
(make-person :name "James")
(make-person) ; 'only the last call is an error' - Common Lisp HyperSpec

(defstruct ship
  (x-position 0.0 :type short-float)
  (y-position 0.0 :type short-float)
  (x-velocity 0.0 :type short-float)
  (y-velocity 0.0 :type short-float)
  (mass *default-ship-mass* :type short-float :read-only t))
(defstruct academic-papers (name 000 :type string))

(make-academic-papers :name "Ricky's > first first paper") ; Impressive, I noticed that make-academic-papers automatically exists as a function name right after i've created a struct called academic-papers


(defstruct personidentity (name 001 :type string))
(make-personidentity :name "Ricky")

                                        ;'In this section and the next we will look at two ways to traverse a network. FIrst we will follow the traditional approach, with nodes defined as structures and separate code to traverse the network. Then in the next section we'll show how to build the same program from a single abstraction. As an example , we will use about the simplest application p[ossible: One of those programs that play twenty questions. Our network will be a binary tree. Each non-leaf node will contain a y/n question, and depending on the answer to the question, the taversal willc ontinue down the left or right subtree. Leaf nodes will contain return values. When the traversal reaches a leaf node, its value will be returned as the value of the taversal. A session with this program might ook as in FIgure 6.1.

                                        ;The traditional way to begin would be to define some sort of data structure to represent nodes. A node is going to have to know several things: whether it is a leaf; if so, which value to return, and if not, which question to ask: and where to go depending on the answer. A sufficient data structure is defined in figure 6.2. or a return value. If the node is not a leaf, the yes or no fields will tell where to go depending on the answer to the question; if the node is a leaf, we will know it because these fields are empty. The global *nodes* will be a hash-table in which bnodes are indexed by name. FInally, defnode makes a new node(of either type) and stores it in *nodes*. Using htese materials we could define the first node of our tree.

;; (defnode 'people "Is the person a man?'
;; 'male 'female)

(defstruct node contents yes no)
(defvar *nodes* (make-hash-table))

(defun defnode (name conts &optional yes no) ;*general* representation and definition of nodes
  (setf (gethash name *nodes*)
        (make-node :contents conts
                   :yes yes
                   :no no)))

(defnode 'people "Is the person a man?" 'male 'female)
(defnode 'male "Is he living?" 'liveman 'deadman)
(defnode 'deadman "Was he American?" 'us 'them)
(defnode 'us "Is he on a coin?" 'coin 'cidence)
(defnode 'coin "Is the coin a penny?" 'penny 'coins)
(defnode 'penny 'lincoln)

(defun run-node (name)
  (let ((n (gethash name *Nodes*)))
    (cond ((node-yes n)
           (format t "~A~%>> " conts)
           (case (read)
             (yes (run-node (node-yes n)))
             (t (run-node (node-no n)))))
          (t (node-contents n)))))

(devar *nodes* (make-hash-table))

(defun defnode (name conts *optional* yes no)
  (setf (gethash name (*nodes*)
                 (if yes
                     #'(lambda ()
                         (format t "~A~%> " conts)
                         (case (read)
                           (yes (funcall (gethash yes *nodes*)))
                           (t (funcall (gethash no *nodes*)))))
                     #'(lambda () conts)))))

;a network compiled into closures, interesting I see no let over lambda, which I thought was the minimum sufficient conditions to achieve lexical closures, perhaps, it is 'setf'! that will achieve this.

                                        ;6.2 Compiling Networks
                                        ;' In the preceding section we wrote a network program as it might have been written in any language. Indeed, the program is so simple that it seems odd to think that we could write it any other way. But we can--in fact, we can write it much ore simply. The code in FIgure 6.5 illustrates this point. It's all we really need to run our network. Instead of havign nodes as data structures and a separate function to traverse them, we represent the nodes as closures. The data formerly contained in the structures get stored in variable bidnings within the closures. Now there is no need for run-node; it is implicit in the nodes themselves. To start the traversal.

(defvar *nodes* nil)

(defun defnode (&rest args)
  (push args *nodes*)
  args)

(defun compile-net (root)
  (let ((node (assoc root *nodes*)))
    (if (null node)
        nil
        (let ((conts (second node))
              (tes (third node))
              (no (fourth node)))
          (if yes
              (let ((yes-fn (compile-net yes))
                    (no-fn (compile-net no)))
                #'(lambda ()
                    (format t "~A~%>> " conts)
                    (funcall (if (eq (read) 'yes)
                                 yes-fn
                                 no-fn))))
              #'(lambda () conts))))))
(funcall (gethash 'people *nodes*))
                                        ;'6.6 Compilation with static references'
                                        ;'defined, we call compile-net to compile a whole network at once. This function recursively works its way right  down to the leaves of the tree, and on the way back up, returns at each step the node/function for each of the two subtrees. So now each node will have a direct handle pon its two destinations, instead of having only their names. When the original call to compile-net returns, it will yield a function representing the portion of the network we asked to have compiled'[Paul Graham]

(defmacro nil! (var) ; THis definition tells lisp: "Whenever you see an expression of the form (nil! var), turn it into one of the form (setq var nil) before evaluating it"
  (list 'setq var nil))
                                        ;'The expression generated by the macro will be evaluated in place of the original macro call. A macro call is a list whse first element is the name of a macro. What happens when we type the macro call (nil! x) into the toplevel? Lisp notices that nil! is the name of a macro, and 1. builds the expression specified by the defintion above, then 2. evaluates that expression in place of the original macro call' [Paul Graham]
