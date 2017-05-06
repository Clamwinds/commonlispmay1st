The function COMMON-LISP-USER::X is undefined.
   [Condition of type UNDEFINED-FUNCTION]

Restarts:
 0: [CONTINUE] Retry calling X.
 1: [USE-VALUE] Call specified function.
 2: [RETURN-VALUE] Return specified values.
 3: [RETURN-NOTHING] Return zero values.
 4: [RETRY] Retry SLIME interactive evaluation request.
 5: [*ABORT] Return to SLIME's top level.
 --more--

Backtrace:
  0: ("undefined function" 2)
  1: ((LAMBDA (X Y)) 1 2)
  2: (SB-INT:SIMPLE-EVAL-IN-LEXENV (APPLY (FUNCTION (LAMBDA # # #)) (QUOTE (1 2))) #<NULL-LEXENV>)
  3: (EVAL (APPLY (FUNCTION (LAMBDA # # #)) (QUOTE (1 2))))
 --more--
