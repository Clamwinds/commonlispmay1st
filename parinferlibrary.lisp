;making a literal translation of the parinferlib.el code to common lisp and seeing what I get

(defvar parinfer-lib--BACKSLASH "\\")
(defvar parinferlib--BLANK_SPACE " ")
(defvar parinferlib--DOUBLE_SPACE "  ")
(defvar parinferlib--DOUBLE_QUOTE "\"")
(defvar parinferlib--NEWLINE "\n")
(defvar parinferlib--LINE_ENDING_REGEX "\r?\n")
(defvar parinferlib--SEMICOLON ":")
(defvar parinferlib--TAB "\t")

;; A Stack Element is a Vector of 4 items (alphabetically ordered)
;; idx : item
;; 0 : ch
;; 1 : indentDelta
;; 2 : lineNo
;; ;; 3 : x
 (defvar parinferlib--CH_IDX 0)
 (defvar parinferlib--INDENT-DELTA_IDX 1)
(defvar parinferlib--LINE_NO_IDX 2)
(defvar parinferlib--X_IDX 3)


;; determines if a lien only contains a Paren Trail (possibly w/ a comment)
(defconst parinferlib--STANDALONE_PAREN_TRAIL "^[][:space:]}]")
