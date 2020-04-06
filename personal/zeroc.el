
(define-derived-mode zeroc-slice-mode idl-mode "ZeroC Slice"
  "Mode for editing ZeroC Slice interface specifications"
  (make-local-variable 'zeroc-slice-font-lock-keywords)
  (setq zeroc-slice-font-lock-keywords
        (list
         (cons "\\<[_a-zA-Z][_a-zA-Z0-9]*\\(Prx\\|Helper\\|Ptr\\)\\>" font-lock-warning-face)
         (cons "\\<\\([_a-zA-Z][_a-zA-Z0-9]*\\)\\>\\*" '(1 font-lock-type-face))
         (cons (make-alternative-words-regexp "bool" "byte" "double" "float" "int" "LocalObject" "long" "Object"
                                              "short" "string" "void")
               font-lock-type-face)
         (cons (make-alternative-words-regexp "false" "true")
                font-lock-constant-face)
         (cons "\\<\\(enum\\|class\\|struct\\|interface\\|exception\\|module\\|throws\\)\\>[[:blank:]]+\\([a-zA-Z][a-zA-Z0-9]*\\)"
               '(2 font-lock-type-face))
         (cons (make-alternative-words-regexp "const" "idempotent" "implements" "local" "out" "class" "dictionary"
                                              "enum" "exception" "extends" "interface" "module" "sequence" "struct"
                                              "throws")
               font-lock-keyword-face)
         (cons (apply #'make-alternatives-regexp
                      (loop for ppkw in '("include" "define" "if" "ifdef" "ifndef" "endif" "undef")
                            collect (concat "^#[[:blank:]]*" "\\<" ppkw "\\>")))
               font-lock-preprocessor-face)
         (cons "\\([a-zA-Z][a-zA-Z0-9]*\\)[[:blank:]]*(" '(1 font-lock-function-name-face))
         (cons (concat (make-alternative-words-regexp "bool" "byte" "double" "float" "int" "LocalObject" "long"
                                                      "Object" "short" "string")
                       "[[:blank:]]+\\([a-zA-Z][a-zA-Z0-9]*\\)")
               '(1 font-lock-variable-name-face))
         (list (apply #'make-alternatives-regexp
                      (loop for ppkw in '("define" "ifdef" "ifndef" "undef")
                            collect (concat "^#[[:blank:]]*" "\\<" ppkw "\\>")))
               "\\<[_a-zA-Z][_a-zA-Z0-9]*\\>"
               nil nil '(0 font-lock-variable-name-face))))
  (setq font-lock-defaults '(zeroc-slice-font-lock-keywords nil nil)))

(setq auto-mode-alist (cons '("\\.ice$" . zeroc-slice-mode) auto-mode-alist))
