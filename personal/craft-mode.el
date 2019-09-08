
(setq craft/font-lock
      '(
        ("#.*\n" . font-lock-comment-face)
        ("[ \n]\\(data\\|concept\\|class\\|def\\|match\\|with\\|extern\\)[ \n]" . font-lock-keyword-face)
        ("self\\|true\\|false" . font-lock-constant-face)
        ("[A-Z]\\([A-Za-z]\\)+" . font-lock-type-face)
        ("[A-Z]" . font-lock-variable-name-face)
        ("[a-z]\\([a-z_]\\)+" . font-lock-function-name-face)
        ))

(define-derived-mode craft-mode prog-mode "craft"
  "Major mode for editing Craft code."
  (setq font-lock-defaults '(craft/font-lock)))

(add-to-list 'auto-mode-alist '("\\.cft\\'" . craft-mode))
