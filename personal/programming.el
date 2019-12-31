;; Ubiquitous programming tools

(use-package iedit)

(use-package yasnippet
  :config
  (yas-global-mode t))

(use-package yasnippet-snippets
  :after (yasnippet))

(use-package rainbow-delimiters
  :config
  (add-hook 'smartparens-mode-hook 'rainbow-delimiters-mode))

(use-package flycheck
  :hook ((prog-mode . flycheck-mode))
  :config
  (add-hook 'python-mode-hook #'(lambda () (setq flycheck-checker 'python-pylint)))
  (add-hook 'js2-mode-hook #'(lambda () (setq flycheck-checker 'javascript-jshint)))

  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
    "e" '(nil :wk "Errors")
    "el" '(flycheck-list-errors :wk "list")
    "ep" '(flycheck-previous-error :wk "previous")
    "en" '(flycheck-next-error :wk "next")))

(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode)
  (show-smartparens-global-mode)

  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
    "k" '(hydra-sexpr/body :wk "Sexpr")))

(use-package company
  :config
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0.25)
  (define-key company-active-map (kbd "C-j") 'company-select-next)
  (define-key company-active-map (kbd "C-k") 'company-select-previous)
  (global-company-mode))

(use-package company-flx
  :after (company)
  :config
  (setq company-flx-limit 100)
  (company-flx-mode t))

(use-package protobuf-mode)

(define-generic-mode 'ebnf-mode
  '(("(*" . "*)"))
  '("=")
  '(("^[^ \t\n][^=]+" . font-lock-variable-name-face)
    ("['\"].*?['\"]" . font-lock-string-face)
    ("\\?.*\\?" . font-lock-negation-char-face)
    ("\\[\\|\\]\\|{\\|}\\|(\\|)\\||\\|,\\|;" . font-lock-type-face)
    ("[^ \t\n]" . font-lock-function-name-face))
  '("\\.ebnf\\'")
  `(,(lambda ()
       (setq mode-name "EBNF")
       (face-remap-add-relative 'font-lock-comment-face '(:foreground "medium sea green"))))
  "Major mode for EBNF metasyntax text highlighting.")

(provide 'ebnf-mode)

(use-package counsel-dash
  :after (counsel)
  :config
  (setq counsel-dash-common-docsets '("C++" "Python 2" "C"))

  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
    "d" '(nil :wk "Dash")
    "dd" '(counsel-dash :wk "search")))

(use-package highlight-doxygen
  :config
  (highlight-doxygen-global-mode))
