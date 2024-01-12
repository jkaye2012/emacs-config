;; Ubiquitous programming tools

(use-package iedit)

(use-package yasnippet
  :config
  (yas-global-mode t)

  (general-define-key
   :states '(normal insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
    "y" '(nil :wk "Snippets")
    "yd" '(yas-describe-tables :wk "describe")
    "yn" '(yas-new-snippet :wk "new")
    "yv" '(yas-visit-snippet-file :wk "visit"))
  )

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
  (setq sp-escape-quotes-after-insert nil)
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
  (define-key company-active-map (kbd "RET") 'company-complete-selection)
  (define-key company-active-map (kbd "C-RET") 'company-abort)
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

(use-package highlight-doxygen
  :config
  ; (highlight-doxygen-global-mode)
  )

(use-package feature-mode)

(use-package origami
  :config
  (global-origami-mode))

(use-package copilot
  :after (quelpa-use-package)
  :quelpa (copilot :fetcher github
                   :repo "zerolfx/copilot.el"
                   :branch "main"
                   :files ("dist" "*.el"))
  :config
  (setq copilot-indent-warning-suppress t)
  (global-copilot-mode)
  (setq copilot-idle-delay (* 60 60 10))
  (general-define-key
   :states '(normal insert emacs)
    "M-<tab>" 'copilot-accept-completion :wk "accept copilot completion"
    "M-\\" 'copilot-complete :wk "complete with copilot"))

(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

(use-package exec-path-from-shell
  :ensure
  :init (exec-path-from-shell-initialize))

(use-package dap-mode
  :ensure
  :config
  (dap-ui-mode)
  (dap-ui-controls-mode 1)

  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  ;; installs .extension/vscode
  (dap-gdb-lldb-setup)
  (dap-register-debug-template
   "Rust::LLDB Run Configuration"
   (list :type "lldb"
         :request "launch"
         :name "LLDB::Run"
	 :gdbpath "rust-lldb"
         :target nil
         :cwd nil)))
