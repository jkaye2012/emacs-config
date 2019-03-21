;; Initialize package system

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)
(unless (require 'use-package nil 'noerror)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Emacs-wide defaults
(setq backup-directory-alist '(("." . "~/.emacs-saves")))
(recentf-mode t)
(setf recentf-max-saved-items 50)
(run-at-time nil (* 5 60) 'recentf-save-list)
(setq-default recent-save-file "~/.emacs.d/recentf")
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(tool-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)
(font-lock-add-keywords 'prog-mode
  '(("TODO" . font-lock-warning-face)))
(setq-default indent-tabs-mode nil)
(menu-bar-mode -1)
(setq compilation-scroll-output t)

;; Modularization

(defconst user-init-dir
  (let ((user-base-dir
         (cond ((boundp 'user-emacs-directory)
                user-emacs-directory)
               ((boundp 'user-init-directory)
                user-init-directory)
               (t (file-name-as-directory "~/.emacs.d/")))))
    (concat user-base-dir "personal")))

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (message "Loading user file %s" file)
  (load-file (expand-file-name file user-init-dir)))

;; Personal configuration

;; Globals should always be loaded first!
(load-user-file "global.el")

(when (string= system-type "darwin")
  (load-user-file "osx.el"))

(load-user-file "navigation.el")
(load-user-file "programming.el")
(load-user-file "git.el")
(load-user-file "elisp.el")

(use-package google-this
  :config
  (general-define-key
     :states '(normal visual insert emacs)
     :prefix "SPC"
     :non-normal-prefix "M-SPC"
      "s" '(nil :wk "Search")
      "sg" '(google-this :wk "at point")
      "ss" '(google-this-search :wk "string")
      "sw" '(google-this-word :wk "word")
      "sr" '(google-this-region :wk "region"))
  )

(use-package flx)

(use-package smex)

(use-package jinja2-mode)

(use-package flycheck
  :config
  (add-hook 'python-mode-hook #'(lambda () (setq flycheck-checker 'python-pylint)))
  (add-hook 'python-mode-hook 'flycheck-mode)
  (add-hook 'js2-mode-hook #'(lambda () (setq flycheck-checker 'javascript-jshint)))
  (add-hook 'js2-mode-hook 'flycheck-mode)

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
    "k" '(hydra-sexpr/body :wk "Sexpr"))
  )

(use-package company
  :config
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0.25)
  (define-key company-active-map (kbd "C-j") 'company-select-next)
  (define-key company-active-map (kbd "C-k") 'company-select-previous)
  (general-define-key
   :keymaps '(python-mode-map)
   :states '(insert)
    "<backtab>" 'company-complete
   )
  (global-company-mode)

  (use-package company-flx
    :config
    (setq company-flx-limit 100)
    (company-flx-mode t)))

(use-package pyenv-mode
  :config
  (pyenv-mode))

(use-package virtualenvwrapper
  :init
  (add-hook 'eshell-mode-hook 'venv-initialize-eshell)

  :config
  (setq-default mode-line-format (cons '(:exec venv-current-name) mode-line-format))
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell))

(defun my/python-shell ()
  (interactive)
  (unless (python-shell-get-process)
    (let ((git (magit-toplevel)))
      (when git
	(setenv "PYTHONPATH" git)))
    (run-python))
  (python-shell-switch-to-shell))

(use-package anaconda-mode
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)

  :bind (:map anaconda-mode-map
	 ("RET" . newline-and-indent)
	 :map inferior-python-mode-map
	 ("C-j" . comint-next-input)
	 ("C-k" . comint-previous-input))

  :config
  (general-define-key
   :states '(normal)
   :keymaps 'anaconda-mode-map
   :prefix ","
    "d" '(anaconda-mode-show-doc :wk "show documentation")
    "g" '(anaconda-mode-find-definitions :wk "go to definition")
    "i" '(my/python-shell :wk "interactive shell")
    )

  (use-package company-anaconda
    :config
    (add-to-list 'company-backends 'company-anaconda))

  (use-package nose
    :config
    (defvar nose-use-verbose nil)
    (general-define-key
     :states '(normal)
     :keymaps 'anaconda-mode-map
     :prefix ","
     "t" '(nil :wk "Test")
     "tm" '(nosetests-module :wk "buffer")
     "tt" '(nosetests-one :wk "current")))
  )

(defun protect-eshell-prompt ()
  "Protect Eshell's prompt like Comint's prompts.
E.g. `evil-change-whole-line' won't wipe the prompt. This
is achieved by adding the relevant text properties."
  (let ((inhibit-field-text-motion t))
    (add-text-properties
     (point-at-bol)
     (point)
     '(rear-nonsticky t
		      inhibit-line-move-field-capture t
		      field output
                      read-only t
                      front-sticky (field inhibit-line-move-field-capture)))))

(use-package eshell-prompt-extras
  :config
  (setq eshell-highlight-prompt nil
	eshell-prompt-function 'epe-theme-lambda)

  (defun my/setup-eshell-keys ()
    (local-set-key (kbd "C-j") 'eshell-next-input)
    (local-set-key (kbd "C-k") 'eshell-previous-input))

  (defun eshell/clear ()
    "Clear the eshell buffer."
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)))

  (add-hook 'eshell-mode-hook 'my/setup-eshell-keys)
  (add-hook 'eshell-after-prompt-hook 'protect-eshell-prompt)
  )

(use-package rust-mode
  :config
  (use-package racer
    :config
    (add-hook 'rust-mode-hook 'racer-mode)
    (add-hook 'racer-mode-hook 'eldoc-mode)
    (add-hook 'racer-mode-hook 'company-mode)))

(define-generic-mode 'ebnf-mode
  '(("(*" . "*)"))
  '("=")
  '(("^[^ \t\n][^=]+" . font-lock-variable-name-face)
    ("['\"].*?['\"]" . font-lock-string-face)
    ("\\?.*\\?" . font-lock-negation-char-face)
    ("\\[\\|\\]\\|{\\|}\\|(\\|)\\||\\|,\\|;" . font-lock-type-face)
    ("[^ \t\n]" . font-lock-function-name-face))
  '("\\.ebnf\\'")
  `(,(lambda () (setq mode-name "EBNF")))
  "Major mode for EBNF metasyntax text highlighting.")

(provide 'ebnf-mode)

(use-package docker)

(use-package dockerfile-mode)

(use-package docker-compose-mode)

(use-package npm-mode
  :config
  (add-hook 'js2-mode-hook 'npm-mode)
  (add-hook 'tide-mode-hook 'npm-mode))

(use-package tide
  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1)
    (add-hook 'before-save-hook 'tide-format-before-save)
    (add-hook 'before-save-hook 'tide-organize-imports)
    (setq company-tooltip-align-annotations t))
  (add-hook 'typescript-mode-hook #'setup-tide-mode)

  (use-package web-mode
    :config
    (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
    (add-hook 'web-mode-hook
              (lambda ()
                (when (string-equal "tsx" (file-name-extension buffer-file-name))
                  (setup-tide-mode))))
    ;; enable typescript-tslint checker
    (flycheck-add-mode 'typescript-tslint 'web-mode)
    (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
    (add-hook 'web-mode-hook
              (lambda ()
                (when (string-equal "jsx" (file-name-extension buffer-file-name))
                  (setup-tide-mode))))
    ;; configure jsx-tide checker to run after your default jsx checker
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)
    )

  (add-hook 'js2-mode-hook #'setup-tide-mode)
  ;; configure javascript-tide checker to run after your default javascript checker
  (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)

  (defun my/tide-navigate ()
    (interactive)
    (tide-references)
    (select-window (get-buffer-window "*tide-references*")))

  (defun my/tide-goto ()
    (interactive)
    (tide-goto-reference))

  (general-define-key
   :states '(normal)
   :keymaps '(tide-references-mode-map)
    "<RET>" '(my/tide-goto :wk "goto reference")
    "n" '(tide-find-next-reference :wk "next")
    "N" '(tide-find-previous-reference :wk "previous"))

  (general-define-key
   :states '(normal)
   :keymaps '(tide-mode-map)
   :prefix ","
    "d" '(tide-documentation-at-point :wk "documentation at point")
    "g" '(tide-jump-to-definition :wk "jump to definition")
    "r" '(my/tide-navigate :wk "navigate references")
    ))

(use-package js2-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (use-package xref-js2
    :config
    (add-hook 'js2-mode-hook (lambda ()
                               (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))
  (use-package company-tern
    :config
    (add-to-list 'company-backends 'company-tern)
    (add-hook 'js2-mode-hook 'tern-mode)))

(use-package cmake-mode)

(use-package cquery
  :config
  (setq cquery-executable "/build/jkaye/cquery/build/cquery")
  (setq cquery-project-roots '("/build/jkaye/Harbor/Laser"))
  (setq xref-prompt-for-identifier '(not xref-find-definitions xref-find-definitions-other-window xref-find-definitions-other-frame xref-find-references))

  (general-define-key
   :states '(normal)
   :keymaps '(c++-mode-map)
   :prefix ","
    "g" '(xref-find-definitions :wk "find definition")
    "h" '(projectile-find-other-file :wk "header/source jump")
    "i" '(lsp-goto-implementation :wk "find implementation")
    "e" '(nil :wk "Error")
    "en" '(flymake-goto-next-error :wk "next")
    "ep" '(flymake-goto-prev-error :wk "previous")
    "p" '(nil :wk "Peek")
    "pb" '(lsp-ui-peek-jump-backward :wk "backward")
    "pf" '(lsp-ui-peek-jump-forward :wk "forward")
    "ps" '(lsp-ui-peek-find-workspace-symbol :wk "symbol")
    "r" '(xref-find-references :wk "find references")
    )

  :init
  (add-hook 'lsp-mode-hook '(lambda ()
                              (setq company-transformers nil company-lsp-async t company-lsp-cache-candidates nil)))
  (add-hook 'c-mode-hook 'lsp)
  (add-hook 'c++-mode-hook 'lsp))

(use-package ivy-xref
  :ensure t
  :init (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(defun my/c++-indentation()
  (c-set-offset 'access-label [1])
  (c-set-offset 'substatement-open 0))
(add-hook 'c++-mode-hook 'my/c++-indentation)

(defun my/hask-completion ()
  (setq-local company-minimum-prefix-length 3)
  (setq-local company-idle-delay 0.5)
  (setq-local flycheck-check-syntax-automatically '(mode-enabled save))
  )

(use-package haskell-mode
  :config
  (require 'haskell-doc)
  )

(use-package intero
  :config
  (intero-global-mode 1))

(use-package omnisharp
  :config
  (defun my-csharp-mode-setup ()
    (omnisharp-mode)
    (company-mode)
    (flycheck-mode)

    (setq indent-tabs-mode nil)
    (setq c-syntactic-indentation t)
    (c-set-style "ellemtel")
    (setq c-basic-offset 4)
    (setq truncate-lines t)
    (setq tab-width 4)
    (setq evil-shift-width 4)

                                        ;csharp-mode README.md recommends this too
  ;(electric-pair-mode 1)       ;; Emacs 24
  ;(electric-pair-local-mode 1) ;; Emacs 25

    (local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
    (local-set-key (kbd "C-c C-c") 'recompile))
  (eval-after-load 'company '(add-to-list 'company-backends #'company-omnisharp))
  (add-hook 'csharp-mode-hook 'my-csharp-mode-setup t))

(use-package dotnet
  :config
  (general-define-key
   :states '(normal)
   :keymaps '(omnisharp-mode-map)
   :prefix ","
    "d" '(nil :wk "Dotnet Cli")
    "da" '(nil :wk "Add")
    "dap" '(dotnet-add-package :wk "package")
    "dar" '(dotnet-add-reference :wk "reference")
    "db" '(dotnet-build :wk "build")
    "dc" '(dotnet-clean :wk "clean")
    "de" '(dotnet-run :wk "run")
    "dg" '(nil :wk "Goto")
    "dgc" '(dotnet-goto-csproj :wk "csproj")
    "dgf" '(dotnet-goto-fsproj :wk "fsproj")
    "dgs" '(dotnet-goto-sln :wk "sln")
    "dn" '(dotnet-new :wk "new")
    "dp" '(dotnet-publish :wk "publish")
    "dr" '(dotnet-restore :wk "restore")
    "ds" '(nil :wk "Sln")
    "dsa" '(dotnet-sln-add :wk "add")
    "dsl" '(dotnet-sln-list :wk "list")
    "dsn" '(dotnet-sln-new :wk "new")
    "dsr" '(dotnet-sln-remove :wk "remove")
    "dt" '(dotnet-test :wk "test")
    "dT" '(dotnet-test-rerun :wk "rerun tests")
   ))

(use-package multi-term
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "t" '(nil :wk "Terminal")
   "tt" '(multi-term :wk "new")
   "tn" '(multi-term-next :wk "next")
   "tp" '(multi-term-prev :wk "next")
   ))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-org
  :ensure t
  :after org
  :config
  (setq org-todo-keywords '((sequence "TODO(t!)" "IN PROGRESS(i!)" "DONE(d@)")))
  (setq org-log-into-drawer t)
  (setq org-log-reschedule 'time)
  (setq org-agenda-skip-scheduled-if-done t)
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (general-define-key
   :states '(normal visual insert emacs)
   :keymaps '(org-mode-map)
   "<tab>" '(org-cycle)
  )
  (general-define-key
   :states '(normal)
   :keymaps '(org-mode-map)
    "t" 'org-shiftright
   )
  (general-define-key
   :states '(normal)
   :keymaps '(org-mode-map)
   :prefix ","
    "a" '(org-agenda :wk "agenda")
    "c" '(org-archive :wk "archive subtree")
    "g" '(org-edit-special :wk "edit linked file")
    "i" '(org-insert-heading-after-current :wk "insert heading")
    "n" '(org-next-visible-heading :wk "next heading")
    "p" '(org-previous-visible-heading :wk "previous heading")
    "s" '(org-sort :wk "sort")
    "t" '(nil :wk "Todo")
    "td" '(org-deadline :wk "deadline")
    "tn" '(org-add-note :wk "note")
    "ts" '(org-schedule :wk "schedule")
    "tt" '(org-shiftright :wk "cycle")
    "tT" '(org-shiftleft :wk "cycle backwards")
    "tg" '(org-todo :wk "goto state")
   )
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; Load decorations at the end!
(load-user-file "decoration.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" default)))
 '(fci-rule-color "#5B6268")
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(package-selected-packages
   (quote
    (iedit doom-modeline ace-window jinja2-mode exec-path-from-shell evil-collection dotnet omnisharp company-anaconda google-this shx intero markdown-mode web-mode tide company-tern xref-js2 js2-mode npm-mode docker docker-mode docker-compose-mode dockerfile-mode evil-org smex w3m counsel-dash multi-term counsel-projectile counsel racer cmake-mode rust-mode evil-visualstar flycheck-rtags rtags flycheck-irony company-irony irony evil-matchit yasnippet-snippets yasnippet evil rainbow-delimiters evil-magit magit smart-mode-line-powerline-theme smart-mode-line eshell-prompt-extras nose virtualenvwrapper pyenv-mode avy anaconda-mode ample-theme flycheck which-key smartparens use-package)))
 '(safe-local-variable-values
   (quote
    ((eval setq cmake-ide-build-dir
           (concat
            (projectile-project-root)
            "build")))))
 '(term-unbind-key-list (quote ("C-z" "C-x" "C-c" "C-h" "C-y" "<ESC>")))
 '(vc-annotate-background "#282c34")
 '(vc-annotate-color-map
   (list
    (cons 20 "#98be65")
    (cons 40 "#b4be6c")
    (cons 60 "#d0be73")
    (cons 80 "#ECBE7B")
    (cons 100 "#e6ab6a")
    (cons 120 "#e09859")
    (cons 140 "#da8548")
    (cons 160 "#d38079")
    (cons 180 "#cc7cab")
    (cons 200 "#c678dd")
    (cons 220 "#d974b7")
    (cons 240 "#ec7091")
    (cons 260 "#ff6c6b")
    (cons 280 "#cf6162")
    (cons 300 "#9f585a")
    (cons 320 "#6f4e52")
    (cons 340 "#5B6268")
    (cons 360 "#5B6268")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
