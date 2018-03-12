(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(unless (require 'use-package nil 'noerror)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Emacs-wide defaults
(setq backup-directory-alist '(("." . "~/.emacs-saves")))
(setq desktop-auto-save-timeout 300)
(desktop-save-mode t)
(recentf-mode t)
(setq-default recent-save-file "~/.emacs.d/recentf")
(semantic-mode t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package ample-theme)

(use-package evil
  :config
  (setq evil-disable-insert-state-bindings t)
  (global-set-key (kbd "C-u") 'evil-scroll-up)
  (define-key evil-normal-state-map (kbd "<tab>") 'evil-indent-line)
  (evil-mode 1))

(use-package helm
  :config
  (setq helm-ff-file-name-history-use-recentf t)
  :bind (:map helm-map
	      ("<tab>" . helm-execute-persistent-action)
	      ("C-h" . helm-find-files-up-one-level)
	      ("C-j" . helm-next-line)
	      ("C-k" . helm-previous-line))
  )

(use-package avy)

(use-package projectile)

(use-package helm-projectile)

(use-package helm-ag)

(use-package yasnippet
  :config
  (yas-global-mode t))

(use-package yasnippet-snippets)

(use-package magit)

(use-package evil-magit)

(use-package rainbow-delimiters
  :config
  (add-hook 'smartparens-mode-hook 'rainbow-delimiters-mode))

(use-package flycheck
  :config
  (add-hook 'python-mode-hook #'(lambda () (setq flycheck-checker 'python-pylint)))
  (add-hook 'python-mode-hook 'flycheck-mode))

(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode)
  (show-smartparens-global-mode))

(use-package pyenv-mode
  :config
  (pyenv-mode))

(use-package virtualenvwrapper
  :config
  (setq-default mode-line-format (cons '(:exec venv-current-name) mode-line-format))
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell))

(use-package nose
  :config
  (defvar nose-use-verbose nil))

(use-package anaconda-mode
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(use-package company-anaconda
  :config
  (add-to-list 'company-backends 'company-anaconda))

(use-package hydra
  :config
  (setq hydra-key-doc-function #'(lambda (key key-width doc doc-width)
				   (format (format "%%%ds → %%%ds" key-width (- -1 doc-width)) key doc)))
  (defhydra hydra-buffer (:exit t)
    "Buffer"
    ("b" helm-mini "helm")
    ("d" evil-delete-buffer "delete"))
  (defhydra hydra-help-describe (:exit t)
    "Describe"
    ("f" describe-function "function")
    ("k" describe-key "key")
    ("v" describe-variable "variable"))
  (defhydra hydra-help (:exit t)
    "Help"
    ("d" hydra-help-describe/body "describe"))
  (defhydra hydra-file (:exit t)
    "File"
    ("f" helm-find-files "find"))
  (defhydra hydra-git (:exit t)
    "Git"
    ("d" magit-diff "diff")
    ("s" magit-status "status"))
  (defhydra hydra-jump (:exit t)
    "Jump"
    ("c" avy-goto-char "to char")
    ("l" avy-goto-line "to line")
    ("w" avy-goto-word-1 "to word"))
  (defhydra hydra-project (:exit t)
    "In project"
    ("/" helm-projectile-ag "search")
    ("f" helm-projectile-find-file "find file"))
  (defhydra hydra-window (:exit t)
    "Window"
    ("m" delete-other-windows "maximize")
    ("-" evil-window-split "split horizontally")
    ("/" evil-window-vsplit "split vertically")
    ("h" evil-window-left "window left")
    ("j" evil-window-down "window down")
    ("k" evil-window-up "window up")
    ("l" evil-window-right "window right")
    ("d" delete-window "delete"))
  (defhydra hydra-semantic (:exit t)
    "Semantic"
    ("j" helm-semantic "jump"))

  ;; Mode-specific Hydras

  ;; Emacs lisp
  (defhydra hydra-lisp-mode (:exit t)
    "Lisp"
    ("b" eval-buffer "evaluate buffer"))

  ;; Python
  (defhydra hydra-python-test (:exit t)
    "Test"
    ("m" nosetests-module "module")
    ("t" nosetests-one "current test"))
  (defhydra hydra-python-mode (:exit t)
    "Python"
    ("d" anaconda-mode-show-doc "show documentation")
    ("g" anaconda-mode-find-definitions "go to definition")
    ("t" hydra-python-test/body "test"))
  )

(use-package general
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "SPC" '(helm-M-x :which-key "Execute command")
   "'" '(eshell :which-key "Eshell")
   "b" '(hydra-buffer/body :which-key "Buffer")
   "f" '(hydra-file/body :which-key "File")
   "g" '(hydra-git/body :which-key "Git")
   "h" '(hydra-help/body :which-key "Help")
   "j" '(hydra-jump/body :which-key "Jump")
   "p" '(hydra-project/body :which-key "Project")
   "s" '(hydra-semantic/body :which-key "Semantic")
   "w" '(hydra-window/body :which-key "Window")
   "W" '(venv-workon :which-key "Choose virtualenv")
   )

  ;; Emacs lisp mode bindings
  (general-define-key
   :states '(normal)
   :keymaps 'emacs-lisp-mode-map
   :prefix "SPC"
   "m" '(hydra-lisp-mode/body :which-key "Lisp-Mode"))

  ;; Python mode bindings
  (general-define-key
   :states '(normal)
   :keymaps 'python-mode-map
   :prefix "SPC"
   "m" '(hydra-python-mode/body :which-key "Python-Mode"))
  )

(use-package which-key
  :config
  (which-key-mode))

(use-package company
  :config
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0.25)
  (define-key company-active-map (kbd "C-j") 'company-select-next)
  (define-key company-active-map (kbd "C-k") 'company-select-previous)
  (global-company-mode))

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
  (general-define-key
   :states '(normal insert)
   "C-j" 'eshell-next-input
   "C-k" 'eshell-previous-input)
  (add-hook 'eshell-after-prompt-hook 'protect-eshell-prompt)
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (ample)))
 '(custom-safe-themes
   (quote
    ("84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" default)))
 '(package-selected-packages
   (quote
    (yasnippet-snippets yasnippet helm-ag helm evil rainbow-delimiters evil-magit magit smart-mode-line-powerline-theme smart-mode-line eshell-prompt-extras nose virtualenvwrapper pyenv-mode avy anaconda-mode ample-theme helm-projectile flycheck which-key smartparens use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
