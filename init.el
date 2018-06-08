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
(recentf-mode t)
(setq-default recent-save-file "~/.emacs.d/recentf")
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(tool-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)
(font-lock-add-keywords 'prog-mode
  '(("TODO" . font-lock-warning-face)))
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))

(defun Fuco1/lisp-indent-function (indent-point state)
  "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:

* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);

* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.

This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
  (let ((normal-indent (current-column))
        (orig-point (point)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (cond
     ;; car of form doesn't seem to be a symbol, or is a keyword
     ((and (elt state 2)
           (or (not (looking-at "\\sw\\|\\s_"))
               (looking-at ":")))
      (if (not (> (save-excursion (forward-line 1) (point))
                  calculate-lisp-indent-last-sexp))
          (progn (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point)
                                     calculate-lisp-indent-last-sexp 0 t)))
      ;; Indent under the list or under the first sexp on the same
      ;; line as calculate-lisp-indent-last-sexp.  Note that first
      ;; thing on that line has to be complete sexp since we are
      ;; inside the innermost containing sexp.
      (backward-prefix-chars)
      (current-column))
     ((and (save-excursion
             (goto-char indent-point)
             (skip-syntax-forward " ")
             (not (looking-at ":")))
           (save-excursion
             (goto-char orig-point)
             (looking-at ":")))
      (save-excursion
        (goto-char (+ 2 (elt state 1)))
        (current-column)))
     (t
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            method)
        (setq method (or (function-get (intern-soft function)
                                       'lisp-indent-function)
                         (get (intern-soft function) 'lisp-indent-hook)))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method indent-point state))))))))
(add-hook 'emacs-lisp-mode-hook
          (lambda () (setq-local lisp-indent-function #'Fuco1/lisp-indent-function)))

(use-package ample-theme)

(use-package evil
  :init
  (setq-default evil-want-C-i-jump nil)
  :config
  (setq evil-disable-insert-state-bindings t)
  (global-set-key (kbd "C-u") 'evil-scroll-up)
  (define-key evil-normal-state-map (kbd "<tab>") 'evil-indent-line)
  (define-key evil-motion-state-map (kbd ",") nil)
  (evil-mode 1))

(use-package evil-visualstar
  :config
  (global-evil-visualstar-mode))

(use-package evil-matchit
  :config
  (add-hook 'python-mode-hook 'evil-matchit-mode))

(use-package flx)

(use-package smex)

(use-package counsel
  :bind (:map ivy-minibuffer-map
	 ("C-u" . ivy-scroll-down-command)
	 ("C-d" . ivy-scroll-up-command)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)

	 :map counsel-find-file-map
	 ("C-h" . counsel-up-directory))

  :config
  (setq ivy-use-virtual-buffers t
	ivy-count-format "%d/%d "
	ivy-height 20)
  (ivy-mode 1)
  (setq ivy-re-builders-alist '((swiper . ivy--regex-plus)
				(t      . ivy--regex-fuzzy)))

  (use-package counsel-projectile
    :config
    (general-define-key
     :states '(normal visual insert emacs)
     :prefix "SPC"
     :non-normal-prefix "M-SPC"
      "p" '(nil :wk "In project")
      "pf" '(counsel-projectile-find-file :wk "find file")
      "p/" '(counsel-projectile-ag :wk "search")))
  )

(use-package hydra
  :config
  (defhydra hydra-window-select ()
    "Select window"
    ("." nil "exit")
    ("h" evil-window-left "left")
    ("j" evil-window-down "down")
    ("k" evil-window-up "up")
    ("l" evil-window-right "right"))

  (defhydra hydra-sexpr ()
    "Sexpr"
    ("s" sp-forward-slurp-sexp "slurp forward")
    ("W" sp-unwrap-sexp "unwrap"))

  )

(defun my/switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(use-package general
  :config
  (general-evil-setup)
  (general-auto-unbind-keys)

  (defun my/find-config ()
    (interactive)
    (find-file "~/.emacs.d/init.el"))

  (general-define-key
   :states '(normal)
    "C-j" '(evil-paste-pop :wk "Paste previous")
    "C-k" '(evil-paste-pop-next :wk "Paste next"))

  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "SPC" '(counsel-M-x :wk "Execute command")
   "'" '(eshell :wk "Eshell")
   "\\" '(my/find-config :wk "Edit config")
   "/" '(swiper :wk "Interactive search")

   "b" '(nil :wk "Buffer")
   "bb" '(ivy-switch-buffer :wk "list")
   "bd" '(evil-delete-buffer :wk "delete")
   "bp" '(my/switch-to-previous-buffer :wk "previous")
   "br" '(counsel-recentf :wk "list")

   "f" '(nil :wk "File")
   "ff" '(counsel-find-file :wk "find")

   "h"  '(nil :wk "Help")
   "ha" '(counsel-apropos :wk "apropos")
   "hd" '(nil :wk "Describe")
   "hdv" '(counsel-describe-variable :wk "variable")
   "hdf" '(counsel-describe-function :wk "function")
   "hdk" '(describe-key :wk "key")

   "s" '(nil :wk "Semantic")
   "sj" '(counsel-semantic :wk "jump")

   "w" '(nil :wk "Window")
   "wh" '(evil-window-left :wk "left")
   "wj" '(evil-window-down :wk "down")
   "wk" '(evil-window-up :wk "up")
   "wl" '(evil-window-right :wk "right")
   "wd" '(delete-window :wk "delete")
   "wm" '(delete-other-windows :wk "maximize")
   "ws" '(hydra-window-select/body :wk "select")
   "w/" '(evil-window-vsplit :wk "split vertically")
   "w-" '(evil-window-split :wk "split horizontally")

   "W" '(venv-workon :wk "Choose virtualenv")
   )

  (general-define-key
   :states '(normal)
   :keymaps 'emacs-lisp-mode-map
   :prefix ","
   "e" '(nil :wk "Evaluate")
   "eb" '(eval-buffer :wk "buffer")
   "ee" '(eval-defun :wk "defun")))

(use-package projectile)

(use-package avy
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "j"  '(nil :wk "Jump to")
   "jc" '(avy-goto-char :wk "char")
   "jw" '(avy-goto-word-1 :wk "word")
   "jl" '(avy-goto-line :wk "line")))

(use-package yasnippet
  :config
  (yas-global-mode t)

  (use-package yasnippet-snippets))


(use-package magit
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "g" '(nil :wk "Git")
   "gs" '(magit-status :wk "status")
   "gd" '(magit-diff :wk "diff")))

(use-package evil-magit)

(use-package rainbow-delimiters
  :config
  (add-hook 'smartparens-mode-hook 'rainbow-delimiters-mode))

(use-package flycheck
  :config
  (add-hook 'python-mode-hook #'(lambda () (setq flycheck-checker 'python-pylint)))
  (add-hook 'python-mode-hook 'flycheck-mode)

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

(use-package which-key
  :config
  (setq which-key-idle-delay 0.125)
  (which-key-mode))

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

(use-package cmake-mode)

(use-package irony
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (add-hook 'c++-mode-hook #'(lambda () (flymake-mode -1)))

  (use-package flycheck-irony
    :config
    (add-hook 'irony-mode-hook #'flycheck-irony-setup)
    (add-hook 'irony-mode-hook #'flycheck-mode))

  (use-package company-irony-c-headers
    :config
    (add-to-list 'company-backends '(company-irony-c-headers company-irony)))
  )

(use-package cmake-ide
  :config
  (cmake-ide-setup)
  (general-define-key
   :states '(normal)
   :keymaps '(c++-mode-map cmake-mode-map)
   :prefix ","
    "c" '(nil :wk "Build")
    "cc" '(cmake-ide-compile :wk "compile")
    "cm" '(cmake-ide-run-cmake :wk "cmake")
    "h" '(ff-find-other-file :wk "jump between header/source")
    )
  )

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

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (general-define-key
   :states '(normal visual insert emacs)
   :keymaps '(org-mode-map)
   "<tab>" '(org-cycle)
   )
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

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
    (evil-org smex w3m counsel-dash multi-term counsel-projectile counsel racer cmake-mode rust-mode evil-visualstar flycheck-rtags rtags flycheck-irony company-irony irony evil-matchit yasnippet-snippets yasnippet evil rainbow-delimiters evil-magit magit smart-mode-line-powerline-theme smart-mode-line eshell-prompt-extras nose virtualenvwrapper pyenv-mode avy anaconda-mode ample-theme flycheck which-key smartparens use-package)))
 '(safe-local-variable-values
   (quote
    ((eval setq cmake-ide-build-dir
	   (concat
	    (projectile-project-root)
	    "build"))
     (eval setq cmake-ide-build-dir
	   (append
	    (projectile-project-root)
	    "build"))
     (eval setq cmake-ide-build-dir
	   (file-truename "./build"))
     (setq cmake-ide-build-dir
	   (file-truename "./build")))))
 '(term-unbind-key-list (quote ("C-z" "C-x" "C-c" "C-h" "C-y" "<ESC>"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
