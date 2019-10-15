
(use-package pyenv-mode
  :config
  (pyenv-mode))

(use-package virtualenvwrapper
  :hook ((eshell-mode . venv-initialize-eshell))

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
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode))

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
    "b" '(python-shell-send-buffer :wk "send buffer")
    "d" '(anaconda-mode-show-doc :wk "show documentation")
    "g" '(anaconda-mode-find-definitions :wk "go to definition")
    "i" '(my/python-shell :wk "interactive shell"))
  )

(use-package company-anaconda
  :after (anaconda-mode)
  :config
  (add-to-list 'company-backends 'company-anaconda))

(use-package nose
  :after (anaconda-mode)
  :config
  (defvar nose-use-verbose nil)
  (general-define-key
   :states '(normal)
   :keymaps 'anaconda-mode-map
   :prefix ","
   "t" '(nil :wk "Test")
   "tm" '(nosetests-module :wk "buffer")
   "tt" '(nosetests-one :wk "current")))
