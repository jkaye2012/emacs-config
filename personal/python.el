
(use-package pyenv-mode
  :config
  (when (executable-find "pyenv")
    (pyenv-mode)))

(use-package virtualenvwrapper
  :hook ((eshell-mode . venv-initialize-eshell)
         (venv-postactivate . (lambda () (interactive)
                                (setq flycheck-python-pylint-executable
                                      (format "~/envs/%s/bin/pylint" venv-current-name)))))

  :config
  (setq-default mode-line-format (cons '(:exec venv-current-name) mode-line-format))
  (setq venv-location "~/envs")
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
    "i" '(my/python-shell :wk "interactive shell")))

(use-package company-anaconda
  :after (anaconda-mode)
  :config
  (add-to-list 'company-backends 'company-anaconda))
