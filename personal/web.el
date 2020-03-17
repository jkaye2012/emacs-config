
(use-package docker)

(use-package dockerfile-mode)

(use-package docker-compose-mode)

(use-package js2-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(use-package tide
  :after (js2-mode)
  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1)
;    (add-hook 'before-save-hook 'tide-format-before-save)
;    (add-hook 'before-save-hook 'tide-organize-imports)
    (setq company-tooltip-align-annotations t))
  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  (add-hook 'before-save-hook #'tide-format-before-save)

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

(use-package web-mode
  :after (tide)
  :config
  (setq web-mode-enable-auto-quoting nil)
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

(use-package xref-js2
  :after(js2-mode)
  :config
  (add-hook 'js2-mode-hook (lambda ()
                             (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))
(use-package company-tern
  :after (js2-mode)
  :config
  (add-to-list 'company-backends 'company-tern)
  (add-hook 'js2-mode-hook 'tern-mode))

(use-package npm-mode
  :after (tide)
  :config
  (add-hook 'js2-mode-hook 'npm-mode)
  (add-hook 'tide-mode-hook 'npm-mode))
