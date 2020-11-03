(defun my/typescript-delay ()
  (interactive)
  (setq-local lsp-idle-delay 2))

(use-package typescript-mode
  :hook ((typescript-mode . lsp)
         (typescript-mode . my/typescript-delay))
  :init
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode)))
