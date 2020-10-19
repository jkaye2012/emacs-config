
(use-package typescript-mode
  :hook ((typescript-mode . lsp))
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode)))
