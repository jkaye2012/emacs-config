
(use-package typescript-mode
  :hook ((typescript-mode . lsp))
  :init
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode)))
