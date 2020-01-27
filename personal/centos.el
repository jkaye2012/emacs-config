
(load-user-module "irony")

(defun my/c++-indentation()
  (c-set-offset 'access-label [1])
  (c-set-offset 'substatement-open 0))
(add-hook 'c++-mode-hook 'my/c++-indentation)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
