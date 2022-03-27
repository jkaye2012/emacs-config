
(load-user-module "lsp")

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(defun my/clear-vterm-prompt ()
  (interactive)
  (vterm-send-C-u)
  (evil-insert 0))

(use-package vterm
  :config
  (general-define-key
   :states '(normal)
   :keymaps 'vterm-mode-map
    "S" '(my/clear-vterm-prompt :wk "clear prompt"))

  (general-define-key
   :states '(insert normal)
   :keymaps 'vterm-mode-map
    "C-n" '(vterm-send-down :wk "down")
    "C-p" '(vterm-send-up :wk "up")))
