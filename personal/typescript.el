(defun my/typescript-delay ()
  (interactive)
  (setq-local lsp-idle-delay 2))

(use-package typescript-mode
  :hook ((typescript-mode . lsp)
         (typescript-mode . my/typescript-delay))
  :init
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode)))

(lsp-register-client
 (make-lsp-client
  :new-connection
   (lsp-tramp-connection '("typescript-language-server" "--tsserver-path" "/home/jkaye/.emacs.d/.cache/lsp/npm/typescript/bin/tsserver" "--stdio"))
   :major-modes '(typescript-mode)
   :remote? t
   :server-id 'ts-ls-remote))
