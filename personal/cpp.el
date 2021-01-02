
(general-define-key
 :keymaps '(c++-mode-map)
 :states '(normal)
 :prefix ","
  "c" '(compile :wk "Compile")
  "h" '(projectile-find-other-file :wk "switch header/impl"))

(defun my/c++-indentation ()
  (c-set-offset 'access-label -1)
  (c-set-offset 'innamespace 0)
  (c-set-offset 'topmost-intro-cont 0)
  (c-set-offset 'brace-list-open 0)
  (c-set-offset 'brace-list-intro 1)
  (c-set-offset 'case-label '+)
  (c-set-offset 'substatement-open 0))
(add-hook 'c++-mode-hook 'my/c++-indentation)

(lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "clangd")
                    :major-modes '(c++-mode)
                    :remote? t
                    :server-id 'clangd-remote))
