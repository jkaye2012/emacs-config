
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" default)))
 '(fci-rule-color "#555556")
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#fabd2f"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#8ec07c"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#555556"))
 '(package-selected-packages
   (quote
    (treemacs-icons-dired treemacs-magit treemacs-projectile treemacs-evil treemacs lsp-ivy company-lsp lsp-ui eglot keychain-environment counsel-spotify spotify ini-mode company-rtags cmake-ide magit-gerrit protobuf-mode ssh groovy-mode ivy-rtags irony-eldoc iedit doom-modeline ace-window jinja2-mode exec-path-from-shell evil-collection dotnet omnisharp company-anaconda google-this shx intero markdown-mode web-mode tide company-tern xref-js2 js2-mode npm-mode docker docker-mode docker-compose-mode dockerfile-mode evil-org smex w3m counsel-dash multi-term counsel-projectile counsel racer cmake-mode rust-mode evil-visualstar flycheck-rtags rtags flycheck-irony company-irony irony evil-matchit yasnippet-snippets yasnippet evil rainbow-delimiters evil-magit magit smart-mode-line-powerline-theme smart-mode-line eshell-prompt-extras nose virtualenvwrapper pyenv-mode avy anaconda-mode ample-theme flycheck which-key smartparens use-package)))
 '(safe-local-variable-values
   (quote
    ((eval setq-local org-default-notes-file
           (concat
            (locate-dominating-file default-directory ".dir-locals.el")
            "todo.org"))
     (eval setq projectile-enable-caching t)
     (eval add-to-list
           (quote projectile-globally-ignored-directories)
           "lib")
     (intero-targets "hs-ipfs-api:lib" "hs-ipfs-api:exe:hs-ipfs-api-exe" "hs-ipfs-api:test:hs-ipfs-api-test")
     (eval setq cmake-ide-build-dir
           (concat
            (projectile-project-root)
            "build")))))
 '(term-unbind-key-list (quote ("C-z" "C-x" "C-c" "C-h" "C-y" "<ESC>")))
 '(vc-annotate-background "#282828")
 '(vc-annotate-color-map
   (list
    (cons 20 "#8ec07c")
    (cons 40 "#b2bf62")
    (cons 60 "#d5be48")
    (cons 80 "#fabd2f")
    (cons 100 "#fba827")
    (cons 120 "#fc9420")
    (cons 140 "#fe8019")
    (cons 160 "#fd6237")
    (cons 180 "#fb4555")
    (cons 200 "#fb2874")
    (cons 220 "#fb335e")
    (cons 240 "#fa3e49")
    (cons 260 "#fb4934")
    (cons 280 "#d14c3c")
    (cons 300 "#a84f45")
    (cons 320 "#7e514d")
    (cons 340 "#555556")
    (cons 360 "#555556")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'erase-buffer 'disabled nil)
