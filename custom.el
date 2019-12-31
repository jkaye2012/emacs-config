
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" default)))
 '(fci-rule-color "#62686E")
 '(jdee-db-active-breakpoint-face-colors (cons "#1c1f24" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1c1f24" "#7bc275"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1c1f24" "#484854"))
 '(package-selected-packages
   (quote
    (highlight-doxygen ivy-posframe counsel-spotify spotify ini-mode company-rtags cmake-ide magit-gerrit protobuf-mode ssh groovy-mode ivy-rtags irony-eldoc iedit doom-modeline ace-window jinja2-mode exec-path-from-shell evil-collection dotnet omnisharp company-anaconda google-this shx intero markdown-mode web-mode tide company-tern xref-js2 js2-mode npm-mode docker docker-mode docker-compose-mode dockerfile-mode evil-org smex w3m counsel-dash multi-term counsel-projectile counsel racer cmake-mode rust-mode evil-visualstar flycheck-rtags rtags flycheck-irony company-irony irony evil-matchit yasnippet-snippets yasnippet evil rainbow-delimiters evil-magit magit smart-mode-line-powerline-theme smart-mode-line eshell-prompt-extras nose virtualenvwrapper pyenv-mode avy anaconda-mode ample-theme flycheck which-key smartparens use-package)))
 '(safe-local-variable-values
   (quote
    ((eval setq projectile-enable-caching t)
     (eval add-to-list
           (quote projectile-globally-ignored-directories)
           "lib")
     (intero-targets "hs-ipfs-api:lib" "hs-ipfs-api:exe:hs-ipfs-api-exe" "hs-ipfs-api:test:hs-ipfs-api-test")
     (eval setq cmake-ide-build-dir
           (concat
            (projectile-project-root)
            "build")))))
 '(term-unbind-key-list (quote ("C-z" "C-x" "C-c" "C-h" "C-y" "<ESC>")))
 '(vc-annotate-background "#242730")
 '(vc-annotate-color-map
   (list
    (cons 20 "#7bc275")
    (cons 40 "#a6c677")
    (cons 60 "#d1ca79")
    (cons 80 "#FCCE7B")
    (cons 100 "#f4b96e")
    (cons 120 "#eda461")
    (cons 140 "#e69055")
    (cons 160 "#db8981")
    (cons 180 "#d082ae")
    (cons 200 "#C57BDB")
    (cons 220 "#d874b0")
    (cons 240 "#eb6d86")
    (cons 260 "#ff665c")
    (cons 280 "#d15e59")
    (cons 300 "#a35758")
    (cons 320 "#754f56")
    (cons 340 "#62686E")
    (cons 360 "#62686E")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'erase-buffer 'disabled nil)
