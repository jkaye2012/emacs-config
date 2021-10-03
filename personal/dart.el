
(use-package dart-mode)

(use-package lsp-dart
  :hook
  (dart-mode . lsp)

  :config
  (setq gc-cons-threshold (* 100 1024 1024))
  (setq read-process-output-max (* 1024 1024))
  (setq lsp-dart-sdk-dir "/home/jkaye/src/flutter/bin/cache/dart-sdk")

  (general-define-key
   :keymaps '(dart-mode-map)
   :states '(normal)
   :prefix ","
    "d" '(nil :wk "Debug")
    "dd" '(dap-debug :wk "start debugging")
    "dr" '(lsp-dart-dap-flutter-hot-reload :wk "reload")
    "ds" '(lsp-dart-dap-flutter-hot-restart :wk "restart")
    "dt" '(lsp-dart-open-devtools :wk "devtools")
    "h" '(dap-hydra :wk "dap hydra")
    "p" '(nil :wk "Pub.dev")
    "pg" '(lsp-dart-pub-get :wk "get")
    "pu" '(lsp-dart-pub-upgrade :wk "upgrade")
    "t" '(nil :wk "Test")
    "tf" '(lsp-dart-run-test-file :wk "file")
    "tp" '(lsp-dart-run-test-at-point :wk "at point")
    "tt" '(lsp-dart-run-all-tests :wk "all")
    "tv" '(lsp-dart-test-show-tree :wk "tree")
    ))
