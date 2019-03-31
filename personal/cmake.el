
(use-package cmake-mode)

(use-package cmake-ide
  :config
  (cmake-ide-setup)

  (general-define-key
   :states '(normal visual insert emacs)
   :prefix ","
   :non-normal-prefix "M-,"
    "b" '(nil :wk "Build")
    "bb" '(cmake-ide-compile :wk "compile")
    "bc" '(cmake-ide-run-cmake :wk "run cmake")
    "bd" '(cmake-ide-delete-file :wk "delete file")
    ))
