
(use-package omnisharp
  :config
  (defun my-csharp-mode-setup ()
    (omnisharp-mode)
    (company-mode)
    (flycheck-mode)

    (setq indent-tabs-mode nil)
    (setq c-syntactic-indentation t)
    (c-set-style "ellemtel")
    (setq c-basic-offset 4)
    (setq truncate-lines t)
    (setq tab-width 4)
    (setq evil-shift-width 4)

    (local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
    (local-set-key (kbd "C-c C-c") 'recompile))
  (eval-after-load 'company '(add-to-list 'company-backends #'company-omnisharp))
  (add-hook 'csharp-mode-hook 'my-csharp-mode-setup t))

(use-package dotnet
  :config
  (general-define-key
   :states '(normal)
   :keymaps '(omnisharp-mode-map)
   :prefix ","
    "d" '(nil :wk "Dotnet Cli")
    "da" '(nil :wk "Add")
    "dap" '(dotnet-add-package :wk "package")
    "dar" '(dotnet-add-reference :wk "reference")
    "db" '(dotnet-build :wk "build")
    "dc" '(dotnet-clean :wk "clean")
    "de" '(dotnet-run :wk "run")
    "dg" '(nil :wk "Goto")
    "dgc" '(dotnet-goto-csproj :wk "csproj")
    "dgf" '(dotnet-goto-fsproj :wk "fsproj")
    "dgs" '(dotnet-goto-sln :wk "sln")
    "dn" '(dotnet-new :wk "new")
    "dp" '(dotnet-publish :wk "publish")
    "dr" '(dotnet-restore :wk "restore")
    "ds" '(nil :wk "Sln")
    "dsa" '(dotnet-sln-add :wk "add")
    "dsl" '(dotnet-sln-list :wk "list")
    "dsn" '(dotnet-sln-new :wk "new")
    "dsr" '(dotnet-sln-remove :wk "remove")
    "dt" '(dotnet-test :wk "test")
    "dT" '(dotnet-test-rerun :wk "rerun tests")
   ))
