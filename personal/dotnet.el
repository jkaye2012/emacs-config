
(use-package dotnet
  :config
  (general-define-key
   :states '(normal)
   :keymaps '(csharp-mode-map)
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
