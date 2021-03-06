(deftheme jk
  "Created 2020-10-14.")

(custom-theme-set-faces
 'jk
 '(default ((t (:inherit nil :extend nil :stipple nil :background "light goldenrod" :foreground "#556b72" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "ADBO" :family "Source Code Pro"))))
 '(cursor ((t (:background "#268bd2"))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(variable-pitch ((((type w32)) (:foundry "outline" :family "Arial")) (t (:family "Sans Serif"))))
 '(escape-glyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(homoglyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(minibuffer-prompt ((t (:foreground "#268bd2"))))
 '(highlight ((t (:foreground "#FFFBF0" :background "#268bd2"))))
 '(region ((t (:extend t :background "#e5e1d2"))))
 '(shadow ((t (:foreground "#D6D6D6"))))
 '(secondary-selection ((t (:extend t :background "#E1DBCD"))))
 '(trailing-whitespace ((t (:background "#dc322f"))))
 '(font-lock-builtin-face ((t (:slant italic :foreground "#d33682"))))
 '(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
 '(font-lock-comment-face ((t (:slant italic :foreground "#96A7A9"))))
 '(font-lock-constant-face ((t (:weight bold :foreground "#6c71c4"))))
 '(font-lock-doc-face ((t (:foreground "#35a69c" :inherit (font-lock-comment-face)))))
 '(font-lock-function-name-face ((t (:foreground "#b58900"))))
 '(font-lock-keyword-face ((t (:weight bold :foreground "#859900"))))
 '(font-lock-negation-char-face ((t (:foreground "#268bd2" :inherit (bold)))))
 '(font-lock-preprocessor-face ((t (:foreground "#268bd2" :inherit (bold)))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "#268bd2" :inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "#268bd2" :inherit (bold)))))
 '(font-lock-string-face ((t (:foreground "#2aa198"))))
 '(font-lock-type-face ((t (:slant italic :foreground "#b58900"))))
 '(font-lock-variable-name-face ((t (:foreground "#268bd2"))))
 '(font-lock-warning-face ((t (:inherit (warning)))))
 '(button ((t (:inherit (link)))))
 '(link ((t (:weight bold :underline (:color foreground-color :style line) :foreground "#268bd2"))))
 '(link-visited ((default (:inherit (link))) (((class color) (background light)) (:foreground "magenta4")) (((class color) (background dark)) (:foreground "violet"))))
 '(fringe ((t (:foreground "#E1DBCD" :inherit (default)))))
 '(header-line ((t (:foreground "#556b72" :background "#FDF6E3"))))
 '(tooltip ((t (:foreground "#556b72" :background "#F2E6CE"))))
 '(mode-line ((t (:box nil :background "#f4ebd7"))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t (:foreground "#268bd2"))))
 '(mode-line-highlight ((t (:inherit (highlight)))))
 '(mode-line-inactive ((t (:box nil :foreground "#96A7A9" :background "#f7f1de"))))
 '(isearch ((t (:weight bold :inherit (lazy-highlight)))))
 '(isearch-fail ((t (:weight bold :foreground "#FFFBF0" :background "#dc322f"))))
 '(lazy-highlight ((t (:weight bold :foreground "#626C6C" :background "#3F88AD"))))
 '(match ((t (:weight bold :foreground "#859900" :background "#FFFBF0"))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch))))))

(provide-theme 'jk)
