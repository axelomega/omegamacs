;;; -*- lexical-binding: t -*-

;; Enable breadcrumbs/function display
(use-package which-func
  :ensure t  ; Built-in
  :config
  (which-function-mode 1)
  (setq which-func-display 'header)
  ;; Fix which-func colors for dark themes
  (set-face-foreground 'which-func "white")
  (set-face-background 'which-func "gray20"))

;; Tree sitter
;; These are some source for grammars
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (verilog "https://github.com/tree-sitter/tree-sitter-verilog")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; Eval this to compile them all
;;(mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; For performance
;; Commented out - conflicts with startup optimization in emacs_init.el
;;(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Yasnippet for code snippets
(use-package yasnippet
  :ensure t
  :defer t
  :hook ((prog-mode . yas-minor-mode)
         (text-mode . yas-minor-mode))
  :config
  ;; Default snippets collection
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"
                           yas-installed-snippets-dir))
  ;; Don't show snippets in completion by default (reduces noise)
  (setq yas-triggers-in-field nil))

;; Common snippets collection
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;; Indentation visualization with highlight-indent-guides
(use-package highlight-indent-guides
  :ensure t
  :hook ((prog-mode . highlight-indent-guides-mode)
         (yaml-mode . highlight-indent-guides-mode)
         (verilog-mode . highlight-indent-guides-mode)
         (verilog-ts-mode . highlight-indent-guides-mode))
  :config
  ;; Use character method in terminals, fill method in GUI
  (setq highlight-indent-guides-method (if (display-graphic-p) 'column 'column))

  ;; Disable auto face detection to avoid theme loading issues
  (setq highlight-indent-guides-auto-enabled nil)

  ;; Highlight current indentation level
  (setq highlight-indent-guides-responsive 'top)

  ;; Performance optimization
  (setq highlight-indent-guides-delay 0.05)

  ;; Set manual colors that work with most themes
  (setq highlight-indent-guides-character ?|)

  ;; Configure faces after theme is loaded
  (with-eval-after-load 'highlight-indent-guides
    (set-face-foreground 'highlight-indent-guides-odd-face "black")
    (set-face-foreground 'highlight-indent-guides-even-face "black")
    (set-face-foreground 'highlight-indent-guides-character-face "black")
    ;; Subtle but visible colors for fill mode
    (set-face-background 'highlight-indent-guides-odd-face "gray40")
    (set-face-background 'highlight-indent-guides-even-face "gray30")
    ;; Current indentation level highlighting
    (set-face-background 'highlight-indent-guides-top-odd-face "black")
    (set-face-background 'highlight-indent-guides-top-even-face "black")
    (set-face-background 'highlight-indent-guides-stack-odd-face "black")
    (set-face-background 'highlight-indent-guides-stack-even-face "black")))

;; XML mode performance optimizations
(use-package nxml-mode
  :ensure nil  ; Built-in
  :mode ("\\.xml\\'" "\\.xsd\\'" "\\.xsl\\'" "\\.xslt\\'" "\\.svg\\'")
  :config
  (setq nxml-slash-auto-complete-flag t
        nxml-auto-insert-xml-declaration-flag nil
        nxml-outline-child-indent 2)
  ;; Performance improvements for large XML files
  (add-hook 'nxml-mode-hook
            (lambda ()
              (setq-local font-lock-maximum-decoration 1)  ; Reduce syntax highlighting
              (setq-local jit-lock-defer-time 0.05)        ; Faster font lock
              (setq-local auto-save-default nil))))
