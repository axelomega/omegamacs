;;; -*- lexical-binding: t -*-

;; Default indentation settings for all programming modes
(use-package prog-mode
  :ensure nil
  :config

  (setq-default indent-tabs-mode nil
                tab-width 4
                standard-indent 4))

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-log-max t)
  (setq lsp-file-watch-threshold 1000)
  :hook ((c-mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  ;; Configure clangd for C/C++
  (with-eval-after-load 'lsp-clangd
    (add-to-list 'lsp-clients-clangd-args "--header-insertion=never"))
  (setq lsp-clients-clangd-executable "clangd-16")

  (add-to-list 'lsp-disabled-clients 'lsp-ruff)
  (add-to-list 'lsp-disabled-clients 'ruff)
  (add-to-list 'lsp-disabled-clients 'bison-mode)
  (add-to-list 'lsp-disabled-clients 'semgrep-ls)

  ;; Suppress semgrep notification warnings
  (setq lsp-warn-no-matched-clients nil)

  (setq lsp-auto-execute-action nil
        lsp-before-save-edits nil
        lsp-completion-enable-additional-text-edit nil
        lsp-enable-indentation nil
        lsp-enable-on-type-formatting nil)

  (setq lsp-idle-delay 0.250)

  :commands lsp)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package lsp-treemacs
  :ensure t
  :after (lsp-mode treemacs)
  :config
  (defun my-lsp-treemacs-symbols-auto ()
    "Auto-open treemacs symbols for C/C++ files when LSP starts."
    (when (and (or (derived-mode-p 'c-mode) (derived-mode-p 'c++-mode))
               (lsp-workspaces))
      (run-with-timer 1 nil #'lsp-treemacs-symbols)))

  :hook (lsp-mode . my-lsp-treemacs-symbols-auto)
  :commands lsp-treemacs-symbols)

;; Use consult-lsp for LSP integration with Vertico
(use-package consult-lsp
  :ensure t
  :after (lsp-mode consult)
  :bind (:map lsp-mode-map
         ([remap xref-find-apropos] . consult-lsp-symbols)))


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

;; LSP performance optimizations
;;(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 4 Mb

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

(use-package imenu-list
  :ensure t
  :config
  (setq imenu-list-auto-resize t
        imenu-list-focus-after-activation t
        imenu-list-update-hook '(imenu-list-update-safe))

  :commands (imenu-list-smart-toggle imenu-list-minor-mode)
  :bind ("C-c i" . imenu-list-smart-toggle))

(use-package flycheck-pos-tip
  :ensure t)

(use-package dap-mode
  :ensure t
  :after lsp-mode)
