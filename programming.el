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
  :defer omegamacs-enable-lazy-loading
  :commands lsp-ui-mode)

(use-package lsp-treemacs
  :ensure t
  :defer omegamacs-enable-lazy-loading
  :after (lsp-mode treemacs)
  :config
  ;; Allow C-x 1 to work normally by removing no-delete-other-windows parameter
  (defun omegamacs--delete-other-windows-advice (orig-fun &rest args)
    "Allow C-x 1 (delete-other-windows) to work normally even when packages set no-delete-other-windows.

Some packages (notably lsp-treemacs) set the 'no-delete-other-windows window parameter
to prevent their special windows from being deleted by C-x 1. This interferes with
normal window management, making it impossible for users to use C-x 1 to focus on a single window.

This advice removes the parameter from all windows before calling the original function,
restoring expected behavior for users.

Alternatives considered:
- Removing the parameter only from treemacs windows: More complex and less robust if other packages use the parameter
- Disabling the parameter via package configuration: Not always possible, as some packages set it dynamically

Potential side effects:
- May interfere with other packages or user configurations that rely on this parameter to protect certain windows
  Use with caution and consider scoping the advice more narrowly if conflicts arise."
    ;; Remove no-delete-other-windows parameter from all windows
    (dolist (window (window-list))
      (set-window-parameter window 'no-delete-other-windows nil))
    (apply orig-fun args))

  (advice-add 'delete-other-windows :around #'omegamacs--delete-other-windows-advice)

  (defun omegamacs--lsp-treemacs-symbols-auto ()
    "Auto-open treemacs symbols for C/C++ files when LSP starts."
    (when (and (or (derived-mode-p 'c-mode) (derived-mode-p 'c++-mode))
               (lsp-workspaces))
      (run-with-timer 1 nil #'lsp-treemacs-symbols)))

  :hook (lsp-mode . omegamacs--lsp-treemacs-symbols-auto)
  :commands lsp-treemacs-symbols)

;; Use consult-lsp for LSP integration with Vertico
(use-package consult-lsp
  :ensure t
  :defer omegamacs-enable-lazy-loading
  :after (lsp-mode consult)
  :bind (:map lsp-mode-map
         ([remap xref-find-apropos] . consult-lsp-symbols)))

;; Enable breadcrumbs/function display
(use-package which-func
  :ensure t  ; Built-in
  :config
  (which-function-mode 1)
  (setq which-func-display 'header)
  ;; Configure which-func colors using theme system
  (defun omegamacs-programming--apply-which-func-colors (theme)
    "Apply theme colors to which-func face."
    (omegamacs-theme-with-colors theme
      (set-face-foreground 'which-func foreground)
      (set-face-background 'which-func background-alt)))

  ;; Register with theme system and apply current theme
  (omegamacs-theme-add-hook #'omegamacs-programming--apply-which-func-colors)
  (omegamacs-programming--apply-which-func-colors omegamacs-theme-current))

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

(defun omegamacs-compile-treesit-grammars ()
  "Compile all tree-sitter grammars defined in `treesit-language-source-alist'."
  (interactive)
  (dolist (lang treesit-language-source-alist)
    (let ((lang-name (car lang)))
      (message "Compiling %s..." lang-name)
      (treesit-install-language-grammar lang-name))))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; LSP performance optimizations
;; Increase GC threshold to 100MB to reduce garbage collection frequency during LSP operations.
(defconst omegamacs/gc-cons-threshold 100000000
  "Garbage collection threshold set to 100MB for improved LSP performance.")
(setq gc-cons-threshold omegamacs/gc-cons-threshold)
(setq read-process-output-max (* 1024 1024 4)) ;; 4 MB

;; Yasnippet for code snippets
(use-package yasnippet
  :ensure t
  :defer omegamacs-enable-lazy-loading
  :hook ((prog-mode . yas-minor-mode)
         (text-mode . yas-minor-mode))
  :config
  ;; Default snippets collection
  (setq yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory)
                           yas-installed-snippets-dir))
  ;; Don't show snippets in completion by default (reduces noise)
  (setq yas-triggers-in-field nil))

;; Common snippets collection
(use-package yasnippet-snippets
  :ensure t
  :defer omegamacs-enable-lazy-loading
  :after yasnippet)

;; Indentation visualization with highlight-indent-guides
(use-package highlight-indent-guides
  :ensure t
  :defer omegamacs-enable-lazy-loading
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

  ;; Use automatic color calculation based on background
  (setq highlight-indent-guides-auto-enabled t
        highlight-indent-guides-auto-odd-face-perc 15
        highlight-indent-guides-auto-even-face-perc 20
        highlight-indent-guides-auto-character-face-perc 25))

(use-package imenu-list
  :ensure t
  :defer omegamacs-enable-lazy-loading
  :commands (imenu-list-smart-toggle imenu-list-minor-mode)
  :config
  (setq imenu-list-auto-resize t
        imenu-list-focus-after-activation t
        imenu-list-update-hook '(imenu-list-update-safe))

  :commands (imenu-list-smart-toggle imenu-list-minor-mode)
  :bind ("C-c i" . imenu-list-smart-toggle))

(use-package flycheck-pos-tip
  :ensure t
  :defer omegamacs-enable-lazy-loading
  :after flycheck)

(use-package dap-mode
  :ensure t
  :defer omegamacs-enable-lazy-loading
  :after lsp-mode)

(use-package realgud
  :ensure t
  :defer omegamacs-enable-lazy-loading
  :after (lsp-mode dap-mode)
  :config
  ;; Use realgud for debugging
  (setq realgud-safe-mode t
        realgud:display-buffer 'realgud:display-buffer-same-window
        realgud:window-height 20
        realgud:window-width 80))

(use-package realgud-ipdb
  :ensure t
  :defer omegamacs-enable-lazy-loading
  :after realgud)

(use-package realgud-lldb
  :ensure t
  :defer omegamacs-enable-lazy-loading
  :after realgud)
