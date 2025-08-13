;; C/C++ settings
(add-hook 'c-mode-common-hook 'flyspell-prog-mode)

;; Enable breadcrumbs/function display
(use-package which-func
  :ensure t  ; Built-in
  :config
  (which-function-mode 1)
  (setq which-func-display 'header))

;;Verilog mode stuff
;;(add-hook 'verilog-mode-hook
;;	  (lambda()
;;	    (setq verilog-indent-level              4
;;		  verilog-indent-level-module       4
;;		  verilog-indent-level-declaration  4
;;		  verilog-indent-level-behavioral   4
;;		  verilog-indent-level-directive    4
;;		  verilog-indent-begin-after-if     nil
;;		  verilog-indent-lists              nil
;;		  verilog-indent-declaration-macros nil
;;		  verilog-case-indent               4
;;		  verilog-cexp-indent               4
;;		  verilog-auto-newline              nil
;;		  verilog-minimum-comment-distance  12
;;		  verilog-align-ifelse              t
;;		  verilog-auto-endcomments          nil)))
;;
;;(add-hook 'verilog-mode-hook 'flyspell-prog-mode)

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

;;(use-package lsp
;;  :ensure t)

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
  :commands lsp)

;;(use-package lsp-jedi
;;  :ensure t
;;  :after lsp-mode)

;; Eglot for Python
(use-package eglot
  :ensure t
  :config
  (setq eglot-connect-timeout 120
        eglot-format-on-save nil)

  ;; Configure Pyright to disable formatting
  (setq-default eglot-workspace-configuration
                '((python (analysis (autoImportCompletions t)
                                   (typeCheckingMode "basic"))
                          (formatting (provider "none")))))

  ;; Python server configuration with fallbacks
  (add-to-list 'eglot-server-programs
	       `(python-mode . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs
	       `(python-ts-mode . ("pyright-langserver" "--stdio")))

  ;; Only auto-start eglot if language server is available
  (defun my-python-eglot-ensure ()
    "Start eglot for Python only if language server is available."
    (when (or (executable-find "pyright-langserver")
              (executable-find "pylsp")
              (executable-find "python-lsp-server"))
      (eglot-ensure)))

  :hook ((python-mode . my-python-eglot-ensure)
         (python-ts-mode . my-python-eglot-ensure)))

;;(use-package python-black
;;  :ensure t)

;;(use-package python-isort
;;  :ensure t)

;;(use-package ruff-format
;;  :ensure t)

;;(use-package flymake-ruff
;;  :ensure t)

;;(use-package pet
;;  :ensure t
;;  ;:ensure-system-package (dasel sqlite3)
;;  :config
;;  (add-hook 'python-mode-hook
;;            (lambda ()
;;              (setq-local python-shell-interpreter (pet-executable-find "python")
;;                          python-shell-virtualenv-root (pet-virtualenv-root))
;;
;;              ;; (pet-eglot-setup)
;;              ;; (eglot-ensure)
;;
;;              ;;(pet-flycheck-setup)
;;              ;;(flycheck-mode)
;;
;;              (setq-local lsp-jedi-executable-command
;;                          (pet-executable-find "jedi-language-server"))
;;
;;              (setq-local lsp-pyright-python-executable-cmd python-shell-interpreter
;;                          lsp-pyright-venv-path python-shell-virtualenv-root)
;;
;;              (lsp)
;;
;;              (setq-local dap-python-executable python-shell-interpreter)
;;
;;              (setq-local python-pytest-executable (pet-executable-find "pytest"))
;;
;;              ;;(when-let* ((ruff-executable (pet-executable-find "ruff")))
;;              ;;  (setq-local ruff-format-command ruff-executable)
;;              ;;  (ruff-format-on-save-mode))
;;
;;              ;;(when-let* ((black-executable (pet-executable-find "black")))
;;              ;;  (setq-local python-black-command black-executable)
;;              ;;  (python-black-on-save-mode))
;;
;;              (when-let* ((isort-executable (pet-executable-find "isort")))
;;                (setq-local python-isort-command isort-executable)
;;                (python-isort-on-save-mode)))))

;; Shared Verilog configuration with performance optimizations
(defun my-setup-verilog-indent ()
  "Configure Verilog indentation settings."
  (setq verilog-indent-level              4
        verilog-indent-level-module       4
        verilog-indent-level-declaration  4
        verilog-indent-level-behavioral   4
        verilog-indent-level-directive    4
        verilog-indent-begin-after-if     nil
        verilog-indent-lists              nil
        verilog-indent-declaration-macros nil
        verilog-case-indent               4
        verilog-cexp-indent               4
        verilog-auto-newline              nil
        verilog-minimum-comment-distance  12
        verilog-align-ifelse              t
        verilog-auto-endcomments          nil
        ;; Performance optimizations
        verilog-scan-cache-tick           t   ; Enable caching
        verilog-auto-save-policy          nil ; Don't auto-save during auto commands
        verilog-auto-template-warn-unused nil ; Reduce warnings for speed
        ))

(use-package verilog-ts-mode
  :ensure t
  :hook (verilog-ts-mode . my-setup-verilog-indent))

(use-package verilog-mode
  :ensure t
  :hook (verilog-mode . my-setup-verilog-indent))

;; https://github.com/gmlarumbe/verilog-ext
(use-package verilog-ext
  :ensure t
  :hook ((verilog-ts-mode . verilog-ext-mode))
  :hook ((verilog-mode . verilog-ext-mode))
  :init
  ;; Comment out/remove the ones you do not need
  (setq verilog-ext-feature-list
        '(font-lock
          xref
          capf
          hierarchy
          eglot
          ;lsp  ; Use eglot for Verilog instead of lsp-mode
          ;Look into setting this up https://github.com/manateelazycat/lsp-bridge
          ;lsp-bridge
          ;Look into setting this up https://github.com/zbelial/lspce
          ;lspce
          flycheck
          ;beautify
          navigation
          template
          ;formatter
          compilation
          imenu
          which-func
          hideshow
          typedefs
          time-stamp
          block-end-comments
          ports))
  :bind
  ;; Not ideal but verilog-ext takes over a bunch of bindings
  ("C-c C-o" . verilog-comment-region)
  ("C-c C-y" . verilog-uncomment-region)
  :config
  (verilog-ext-mode-setup)
  (verilog-ext-eglot-set-server 've-svlangserver))  ;`eglot' config only

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; Helm packages removed - using consult-lsp and consult-xref instead
;; (use-package helm-lsp
;;   :ensure t
;;   :commands helm-lsp-workspace-symbol)
;;
;; (use-package helm-xref
;;   :ensure t)

;; Use consult-lsp for LSP integration with Vertico
(use-package consult-lsp
  :ensure t
  :after (lsp-mode consult)
  :bind (:map lsp-mode-map
         ([remap xref-find-apropos] . consult-lsp-symbols)))

(use-package dap-mode
    :ensure t
    :after lsp-mode)

;; Neededed to make eglot happy
(use-package flycheck-clang-tidy
  :ensure t)

(use-package flycheck-clang-analyzer
  :ensure t)

(use-package flycheck-pos-tip
  :ensure t)

;; For performace
;; Commented out - conflicts with startup optimization in emacs_init.el
;;(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;;Python mode things
;;(use-package envrc
;;  :ensure t
;;  :when (executable-find "direnv")
;;  :hook (after-init . envrc-global-mode))
;;
;;(use-package lsp-pyright
;;  :ensure t
;;  :after lsp
;;  :custom (lsp-pyright-langserver-command "pyright") ;; or basedpyright
;;  :hook (python-mode . (lambda ()
;;                          (require 'lsp-pyright)
;;                          (lsp))))  ; or lsp-deferred

;; C++ things
;;(add-hook 'c++-mode-hook #'eglot-ensure)
;;(add-to-list 'eglot-server-programs
;;	     `(c++-mode "clangd-16"))

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

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools)
  ;; yasnippet is now configured above
  )

;;  '(cl-dolist (key '("M-<up>" "M-<down>" "M-<left>" "M-<right>"))
;;     (define-key elpy-mode-map (kbd key) nil))
;(use-package elpy
;  :ensure t
;  :bind (("C-M-<right>" . elpy-nav-indent-shift-right)
;         ("C-M-<left>" . elpy-nav-indent-shift-left))
;  :config
;  (elpy-enable)
;  (setq python-shell-interpreter "ipython"
;        python-shell-interpreter-args "-i --simple-prompt")
;
;  (eval-after-load "elpy"
;    '(cl-dolist (key '("M-<up>" "M-<down>" "M-<left>" "M-<right>"))
;       (define-key elpy-mode-map (kbd key) nil)))
;
;  (add-hook 'elpy-mode-hook
;            (lambda ()
;              (define-key elpy-mode-map (kbd "C-M-<right>") 'elpy-nav-indent-shift-right)
;              (define-key elpy-mode-map (kbd "C-M-<left>") 'elpy-nav-indent-shift-left))))

(use-package bison-mode
  :ensure t)

;; Enable eglot breadcrumbs for managed buffers
(add-hook 'eglot-managed-mode-hook 'eglot-inlay-hints-mode)

;; Indentation visualization with highlight-indent-guides
(use-package highlight-indent-guides
  :ensure t
  :hook ((prog-mode . highlight-indent-guides-mode)
         (yaml-mode . highlight-indent-guides-mode)
         (verilog-mode . highlight-indent-guides-mode)
         (verilog-ts-mode . highlight-indent-guides-mode))
  :config
  ;; Use character method in terminals, fill method in GUI
  (setq highlight-indent-guides-method (if (display-graphic-p) 'column 'fill))

  ;; Disable auto face detection to avoid theme loading issues
  (setq highlight-indent-guides-auto-enabled nil)

  ;; Highlight current indentation level
  (setq highlight-indent-guides-responsive 'stack)

  ;; Performance optimization
  (setq highlight-indent-guides-delay 0.1)

  ;; Set manual colors that work with most themes
  (setq highlight-indent-guides-character ?|)

  ;; Configure faces after theme is loaded
  (with-eval-after-load 'highlight-indent-guides
    (set-face-foreground 'highlight-indent-guides-odd-face "gray30")
    (set-face-foreground 'highlight-indent-guides-even-face "gray25")
    (set-face-foreground 'highlight-indent-guides-character-face "gray35")
    ;; Subtle but visible colors for fill mode
    (set-face-background 'highlight-indent-guides-odd-face "gray10")
    (set-face-background 'highlight-indent-guides-even-face "black")
    ;; Current indentation level highlighting
    (set-face-background 'highlight-indent-guides-top-odd-face "gray30")
    (set-face-background 'highlight-indent-guides-top-even-face "gray50")
    (set-face-background 'highlight-indent-guides-stack-odd-face "gray70")
    (set-face-background 'highlight-indent-guides-stack-even-face "gray90")))

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
