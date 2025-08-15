;;; -*- lexical-binding: t -*-
;;; C/C++ Language Configuration

;; C/C++ settings
(add-hook 'c-mode-common-hook 'flyspell-prog-mode)

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

  ;; Add Semgrep notification handler
  (with-eval-after-load 'lsp-mode
    (lsp-register-custom-settings
     '(("semgrep/rulesRefreshed" (lambda (workspace params) nil))))
    (add-to-list 'lsp-server-install-dir "semgrep"))

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

(use-package dap-mode
  :ensure t
  :after lsp-mode)

;; Needed to make eglot happy
(use-package flycheck-clang-tidy
  :ensure t)

(use-package flycheck-clang-analyzer
  :ensure t)

(use-package flycheck-pos-tip
  :ensure t)

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools))

(use-package bison-mode
  :ensure t)
