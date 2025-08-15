;;; -*- lexical-binding: t -*-
;;; C/C++ Language Configuration

;; C/C++ settings
(add-hook 'c-mode-common-hook 'flyspell-prog-mode)

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
