;;; -*- lexical-binding: t -*-
;;; C/C++ Language Configuration

;; C/C++ settings
(add-hook 'c-mode-common-hook 'flyspell-prog-mode)

;; C/C++ indentation configuration
(use-package cc-mode
  :ensure nil
  :config
  ;; Set indentation style
  (setq c-default-style "linux"
        c-basic-offset 4)

  ;; Custom indentation rules
  (c-add-style "my-c-style"
               '("linux"
                 (c-offsets-alist
                  (innamespace . 0)           ; No indentation for namespace contents
                  (case-label . +)            ; Indent case labels
                  (statement-case-open . +)   ; Indent opening brace in case
                  (substatement-open . 0)     ; No extra indent for opening braces
                  (brace-list-open . 0)       ; No extra indent for brace lists
                  (arglist-intro . +)         ; Indent function arguments
                  (arglist-cont-nonempty . c-lineup-arglist))))

  ;; Apply the style to C/C++ modes
  (add-hook 'c-mode-hook
            (lambda ()
              (c-set-style "my-c-style")))
  (add-hook 'c++-mode-hook
            (lambda ()
              (c-set-style "my-c-style"))))

(use-package flycheck-clang-tidy
  :ensure t
  :defer my-enable-lazy-loading
  :hook (flycheck-mode . flycheck-clang-tidy-setup))

(use-package flycheck-clang-analyzer
  :ensure t
  :defer my-enable-lazy-loading
  :hook (flycheck-mode . flycheck-clang-analyzer-setup))

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools))

(use-package bison-mode
  :ensure t
  :defer my-enable-lazy-loading
  :mode "\.y\'|\.yy\'|\.l\'|\.ll\'")
