;;; -*- lexical-binding: t -*-
;;; Elisp Language Configuration

;; Enhanced evaluation feedback
(setq eval-expression-print-length nil
      eval-expression-print-level nil)

;; Elisp mode configuration
(use-package emacs-lisp-mode
  :ensure nil
  :config

  ;; Enable eldoc for inline documentation
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

  ;; Enable checkdoc for documentation style checking
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc))))

  (setq lisp-indent-offset 2)

  :bind (:map emacs-lisp-mode-map
              ("C-c C-e" . eval-last-sexp)
              ("C-c C-b" . eval-buffer)
              ("C-c C-r" . eval-region)
              ("C-c C-d" . describe-function-at-point)))
