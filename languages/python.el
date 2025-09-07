;;; -*- lexical-binding: t -*-
;;; Python Language Configuration

;; Eglot for Python
(use-package eglot
  :ensure t
  :config
  (setq eglot-connect-timeout 60  ; Reduced timeout
        eglot-format-on-save nil
        eglot-sync-connect nil    ; Use async connection
        eglot-events-buffer-size 0) ; Disable event logging for performance

  ;; Python server configuration using symlink
  (add-to-list 'eglot-server-programs
               `(python-mode . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               `(python-ts-mode . ("pyright-langserver" "--stdio")))

  ;; Enhanced auto-start function with better error handling
  (defun my--python-eglot-ensure ()
    "Start eglot for Python with error handling."
    (when (and (executable-find "pyright-langserver")
               (not (eglot-current-server)))
      (condition-case err
          (eglot-ensure)
        (error
         (message "Eglot failed to start: %s" (error-message-string err))))))

  ;; Manual eglot commands for debugging
  (defun my-eglot-restart ()
    "Restart eglot server for current buffer."
    (interactive)
    (when (eglot-current-server)
      (eglot-shutdown (eglot-current-server)))
    (eglot-ensure))

  :hook ((python-mode . my--python-eglot-ensure)
         (python-ts-mode . my--python-eglot-ensure))
  :bind (:map python-mode-map
         ("C-c l r" . my-eglot-restart)
         ("C-c l s" . eglot)))

;; Enable eglot breadcrumbs for managed buffers
(add-hook 'eglot-managed-mode-hook 'eglot-inlay-hints-mode)

;; Fix eglot breadcrumb colors for dark themes
(with-eval-after-load 'eglot
  (set-face-foreground 'header-line "white")
  (set-face-background 'header-line "gray20"))

;; Lark grammar files support
(use-package lark-mode
  :ensure t
  :defer t
  :mode "\\.lark\\'")
