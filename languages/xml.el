;;; -*- lexical-binding: t -*-
;;; XML Language Configuration

(use-package nxml-mode
  :ensure nil  ; Built-in
  :mode ("\\.xml\\'" "\\.xsd\\'" "\\.xsl\\'" "\\.xslt\\'" "\\.svg\\'")
  :config
  (setq nxml-slash-auto-complete-flag (and (boundp 'omegamacs-parenthesis-autocomplete-enable)
                                            omegamacs-parenthesis-autocomplete-enable)
        nxml-auto-insert-xml-declaration-flag nil
        nxml-outline-child-indent 2)
  ;; Performance improvements for large XML files
  (add-hook 'nxml-mode-hook
            (lambda ()
              (setq-local font-lock-maximum-decoration 1)  ; Reduce syntax highlighting
              (setq-local jit-lock-defer-time 0.05)        ; Faster font lock
              (setq-local auto-save-default nil))))
