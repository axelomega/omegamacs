;;; -*- lexical-binding: t -*-
;;; Verilog Language Configuration

;; Shared Verilog configuration with performance optimizations
(defun my-setup-verilog-indent ()
  "Configure Verilog indentation settings."
  (message "VERILOG MODE SETUP")
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
        verilog-auto-lineup               nil
        verilog-auto-indent-on-newline    t
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
  :hook ((verilog-ts-mode . verilog-ext-mode)
         (verilog-mode . verilog-ext-mode))
  :init
  ;; Comment out/remove the ones you do not need
  (setq verilog-ext-feature-list
        '(font-lock
          xref
          capf
          hierarchy
          ;eglot
          ;lsp  ; Use eglot for Verilog instead of lsp-mode
          ;Look into setting this up https://github.com/manateelazycat/lsp-bridge
          ;lsp-bridge
          ;Look into setting this up https://github.com/zbelial/lspce
          ;lspce
          ;flycheck
          ;beautify
          navigation
          ;template
          ;formatter
          ;compilation
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
  (verilog-ext-mode-setup))
