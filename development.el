;;; -*- lexical-binding: t -*-

;; Modern development environment enhancements

;; Enhanced project management (built-in since Emacs 28)
(use-package project
  :ensure nil  ; Built-in
  :config
  (setq project-switch-commands
        '((project-find-file "Find file")
          (project-find-regexp "Find regexp")
          (project-dired "Dired")
          (magit-project-status "Magit" ?g)
          (project-shell "Shell" ?s))))

;; Better help system with more detailed information
(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)
         ("C-h d" . helpful-at-point)))

;; Enhanced grep with ripgrep integration
(use-package rg
  :ensure t
  :config
  (rg-enable-default-bindings))

;; Better dired with modern features
(use-package dired-x
  :ensure nil  ; Built-in
  :config
  (setq dired-omit-files "^\\.[^.]\\|^#\\|~$"))

(use-package dired-subtree
  :ensure t
  :defer omegamacs-enable-lazy-loading
  :bind (:map dired-mode-map
         ("<tab>" . dired-subtree-toggle)
         ("<backtab>" . dired-subtree-cycle)))

;; Enhanced programming modes
(use-package rainbow-delimiters
  :ensure t
  :defer omegamacs-enable-lazy-loading
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smartparens
  :ensure t
  :defer omegamacs-enable-lazy-loading
  :hook (prog-mode . (lambda ()
                       (when (and (boundp 'omegamacs-parenthesis-autocomplete-enable)
                                  omegamacs-parenthesis-autocomplete-enable)
                         (smartparens-mode 1))))
  :config
  (require 'smartparens-config)
  ;; Configure smartparens auto-insertion based on omegamacs setting
  (setq sp-autoinsert-pair (and (boundp 'omegamacs-parenthesis-autocomplete-enable)
                                omegamacs-parenthesis-autocomplete-enable)))

;; Better editing with multiple cursors
(use-package multiple-cursors
  :ensure t
  :defer omegamacs-enable-lazy-loading
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;; Enhanced search and replace
(use-package anzu
  :ensure t
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode +1))

;; Better undo system
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t
        ;; Store undo-tree files in .emacs.d/undo-tree/
        undo-tree-history-directory-alist (list (cons "." (expand-file-name "undo-tree/" omegamacs-user-emacs-directory-local))))
  ;; Create the directory if it doesn't exist
  (let ((undo-tree-dir (expand-file-name "undo-tree/" omegamacs-user-emacs-directory-local)))
    (unless (file-exists-p undo-tree-dir)
      (make-directory undo-tree-dir t))))

;; Enhanced terminal integration
(use-package vterm
  :ensure t
  :defer omegamacs-enable-lazy-loading
  :commands (vterm vterm-other-window)
  :config
  (setq vterm-max-scrollback 10000))

;; Code formatting
;;(use-package format-all
;;  :ensure t
;;  :hook (prog-mode . format-all-ensure-formatter))

;; Additional compilation settings (extending existing compilation.el)
(with-eval-after-load 'compile
  (setq compilation-ask-about-save nil
        compilation-always-kill t
        compilation-scroll-output 'first-error))

;; Hide comments temporarily to focus on code structure
(use-package nocomments-mode
  :ensure t
  :defer omegamacs-enable-lazy-loading
  :bind (("<f12>" . nocomments-mode)))
