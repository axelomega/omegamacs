;;; -*- lexical-binding: t -*-
;;; Omegamacs initialization file
;;; Copy this file to ~/.emacs.d/init.el
;;; This file loads the main configuration from a configurable directory

;;; Emacs configuration root directory
;; (setq my-emacs-config-dir "~/omegamacs")

;;; Local data directory (set in early-init.el if needed for performance)
;; Ensure my-user-emacs-directory-local is set to default if not configured
(unless (boundp 'my-user-emacs-directory-local)
  (setq my-user-emacs-directory-local user-emacs-directory))

;;; Personal settings
;; Suppress startup echo area message - replace "your-username" with your actual username
;; (setq inhibit-startup-echo-area-message "your-username")

;;; helm-jira settings
;; (setq my-settings-jira-url      "url to JIRA project")
;; (setq my-settings-jira-username "JIRA user")
;; (setq my-settings-jira-project  "JIRA project name")

;;; Copilot configuration
;; Set to 'none, 'setup, or 'full to control Copilot loading
;; none: No Copilot support
;; setup: Minimal Copilot setup for initial server installation and authentication
;; full: Complete Copilot configuration with all features
;; (setq my-copilot-config 'none)

;;; Projectile
;; (setq my-settings-projectile-generic-command "find . -type f -not -wholename '*some_folder_to_filter/*' -not -wholename '*some_other_folder_to_filter/*' -print0")

;; Load main configuration from the configured directory
(let ((config-dir (or (and (boundp 'my-emacs-config-dir) my-emacs-config-dir)
                      "~/omegamacs")))  ; Default fallback
  (load-file (expand-file-name "emacs_init.el" config-dir)))
