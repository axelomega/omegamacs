;;; -*- lexical-binding: t -*-
;;; Omegamacs local settings
;;; Copy this file to ~/.emacs.d/local_settings.el

;;; Emacs configuration root directory
;; (setq my-emacs-config-dir "~/omegamacs")

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
