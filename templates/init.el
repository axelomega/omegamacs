;;; -*- lexical-binding: t -*-
;;; Omegamacs initialization file
;;; Copy this file to ~/.emacs.d/init.el
;;; This file loads the main configuration from a configurable directory
;;;
;;; Command line arguments:
;;;   --minimal   : Load minimal configuration only
;;;   --no-defer  : Disable lazy loading (load all packages immediately)

;;; Emacs configuration root directory
;; (setq omegamacs-emacs-config-dir "~/omegamacs")

;;; Local data directory (set in early-init.el if needed for performance)
;; Ensure omegamacs-user-emacs-directory-local is set to default if not configured
(unless (boundp 'omegamacs-user-emacs-directory-local)
  (setq omegamacs-user-emacs-directory-local user-emacs-directory))

;;; Personal settings
;; Suppress startup echo area message - replace "your-username" with your actual username
;; (setq inhibit-startup-echo-area-message "your-username")

;; Fill column setting - controls line wrapping width (defaults to 120 if unset)
;; (setq omegamacs-fill-column 200)

;;; helm-jira settings
;; (setq omegamacs-settings-jira-url      "url to JIRA project")
;; (setq omegamacs-settings-jira-username "JIRA user")
;; (setq omegamacs-settings-jira-project  "JIRA project name")

;;; Copilot configuration
;; Set to 'none, 'setup, or 'full to control Copilot loading
;; none: No Copilot support
;; setup: Minimal Copilot setup for initial server installation and authentication
;; full: Complete Copilot configuration with all features
;; (setq omegamacs-copilot-config 'none)

;;; Projectile
;; (setq omegamacs-settings-projectile-generic-command "find . -type f -not -wholename '*some_folder_to_filter/*' -not -wholename '*some_other_folder_to_filter/*' -print0")

;; Un-comment and set to use a specific shell history file
;; (setq omegamacs-compile-mode-shell-history-file (getenv "HISTFILE"))
;; Or set to a specific file path:
;; (setq omegamacs-compile-mode-shell-history-file "~/.bash_history")
;; If left unset the history file will be guessed

;; Configuration variables for shell history integration into compile mode
(defvar omegamacs-compile-mode-shell-history-size 100
  "Control how many lines of shell history to read into compile mode.
nil: never read shell history
0: read full shell history
1-N: read this many lines of most recent history")


;; Load main configuration from the configured directory
(let ((config-dir (or (and (boundp 'omegamacs-emacs-config-dir) omegamacs-emacs-config-dir)
                      "~/omegamacs")))  ; Default fallback
  (load-file (expand-file-name "emacs_init.el" config-dir)))
