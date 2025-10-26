;;; -*- lexical-binding: t -*-
;;; Omegamacs initialization file
;;; Copy this file to ~/.emacs.d/init.el
;;; This file loads the main configuration from a configurable directory
;;;
;;; Command line arguments:
;;;   --minimal   : Load minimal configuration only
;;;   --no-defer  : Disable lazy loading (load all packages immediately)
;;;
;;; NOTE: All omegamacs configuration variables are defined with defaults in defaults.el
;;; You can override any of these defaults by setting them in this file BEFORE loading
;;; the main configuration. Use setq to override - no need for defvar or boundp checks.

;;; Emacs configuration root directory
;; Uncomment and modify if omegamacs is located somewhere other than ~/omegamacs
;; (setq omegamacs-emacs-config-dir "~/omegamacs")

;;; Local data directory
;; This is typically set in early-init.el if needed for performance
;; Defaults to user-emacs-directory if not set
;; (setq omegamacs-user-emacs-directory-local "/path/to/local/data")

;;; Personal settings
;; Suppress startup echo area message - replace "your-username" with your actual username
;; (setq inhibit-startup-echo-area-message "your-username")

;;; Display settings
;; Fill column setting - controls line wrapping width (default: 120)
;; (setq omegamacs-fill-column 200)

;;; Feature flags
;; Parenthesis auto-completion control (default: nil/disabled)
;; Set to t to enable auto-pairing of parentheses, brackets, braces across modes
;; (setq omegamacs-parenthesis-autocomplete-enable t)

;; Copilot configuration (default: nil)
;; Set to 'setup or 'full to control Copilot loading
;; nil: No Copilot support
;; 'setup: Minimal Copilot setup for initial server installation and authentication
;; 'full: Complete Copilot configuration with all features
;; (setq omegamacs-copilot-config 'full)

;;; Integration settings - JIRA
;; (setq omegamacs-settings-jira-url      "https://jira.example.com")
;; (setq omegamacs-settings-jira-username "your-username")
;; (setq omegamacs-settings-jira-project  "PROJ")

;;; Integration settings - Projectile
;; Custom command for file listing (default: nil, uses git ls-files with fallback)
;; (setq omegamacs-settings-projectile-generic-command
;;       "find . -type f -not -wholename '*some_folder/*' -print0")

;;; Integration settings - Compilation
;; Shell history file integration (default: nil, auto-detected based on $SHELL)
;; (setq omegamacs-compile-mode-shell-history-file (getenv "HISTFILE"))
;; (setq omegamacs-compile-mode-shell-history-file "~/.bash_history")

;; Number of shell history lines to read (default: 100)
;; nil: never read shell history
;; 0: read full shell history
;; N: read this many lines of most recent history
;; (setq omegamacs-compile-mode-shell-history-size 200)

;;; Load main configuration
;; Load the main omegamacs configuration from the configured directory
(let ((config-dir (if (boundp 'omegamacs-emacs-config-dir)
                      omegamacs-emacs-config-dir
                    "~/omegamacs")))  ; Default fallback
  (load-file (expand-file-name "emacs_init.el" config-dir)))
