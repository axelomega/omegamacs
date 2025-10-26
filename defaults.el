;;; defaults.el --- Default values for all omegamacs configuration variables -*- lexical-binding: t; -*-

;; This file is part of omegamacs and is loaded very early in the
;; initialization process. It defines default values for all omegamacs
;; configuration variables so that the rest of the codebase can assume
;; these variables are always bound.
;;
;; IMPORTANT: Users can override these defaults by setting variables in their
;; ~/.emacs.d/init.el BEFORE loading emacs_init.el. The `defvar` form will
;; NOT override variables that are already bound, preserving user settings.
;;
;; NOTE: omegamacs-emacs-config-dir is set in emacs_init.el, not here, since
;; it must be determined before loading this file.

;;; Code:

;; ============================================================================
;; System and Path Variables
;; ============================================================================

(unless (boundp 'omegamacs-user-emacs-directory-local)
  (defvar omegamacs-user-emacs-directory-local
    user-emacs-directory
    "Directory for storing local Emacs data (cache, packages, etc.).
This can be set to a different location to keep user data separate
from the configuration directory."))

;; ============================================================================
;; Internal System Variables (GC and Performance)
;; ============================================================================
;; These capture original values before optimization

(defvar omegamacs--gc-cons-threshold gc-cons-threshold
  "Original value of gc-cons-threshold before omegamacs initialization.")

(defvar omegamacs--gc-cons-percentage gc-cons-percentage
  "Original value of gc-cons-percentage before omegamacs initialization.")

(defvar omegamacs--file-name-handler-alist file-name-handler-alist
  "Original value of file-name-handler-alist before omegamacs initialization.")

;; ============================================================================
;; Command-Line Flags and Initialization Modes
;; ============================================================================
;; These are computed from command-line-args, but can be overridden by user

(unless (boundp 'omegamacs-minimal-config)
  (defvar omegamacs-minimal-config
    (member "--minimal" command-line-args)
    "When non-nil, load only essential configuration.
Set automatically based on the --minimal command-line flag."))

(unless (boundp 'omegamacs-enable-lazy-loading)
  (defvar omegamacs-enable-lazy-loading
    (not (member "--no-defer" command-line-args))
    "When non-nil, enable lazy loading with :defer keyword.
Set automatically based on the --no-defer command-line flag."))

;; ============================================================================
;; Display and Editor Settings
;; ============================================================================

(unless (boundp 'omegamacs-fill-column)
  (defvar omegamacs-fill-column 120
    "Default fill column width for text wrapping."))

(unless (boundp 'omegamacs-theme-current)
  (defvar omegamacs-theme-current 'dark
    "Current active theme.
This variable is also defined as a defcustom in theme.el for the
customize interface, but we provide a default here for early initialization."))

;; ============================================================================
;; Feature Flags
;; ============================================================================

(unless (boundp 'omegamacs-parenthesis-autocomplete-enable)
  (defvar omegamacs-parenthesis-autocomplete-enable nil
    "When non-nil, enable automatic parenthesis/bracket completion.
This affects electric-pair-mode, smartparens, and various language-specific
pairing behaviors in LaTeX (cdlatex) and other modes."))

(unless (boundp 'omegamacs-copilot-config)
  (defvar omegamacs-copilot-config nil
    "Copilot integration configuration.
Valid values:
  nil    - No copilot integration
  'setup - Load basic copilot setup
  'full  - Load full copilot configuration"))

;; ============================================================================
;; Integration Settings - JIRA
;; ============================================================================

(unless (boundp 'omegamacs-settings-jira-url)
  (defvar omegamacs-settings-jira-url nil
    "JIRA server URL for issue tracking integration.
Example: \"https://jira.example.com\""))

(unless (boundp 'omegamacs-settings-jira-username)
  (defvar omegamacs-settings-jira-username nil
    "JIRA username for authentication."))

(unless (boundp 'omegamacs-settings-jira-project)
  (defvar omegamacs-settings-jira-project nil
    "Default JIRA project key.
Example: \"PROJ\""))

;; ============================================================================
;; Integration Settings - Compilation
;; ============================================================================

(unless (boundp 'omegamacs-compile-mode-shell-history-file)
  (defvar omegamacs-compile-mode-shell-history-file nil
    "Path to shell history file for compile mode integration.
When nil, a default path will be computed based on the current shell."))

(unless (boundp 'omegamacs-compile-mode-shell-history-size)
  (defvar omegamacs-compile-mode-shell-history-size 100
    "Number of shell history entries to keep in compile mode."))

;; ============================================================================
;; Integration Settings - Projectile
;; ============================================================================

(unless (boundp 'omegamacs-settings-projectile-generic-command)
  (defvar omegamacs-settings-projectile-generic-command nil
    "Custom command for projectile file listing.
When nil, projectile will use its default command:
\"git ls-files -zco --exclude-standard 2>/dev/null || find . -type f -print0\""))

;; ============================================================================
;; Org Mode Settings
;; ============================================================================

(unless (boundp 'omegamacs--org-dir)
  (defvar omegamacs--org-dir nil
    "Base directory for org files.
When nil, defaults to ~/org/"))

(unless (boundp 'omegamacs--org-file-inbox)
  (defvar omegamacs--org-file-inbox nil
    "Path to org inbox file.
When nil, computed from omegamacs--org-dir."))

(unless (boundp 'omegamacs--org-file-projects)
  (defvar omegamacs--org-file-projects nil
    "Path to org projects file.
When nil, computed from omegamacs--org-dir."))

(unless (boundp 'omegamacs--org-file-ideas)
  (defvar omegamacs--org-file-ideas nil
    "Path to org ideas file.
When nil, computed from omegamacs--org-dir."))

(unless (boundp 'omegamacs--org-file-recurring)
  (defvar omegamacs--org-file-recurring nil
    "Path to org recurring tasks file.
When nil, computed from omegamacs--org-dir."))

(unless (boundp 'omegamacs--org-file-archive)
  (defvar omegamacs--org-file-archive nil
    "Path to org archive file.
When nil, computed from omegamacs--org-dir."))

;;; defaults.el ends here
