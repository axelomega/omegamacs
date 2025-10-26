;;; defaults.el --- Default values for all omegamacs configuration variables -*- lexical-binding: t; -*-

;; This file is part of omegamacs and is loaded very early in the
;; initialization process. It defines default values for all omegamacs
;; configuration variables so that the rest of the codebase can assume
;; these variables are always bound.
;;
;; IMPORTANT: Users can override these defaults by setting variables in their
;; ~/.emacs.d/init.el BEFORE loading emacs_init.el. Since `defvar` does NOT
;; override variables that are already bound, user settings via `setq` will
;; be preserved. The docstrings and special variable declarations will still
;; be applied.
;;
;; NOTE: omegamacs-emacs-config-dir is set in emacs_init.el, not here, since
;; it must be determined before loading this file.

;;; Code:

;; ============================================================================
;; System and Path Variables
;; ============================================================================

(defvar omegamacs-user-emacs-directory-local
  user-emacs-directory
  "Directory for storing local Emacs data (cache, packages, etc.).
This can be set to a different location to keep user data separate
from the configuration directory.")

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

(defvar omegamacs-minimal-config
  (member "--minimal" command-line-args)
  "When non-nil, load only essential configuration.
Set automatically based on the --minimal command-line flag.")

(defvar omegamacs-enable-lazy-loading
  (not (member "--no-defer" command-line-args))
  "When non-nil, enable lazy loading with :defer keyword.
Set automatically based on the --no-defer command-line flag.")

;; ============================================================================
;; Display and Editor Settings
;; ============================================================================

(defvar omegamacs-fill-column 120
  "Default fill column width for text wrapping.")

(defvar omegamacs-theme-current 'dark
  "Current active theme.
This variable is also defined as a defcustom in theme.el for the
customize interface, but we provide a default here for early initialization.")

(defvar omegamacs-indent-style 'space
  "Default indentation style for code editing.
This is used as the fallback when a mode is not specified in
`omegamacs-indent-style-alist'.
Valid values:
  'space - Use spaces for indentation (default)
  'tab   - Use tabs for indentation")

(defvar omegamacs-indent-style-alist nil
  "Alist mapping major modes to their preferred indentation style.
Each entry is (MODE . STYLE) where MODE is a major mode symbol and
STYLE is either 'space or 'tab.

Modes not listed here will use `omegamacs-indent-style' as the default.

Example:
  ((makefile-mode . tab)
   (python-mode . space)
   (go-mode . tab)
   (c-mode . space))")

(defvar omegamacs-indent-amount 4
  "Default number of spaces (or tab width) for each indentation level.
This is used as the fallback when a mode is not specified in
`omegamacs-indent-amount-alist'.
Default: 4")

(defvar omegamacs-indent-amount-alist
  '((emacs-lisp-mode . 2)
    (lisp-mode . 2)
    (LaTeX-mode . 2))
  "Alist mapping major modes to their preferred indentation amounts.
Each entry is (MODE . AMOUNT) where MODE is a major mode symbol and
AMOUNT is the number of spaces for indentation in that mode.

Modes not listed here will use `omegamacs-indent-amount' as the default.

Example:
  ((emacs-lisp-mode . 2)
   (python-mode . 4)
   (c-mode . 4)
   (verilog-mode . 4))")

(defun omegamacs-get-indent-amount (mode)
  "Get indentation amount for MODE from alist, falling back to default.
MODE should be a major mode symbol (e.g., 'emacs-lisp-mode).
Returns the indentation amount from `omegamacs-indent-amount-alist' if found,
otherwise returns `omegamacs-indent-amount'."
  (or (alist-get mode omegamacs-indent-amount-alist)
      omegamacs-indent-amount))

(defun omegamacs-get-indent-style (mode)
  "Get indentation style for MODE from alist, falling back to default.
MODE should be a major mode symbol (e.g., 'python-mode).
Returns the indentation style from `omegamacs-indent-style-alist' if found,
otherwise returns `omegamacs-indent-style'.
Result is either 'space or 'tab."
  (or (alist-get mode omegamacs-indent-style-alist)
      omegamacs-indent-style))

;; ============================================================================
;; Feature Flags
;; ============================================================================

(defvar omegamacs-parenthesis-autocomplete-enable nil
  "When non-nil, enable automatic parenthesis/bracket completion.
This affects electric-pair-mode, smartparens, and various language-specific
pairing behaviors in LaTeX (cdlatex) and other modes.")

(defvar omegamacs-copilot-config nil
  "Copilot integration configuration.
Valid values:
  nil    - No copilot integration
  'setup - Load basic copilot setup
  'full  - Load full copilot configuration")

;; ============================================================================
;; Integration Settings - JIRA
;; ============================================================================

(defvar omegamacs-settings-jira-url nil
  "JIRA server URL for issue tracking integration.
Example: \"https://jira.example.com\"")

(defvar omegamacs-settings-jira-username nil
  "JIRA username for authentication.")

(defvar omegamacs-settings-jira-project nil
  "Default JIRA project key.
Example: \"PROJ\"")

;; ============================================================================
;; Integration Settings - Compilation
;; ============================================================================

(defvar omegamacs-compile-mode-shell-history-file nil
  "Path to shell history file for compile mode integration.
When nil, a default path will be computed based on the current shell.")

(defvar omegamacs-compile-mode-shell-history-size 100
  "Controls how many lines of shell history to read for compile mode integration.
Special values:
  nil - never read shell history
  0   - read the full shell history
Any positive integer N - read the last N lines of shell history.")

;; ============================================================================
;; Integration Settings - Projectile
;; ============================================================================

(defvar omegamacs-settings-projectile-generic-command nil
  "Custom command for projectile file listing.
When nil, projectile will use its default command:
\"git ls-files -zco --exclude-standard 2>/dev/null || find . -type f -print0\"")

;; ============================================================================
;; Org Mode Settings
;; ============================================================================

(defvar omegamacs--org-dir nil
  "Base directory for org files.
When nil, defaults to ~/org/")

(defvar omegamacs--org-file-inbox nil
  "Path to org inbox file.
When nil, computed from omegamacs--org-dir.")

(defvar omegamacs--org-file-projects nil
  "Path to org projects file.
When nil, computed from omegamacs--org-dir.")

(defvar omegamacs--org-file-ideas nil
  "Path to org ideas file.
When nil, computed from omegamacs--org-dir.")

(defvar omegamacs--org-file-recurring nil
  "Path to org recurring tasks file.
When nil, computed from omegamacs--org-dir.")

(defvar omegamacs--org-file-archive nil
  "Path to org archive file.
When nil, computed from omegamacs--org-dir.")

;;; defaults.el ends here
