;;; -*- lexical-binding: t -*-
;;; theme.el --- Color-focused theme support for omegamacs

;;; Commentary:
;; This module provides a clean, color-focused theme system that:
;; - Defines color palettes in a central location
;; - Provides utilities for accessing theme colors
;; - Supports easy theme switching
;; - Separates color definitions from package-specific settings
;; - Allows other configuration files to reference theme colors

;;; Code:

(defgroup omegamacs-theme nil
  "Color-focused theme support for omegamacs."
  :group 'omegamacs
  :prefix "omegamacs-theme-")

(defcustom omegamacs-theme-current 'dark
  "Current active theme."
  :type '(choice (const :tag "Dark Theme" dark)
                 (const :tag "Light Theme" light)
                 (const :tag "Mid Gray Theme" mid-gray)
                 (const :tag "Dark X11 Theme" dark-x11)
                 (const :tag "Classic Theme" classic))
  :group 'omegamacs-theme
  :set (lambda (symbol value)
         (set-default symbol value)
         (when (fboundp 'omegamacs-theme-apply)
           (omegamacs-theme-apply value))))

(defvar omegamacs-theme--color-palettes
  '((dark . ((background . "#2e2e2e")
             (background-alt . "#1a1a1a")
             (background-light . "#3a3a3a")
             (background-lighter . "#4a4a4a")
             (foreground . "#cccccc")
             (foreground-alt . "#999999")
             (foreground-dim . "#666666")
             (cursor . "#00cccc")
             (region . "#4a4a4a")
             (highlight . "#1a1a1a")
             (line-number . "#666666")
             (line-number-current . "#ffffff")
             (line-number-bg . "#363636")
             (line-number-current-bg . "#404040")
             (mode-line-bg . "#262626")
             (mode-line-fg . "#ffffff")
             (mode-line-inactive-bg . "#404040")
             (mode-line-inactive-fg . "#999999")
             (fringe . "#2e2e2e")
             (border . "#666666")
             (minibuffer-prompt . "#00cccc")
             (link . "#00cccc")
             (trailing-whitespace . "#262626")
             (show-paren-match . "#666666")
             ;; Syntax highlighting colors
             (comment . "#999999")
             (string . "#90ee90")
             (keyword . "#add8e6")
             (function-name . "#ffffe0")
             (variable-name . "#e0ffff")
             (type . "#dda0dd")
             (constant . "#ffa500")
             ;; UI element colors
             (success . "#90ee90")
             (warning . "#ffb347")
             (error . "#ff6b6b")
             (info . "#87ceeb")
             ;; Indent guide colors
             (indent-guide-odd . "#666666")
             (indent-guide-even . "#4a4a4a")
             (indent-guide-char . "#000000")
             (indent-guide-top . "#000000")
             (indent-guide-stack . "#000000")
             ;; Diff colors
             (diff-added . "#90ee90")
             (diff-removed . "#ff6b6b")
             (diff-changed . "#dda0dd")
             ;; Company/completion colors
             (completion-bg . "#404040")
             (completion-fg . "#cccccc")
             (completion-selection . "#666666")
             (completion-annotation . "#999999")))

    (light . ((background . "#ffffff")
              (background-alt . "#f8f8f8")
              (background-light . "#f0f0f0")
              (background-lighter . "#e8e8e8")
              (foreground . "#000000")
              (foreground-alt . "#666666")
              (foreground-dim . "#999999")
              (cursor . "#0066cc")
              (region . "#add8e6")
              (highlight . "#ffffe0")
              (line-number . "#666666")
              (line-number-current . "#000000")
              (line-number-bg . "#f0f0f0")
              (line-number-current-bg . "#ffffe0")
              (mode-line-bg . "#d3d3d3")
              (mode-line-fg . "#000000")
              (mode-line-inactive-bg . "#e5e5e5")
              (mode-line-inactive-fg . "#666666")
              (fringe . "#ffffff")
              (border . "#cccccc")
              (minibuffer-prompt . "#0066cc")
              (link . "#0066cc")
              (trailing-whitespace . "#ffb6c1")
              (show-paren-match . "#add8e6")
              ;; Syntax highlighting colors
              (comment . "#666666")
              (string . "#006400")
              (keyword . "#0066cc")
              (function-name . "#8b0000")
              (variable-name . "#008b8b")
              (type . "#8b008b")
              (constant . "#ff8c00")
              ;; UI element colors
              (success . "#006400")
              (warning . "#ff8c00")
              (error . "#dc143c")
              (info . "#4682b4")
              ;; Indent guide colors
              (indent-guide-odd . "#d5d5d5")
              (indent-guide-even . "#e8e8e8")
              (indent-guide-char . "#b3b3b3")
              (indent-guide-top . "#b3b3b3")
              (indent-guide-stack . "#b3b3b3")
              ;; Diff colors
              (diff-added . "#006400")
              (diff-removed . "#dc143c")
              (diff-changed . "#8b008b")
              ;; Company/completion colors
              (completion-bg . "#f8f8f8")
              (completion-fg . "#000000")
              (completion-selection . "#d3d3d3")
              (completion-annotation . "#666666")))

    (mid-gray . ((background . "#666666")
                 (background-alt . "#555555")
                 (background-light . "#777777")
                 (background-lighter . "#888888")
                 (foreground . "#eeeeee")
                 (foreground-alt . "#cccccc")
                 (foreground-dim . "#aaaaaa")
                 (cursor . "#00ffff")
                 (region . "#777777")
                 (highlight . "#555555")
                 (line-number . "#aaaaaa")
                 (line-number-current . "#ffffff")
                 (line-number-bg . "#606060")
                 (line-number-current-bg . "#707070")
                 (mode-line-bg . "#505050")
                 (mode-line-fg . "#ffffff")
                 (mode-line-inactive-bg . "#707070")
                 (mode-line-inactive-fg . "#cccccc")
                 (fringe . "#666666")
                 (border . "#999999")
                 (minibuffer-prompt . "#00ffff")
                 (link . "#66aaff")
                 (comment . "#bbbbbb")
                 (string . "#88dd88")
                 (keyword . "#ffaa88")
                 (function-name . "#ffdd66")
                 (variable-name . "#aaccff")
                 (type . "#ddaa77")
                 (constant . "#ff9999")
                 (success . "#88cc88")
                 (warning . "#ffcc66")
                 (error . "#ff6666")
                 (info . "#66ccff")
                 (trailing-whitespace . "#994444")
                 (show-paren-match . "#888888")
                 (indent-guide-odd . "#777777")
                 (indent-guide-even . "#777777")
                 (indent-guide-char . "#777777")
                 (indent-guide-top . "#999999")
                 (indent-guide-stack . "#999999")
                 (diff-added . "#448844")
                 (diff-removed . "#884444")
                 (diff-changed . "#886644")
                 (completion-bg . "#888888")
                 (completion-fg . "#eeeeee")
                 (completion-selection . "#777777")
                 (completion-annotation . "#aaaaaa")))

    (dark-x11 . ((background . "gray20")
                 (background-alt . "gray10")
                 (background-light . "gray25")
                 (background-lighter . "gray30")
                 (foreground . "gainsboro")
                 (foreground-alt . "lightgray")
                 (foreground-dim . "dimgray")
                 (cursor . "cyan")
                 (region . "gray30")
                 (highlight . "gray10")
                 (line-number . "dimgray")
                 (line-number-current . "white")
                 (line-number-bg . "gray25")
                 (line-number-current-bg . "gray30")
                 (mode-line-bg . "gray15")
                 (mode-line-fg . "white")
                 (mode-line-inactive-bg . "gray30")
                 (mode-line-inactive-fg . "lightgray")
                 (fringe . "gray20")
                 (border . "dimgray")
                 (minibuffer-prompt . "cyan")
                 (link . "lightblue")
                 (trailing-whitespace . "darkred")
                 (show-paren-match . "gray30")
                 ;; Syntax highlighting colors
                 (comment . "lightgray")
                 (string . "lightgreen")
                 (keyword . "lightyellow")
                 (function-name . "yellow")
                 (variable-name . "lightcyan")
                 (type . "orange")
                 (constant . "lightcoral")
                 ;; UI element colors
                 (success . "green")
                 (warning . "orange")
                 (error . "red")
                 (info . "lightblue")
                 ;; Indent guide colors
                 (indent-guide-odd . "gray40")
                 (indent-guide-even . "gray40")
                 (indent-guide-char . "gray40")
                 (indent-guide-top . "gray50")
                 (indent-guide-stack . "gray50")
                 ;; Diff colors
                 (diff-added . "darkgreen")
                 (diff-removed . "darkred")
                 (diff-changed . "darkorange")
                 ;; Company/completion colors
                 (completion-bg . "gray25")
                 (completion-fg . "gainsboro")
                 (completion-selection . "gray30")
                 (completion-annotation . "lightgray")))

    (classic . ((background . "gray20")
                (background-alt . "gray20")
                (background-light . "gray25")
                (background-lighter . "gray30")
                (foreground . "gray80")
                (foreground-alt . "gray70")
                (foreground-dim . "gray60")
                (cursor . "cyan")
                (region . "gray30")
                (highlight . "Gray10")
                (line-number . "gray60")
                (line-number-current . "white")
                (line-number-bg . "gray20")
                (line-number-current-bg . "gray25")
                (mode-line-bg . "grey75")
                (mode-line-fg . "black")
                (mode-line-inactive-bg . "grey30")
                (mode-line-inactive-fg . "grey20")
                (fringe . "gray20")
                (border . "gray60")
                (minibuffer-prompt . "cyan")
                (link . "lightblue")
                (trailing-whitespace . "gray15")
                (show-paren-match . "gray30")
                ;; Syntax highlighting (matching Emacs defaults on gray20 background)
                (comment . "darkorange")
                (string . "palegreen")
                (keyword . "cyan")
                (function-name . "blue")
                (variable-name . "yellow")
                (type . "green")
                (constant . "magenta")
                ;; UI element colors
                (success . "green")
                (warning . "orange")
                (error . "red")
                (info . "lightblue")
                ;; Indent guide colors (matching original)
                (indent-guide-odd . "black")
                (indent-guide-even . "black")
                (indent-guide-char . "black")
                (indent-guide-top . "black")
                (indent-guide-stack . "black")
                ;; Diff colors
                (diff-added . "darkgreen")
                (diff-removed . "darkred")
                (diff-changed . "darkmagenta")
                ;; Company/completion colors
                (completion-bg . "gray40")
                (completion-fg . "white")
                (completion-selection . "gray30")
                (completion-annotation . "gray70"))))
  "Color palettes for different themes.
Each theme is an alist mapping color names to hex color values.")

(defvar omegamacs-theme--change-hooks nil
  "List of functions to call when theme changes.
Each function is called with the new theme name as argument.")

;;; Color Access Functions

(defun omegamacs-theme-color (color-name &optional theme)
  "Get the color value for COLOR-NAME in the current or specified THEME.
COLOR-NAME should be a symbol representing the color.
THEME defaults to the current theme."
  (let* ((theme (or theme omegamacs-theme-current))
         (palette (cdr (assq theme omegamacs-theme--color-palettes))))
    (cdr (assq color-name palette))))

(defun omegamacs-theme-colors (&optional theme)
  "Get the complete color palette for THEME.
Returns an alist of (color-name . color-value) pairs.
THEME defaults to the current theme."
  (let ((theme (or theme omegamacs-theme-current)))
    (cdr (assq theme omegamacs-theme--color-palettes))))

(defmacro omegamacs-theme-with-colors (theme &rest body)
  "Execute BODY with color variables bound from THEME.
Each color in the theme palette becomes a local variable.
For example, 'background becomes the background color value."
  (declare (indent 1))
  ;; Get all unique color names from all themes at macro expansion time
  (let ((all-color-names (delete-dups
                          (apply #'append
                                 (mapcar (lambda (theme-def)
                                           (mapcar #'car (cdr theme-def)))
                                         omegamacs-theme--color-palettes)))))
    `(let* ((colors (omegamacs-theme-colors ,theme))
            ,@(mapcar (lambda (color-name)
                        `(,color-name (cdr (assq ',color-name colors))))
                      all-color-names))
       ,@body)))

;;; Theme Management Functions

;; TODO: Theme switching sometimes requires cursor movement to fully refresh display.
;; This suggests a timing or redraw issue that should be investigated further.
(defun omegamacs-theme-apply (theme)
  "Apply THEME as the current theme.
This updates the current theme and runs all change hooks."
  (interactive (list (intern (completing-read "Apply theme: "
                                              (mapcar #'symbol-name
                                                      (mapcar #'car omegamacs-theme--color-palettes))
                                              nil t))))
  (setq omegamacs-theme-current theme)
  (run-hook-with-args 'omegamacs-theme--change-hooks theme)
  ;; Use a timer to ensure all theme changes are processed before redrawing
  (redraw-display)
  (force-mode-line-update t)
  (message "Applied theme: %s" theme))

(defun omegamacs-theme-cycle ()
  "Cycle through available themes automatically based on defined themes."
  (interactive)
  (let* ((available-themes (mapcar #'car omegamacs-theme--color-palettes))
         (current-index (or (cl-position omegamacs-theme-current available-themes) 0))
         (next-index (mod (1+ current-index) (length available-themes)))
         (next-theme (nth next-index available-themes)))
    (omegamacs-theme-apply next-theme)))

(defun omegamacs-theme-reload ()
  "Reload the current theme.
This re-applies the current theme, useful after color palette changes."
  (interactive)
  (omegamacs-theme-apply omegamacs-theme-current))

;;; Hook Registration

(defun omegamacs-theme-add-hook (function)
  "Add FUNCTION to be called when theme changes.
FUNCTION should accept a theme name as its argument."
  (add-hook 'omegamacs-theme--change-hooks function))

(defun omegamacs-theme-remove-hook (function)
  "Remove FUNCTION from theme change hooks."
  (remove-hook 'omegamacs-theme--change-hooks function))

;;; Color Utilities

(defun omegamacs-theme-color-exists-p (color-name &optional theme)
  "Check if COLOR-NAME exists in THEME.
THEME defaults to the current theme."
  (not (null (omegamacs-theme-color color-name theme))))

(defun omegamacs-theme-color-with-fallback (color-name fallback-color &optional theme)
  "Get COLOR-NAME from THEME, or FALLBACK-COLOR if it doesn't exist.
THEME defaults to the current theme."
  (or (omegamacs-theme-color color-name theme) fallback-color))

(defun omegamacs-theme--color-to-hex (color)
  "Convert COLOR (hex or named) to hex string #RRGGBB.
Returns nil if COLOR is invalid."
  (cond
   ;; Already a hex color (#RRGGBB)
   ((and (stringp color)
         (string-match "^#\\([[:xdigit:]]\\{6\\}\\)$" color))
    color)
   ;; Named color: use color-name-to-rgb and color-rgb-to-hex
   ((and (stringp color)
         (color-defined-p color))
    (apply #'color-rgb-to-hex (color-name-to-rgb color)))
   (t nil)))

(defun omegamacs-theme-interpolate-color (color1 color2 factor)
  "Interpolate between COLOR1 and COLOR2 by FACTOR (0.0 to 1.0).
Returns a hex color string."
  ;; Convert both colors to hex
  (let* ((hex1 (omegamacs-theme--color-to-hex color1))
         (hex2 (omegamacs-theme--color-to-hex color2)))
    (if (and hex1 hex2)
        (let ((r1 (string-to-number (substring hex1 1 3) 16))
              (g1 (string-to-number (substring hex1 3 5) 16))
              (b1 (string-to-number (substring hex1 5 7) 16))
              (r2 (string-to-number (substring hex2 1 3) 16))
              (g2 (string-to-number (substring hex2 3 5) 16))
              (b2 (string-to-number (substring hex2 5 7) 16)))
          (format "#%02x%02x%02x"
                  (round (+ r1 (* factor (- r2 r1))))
                  (round (+ g1 (* factor (- g2 g1))))
                  (round (+ b1 (* factor (- b2 b1))))))
      ;; If either color is invalid, return nil
      nil)))

;;; Built-in Face Configuration

(defun omegamacs-theme--apply-builtin-faces (theme)
  "Apply THEME colors to built-in Emacs faces."
  (omegamacs-theme-with-colors theme
    ;; Basic faces
    (set-face-attribute 'default nil
                        :foreground foreground
                        :background background)
    (set-face-attribute 'cursor nil
                        :background cursor)
    (set-face-attribute 'region nil
                        :background region)
    (set-face-attribute 'highlight nil
                        :background highlight)
    (when (facep 'hl-line)
      (set-face-attribute 'hl-line nil
                          :background highlight))

    ;; Mode line
    (set-face-attribute 'mode-line nil
                        :foreground mode-line-fg
                        :background mode-line-bg
                        :box `(:line-width 1 :color ,border))
    (set-face-attribute 'mode-line-inactive nil
                        :foreground mode-line-inactive-fg
                        :background mode-line-inactive-bg
                        :box `(:line-width 1 :color ,border))

    ;; Line numbers
    (when (facep 'line-number)
      (set-face-attribute 'line-number nil
                          :foreground line-number
                          :background line-number-bg)
      (set-face-attribute 'line-number-current-line nil
                          :foreground line-number-current
                          :background line-number-current-bg
                          :weight 'bold))

    ;; Fringe and borders
    (set-face-attribute 'fringe nil
                        :background fringe)

    ;; Interactive elements
    (set-face-attribute 'minibuffer-prompt nil
                        :foreground minibuffer-prompt
                        :weight 'bold)
    (set-face-attribute 'link nil
                        :foreground link
                        :underline t)

    ;; Syntax highlighting
    (set-face-attribute 'font-lock-comment-face nil
                        :foreground comment
                        :slant 'italic)
    (set-face-attribute 'font-lock-string-face nil
                        :foreground string)
    (set-face-attribute 'font-lock-keyword-face nil
                        :foreground keyword
                        :weight 'bold)
    (set-face-attribute 'font-lock-function-name-face nil
                        :foreground function-name)
    (set-face-attribute 'font-lock-variable-name-face nil
                        :foreground variable-name)
    (set-face-attribute 'font-lock-type-face nil
                        :foreground type)
    (set-face-attribute 'font-lock-constant-face nil
                        :foreground constant)

    ;; Special highlighting
    (set-face-attribute 'trailing-whitespace nil
                        :background trailing-whitespace)
    (set-face-attribute 'show-paren-match nil
                        :background show-paren-match
                        :weight 'bold)))

;;; Theme Hydra

(defhydra hydra-theme (:color pink :hint nil)
  "
Theme: _c_ycle   _d_ark   _l_ight   _m_id-gray   _x_11-dark   cla_s_sic   _r_eload   _q_uit
"
  ("c" omegamacs-theme-cycle "cycle")
  ("d" (omegamacs-theme-apply 'dark) "dark")
  ("l" (omegamacs-theme-apply 'light) "light")
  ("m" (omegamacs-theme-apply 'mid-gray) "mid-gray")
  ("x" (omegamacs-theme-apply 'dark-x11) "x11-dark")
  ("s" (omegamacs-theme-apply 'classic) "classic")
  ("r" omegamacs-theme-reload "reload")
  ("q" nil "quit" :exit t))

;; Key Binding
(global-set-key (kbd "C-c T") 'hydra-theme/body)

;;; Initialization

(defun omegamacs-theme--initialize ()
  "Initialize the theme system."
  ;; Register built-in face handler
  (omegamacs-theme-add-hook #'omegamacs-theme--apply-builtin-faces)

  ;; Apply the current theme
  (omegamacs-theme-apply omegamacs-theme-current))

;; Initialize when this file is loaded
(omegamacs-theme--initialize)
