# Omegamacs Theme System

## Architecture and Rationale

### Code as Config Philosophy

The Omegamacs theme system follows the core "code as config" philosophy - no abstractions, no magic, just transparent Emacs Lisp code you can read and understand.

**Design Principles:**
- **Colors defined once** in `theme.el` as simple alist data structures
- **Package configurations remain visible** - color settings stay in their respective files
- **Direct function calls** - no framework abstractions or custom DSLs
- **Explicit dependencies** - each file clearly shows what theme colors it uses
- **Real Emacs Lisp** - uses standard face manipulation and hook functions

### Architecture Overview

The system has three components:

1. **Color definitions** (`theme.el`) - Simple alists mapping semantic names to color values
2. **Integration functions** (package files) - Standard Emacs Lisp functions that reference theme colors
3. **Hook system** - Standard Emacs hooks that update colors when themes change

**No abstractions, no magic** - just data structures and function calls.

## Theme System Structure

### Color Palette Format

```elisp
(defvar omegamacs-theme--color-palettes
  '((theme-name . ((background . "#2e2e2e")
                   (foreground . "#cccccc")
                   (keyword . "#66d9ef")
                   ;; ... more colors
                   ))))
```

Each theme is an alist. Color names are symbols, color values are strings (hex or X11 names).

### Integration Pattern

Package files use this standard pattern:

```elisp
;; Define function that applies theme colors
(defun package--apply-colors (theme)
  "Apply theme colors to package faces."
  (omegamacs-theme-with-colors theme
    (set-face-attribute 'some-face nil
                        :background background
                        :foreground keyword)))

;; Register function to run when theme changes
(omegamacs-theme-add-hook #'package--apply-colors)

;; Apply colors for current theme
(package--apply-colors omegamacs-theme-current)
```

This keeps color settings visible in package files while referencing centralized color definitions.

## Available Themes

1. **`dark`** - Modern dark theme with hex colors
2. **`light`** - Clean light theme with high contrast
3. **`mid-gray`** - Medium gray theme balancing dark and light
4. **`dark-x11`** - Terminal-friendly theme using X11 named colors
5. **`classic`** - Recreates the original omegamacs color scheme

## Theme Management

### Theme Switching

Use **`C-c T`** to open the theme hydra:

- `c` - Cycle through all themes automatically
- `d` - Apply dark theme
- `l` - Apply light theme
- `m` - Apply mid-gray theme
- `x` - Apply dark-x11 theme
- `s` - Apply classic theme
- `r` - Reload current theme

### Core API Functions

```elisp
;; Get a color from current theme
(omegamacs-theme-color 'background)

;; Get a color from specific theme
(omegamacs-theme-color 'foreground 'light)

;; Get all colors for a theme
(omegamacs-theme-colors 'dark)

;; Apply specific theme
(omegamacs-theme-apply 'dark)

;; Cycle through all themes
(omegamacs-theme-cycle)
```

### Theme Color Macro

The `omegamacs-theme-with-colors` macro binds theme colors as local variables:

```elisp
(omegamacs-theme-with-colors theme
  ;; Colors available as variables: background, foreground, keyword, etc.
  (set-face-attribute 'my-face nil
                      :background background
                      :foreground keyword))
```

The macro dynamically discovers all color names from theme definitions at compile time.

## Color Vocabulary

Each theme defines these semantic colors:

**Backgrounds:** `background`, `background-alt`, `background-light`, `background-lighter`

**Text:** `foreground`, `foreground-alt`, `foreground-dim`

**UI Elements:** `cursor`, `region`, `highlight`, `mode-line-bg`, `mode-line-fg`, `minibuffer-prompt`, `link`

**Syntax Highlighting:** `comment`, `string`, `keyword`, `function-name`, `variable-name`, `type`, `constant`

**Status:** `success`, `warning`, `error`, `info`

**Special Purpose:** `trailing-whitespace`, `show-paren-match`, `indent-guide-odd/even/char/top/stack`, `diff-added/removed/changed`, `completion-bg/fg/selection/annotation`

## Integration Examples

### Basic Integration

```elisp
(defun my-package--apply-colors (theme)
  "Apply theme colors to package faces."
  (omegamacs-theme-with-colors theme
    (set-face-attribute 'my-face nil
                        :background background
                        :foreground keyword)))

(omegamacs-theme-add-hook #'my-package--apply-colors)
(my-package--apply-colors omegamacs-theme-current)
```

### Use-Package Integration

```elisp
(use-package my-package
  :config
  (defun my-package--theme-colors (theme)
    (omegamacs-theme-with-colors theme
      (set-face-attribute 'my-package-face nil
                          :background background-alt
                          :foreground foreground)))

  (omegamacs-theme-add-hook #'my-package--theme-colors)
  (my-package--theme-colors omegamacs-theme-current))
```

### With eval-after-load

```elisp
(with-eval-after-load 'package-name
  (defun my-package--configure-colors (theme)
    (omegamacs-theme-with-colors theme
      (set-face-attribute 'some-face nil :foreground comment)))

  (omegamacs-theme-add-hook #'my-package--configure-colors)
  (my-package--configure-colors omegamacs-theme-current))
```

## Adding New Themes

Extend the color palette in `theme.el`:

```elisp
(setq omegamacs-theme--color-palettes
  (append omegamacs-theme--color-palettes
    '((my-theme . ((background . "#123456")
                   (foreground . "#abcdef")
                   ;; ... define all semantic colors
                   )))))
```

New themes automatically:
- Appear in theme cycling
- Work with interactive completion
- Integrate with all existing configurations

**Note:** The hydra interface (`C-c T`) must be manually updated if you want a dedicated key for the new theme. Theme cycling and `M-x omegamacs-theme-apply` will work immediately.

No code changes needed in management functions.

## Current Implementation Status

Files using the theme system:

- **`theme.el`** - Core system and built-in Emacs faces
- **`settings.el`** - Basic UI colors (trailing-whitespace, hl-line, frame colors)
- **`programming.el`** - which-func and highlight-indent-guides colors
- **`languages/python.el`** - eglot header-line colors

Each file maintains its color configuration code visibly, but references centralized theme colors.

## Implementation Notes

### Dynamic System
Theme cycling automatically discovers themes from the palette data structure. Adding themes requires no code changes to management functions.

### Terminal Compatibility
The `dark-x11` theme uses X11 named colors for reliable terminal display where hex colors might not work.

### Classic Theme
Recreates original omegamacs colors: gray20 background, gray80 foreground, cyan cursor, gray mode line, orange comments.

### Redraw Behavior
Theme switching includes display refresh. Occasionally cursor movement needed for complete updates (documented issue on Emacs 28+ X11).

### Color Interpolation
Includes utility functions for color manipulation that work with both hex and X11 color formats.

## Benefits of This Approach

**Transparency:** All color logic is visible Emacs Lisp code in the files where it's used.

**Maintainability:** Color changes in one place automatically propagate through the entire configuration.

**No Lock-in:** Standard Emacs functions and hooks - no framework dependencies to become obsolete.

**Extensibility:** Adding themes or integrating new packages follows clear, documented patterns.

**Learning:** Users see real Emacs Lisp color manipulation, not abstractions.
