# Omegamacs Color-Focused Theme System

## Overview

The Omegamacs theme system provides a centralized, color-focused approach to managing themes across the entire Emacs configuration. The system separates color definitions from package-specific settings, enabling easy theme switching and consistent visual experience.

## Key Design Principles

- **Colors defined centrally** in `theme.el`
- **Package configurations reference theme colors** instead of hard-coding them
- **Automatic updates** when switching themes
- **Clean separation** between color definitions and package settings

## Available Themes

The system currently includes 5 themes:

1. **`dark`** - Modern dark theme with hex colors
2. **`light`** - Clean light theme with high contrast
3. **`mid-gray`** - Medium gray theme balancing dark and light
4. **`dark-x11`** - Dark theme using X11 named colors (terminal-friendly)
5. **`classic`** - Recreates the original omegamacs color scheme

## Theme Management

### Theme Switching

Access themes via the convenient hydra: **`C-c T`**

```
Theme: _c_ycle   _d_ark   _l_ight   _m_id-gray   _x_11-dark   cla_s_sic   _r_eload   _q_uit
```

- **`c`** - Cycle through all themes automatically
- **`d`** - Apply dark theme
- **`l`** - Apply light theme
- **`m`** - Apply mid-gray theme
- **`x`** - Apply dark-x11 theme
- **`s`** - Apply classic theme
- **`r`** - Reload current theme
- **`q`** - Quit hydra

### Theme Cycling

The system automatically discovers all available themes and cycles through them in order. Adding new themes requires no code changes to the cycling function.

## Core API

### Color Access Functions

```elisp
;; Get a color from current theme
(omegamacs-theme-color 'background)

;; Get a color from specific theme
(omegamacs-theme-color 'foreground 'light)

;; Get all colors for a theme
(omegamacs-theme-colors 'dark)
```

### Theme Color Macro

```elisp
(omegamacs-theme-with-colors theme
  ;; Use color names directly as variables
  (set-face-attribute 'my-face nil 
                      :background background 
                      :foreground keyword))
```

### Theme Management Functions

```elisp
;; Apply specific theme
(omegamacs-theme-apply 'dark)

;; Cycle through all themes
(omegamacs-theme-cycle)

;; Reload current theme
(omegamacs-theme-reload)
```

### Hook System

```elisp
;; Register function to be called on theme changes
(omegamacs-theme-add-hook #'my-theme-handler)

;; Remove theme change handler
(omegamacs-theme-remove-hook #'my-theme-handler)
```

## Color Palette Structure

Each theme defines semantic color names:

### Backgrounds
- `background` - Main background
- `background-alt` - Alternative/darker background  
- `background-light` - Lighter background variant
- `background-lighter` - Even lighter background

### Foregrounds
- `foreground` - Main text color
- `foreground-alt` - Alternative text color
- `foreground-dim` - Dimmed text color

### UI Elements
- `cursor` - Cursor color
- `region` - Selected region background
- `highlight` - Highlight background (hl-line, etc.)
- `mode-line-bg/fg` - Mode line colors
- `minibuffer-prompt` - Minibuffer prompt color
- `link` - Link color

### Syntax Highlighting
- `comment` - Comment text
- `string` - String literals
- `keyword` - Language keywords
- `function-name` - Function names
- `variable-name` - Variable names
- `type` - Type names
- `constant` - Constants

### Status Colors
- `success` - Success indicators
- `warning` - Warning indicators  
- `error` - Error indicators
- `info` - Information indicators

### Special Purpose
- `trailing-whitespace` - Trailing whitespace background
- `show-paren-match` - Matching parenthesis highlight
- `indent-guide-normal/current` - Indent guide colors
- `diff-added/removed/changed` - Diff colors

## Integration Patterns

### Basic Package Integration

```elisp
;; Define theme color handler
(defun my-package--apply-colors (theme)
  "Apply theme colors to my-package faces."
  (omegamacs-theme-with-colors theme
    (set-face-attribute 'my-face nil 
                        :background background 
                        :foreground keyword)))

;; Register with theme system
(omegamacs-theme-add-hook #'my-package--apply-colors)

;; Apply current theme immediately
(my-package--apply-colors omegamacs-theme-current)
```

### Use-Package Integration

```elisp
(use-package my-package
  :ensure t
  :config
  ;; Package configuration...
  
  ;; Theme integration
  (defun my-package--theme-colors (theme)
    (omegamacs-theme-with-colors theme
      (set-face-attribute 'my-package-face nil
                          :background background-alt
                          :foreground foreground)))
  
  ;; Register and apply
  (omegamacs-theme-add-hook #'my-package--theme-colors)
  (my-package--theme-colors omegamacs-theme-current))
```

### With eval-after-load

```elisp
(with-eval-after-load 'package-name
  (defun my-package--configure-colors (theme)
    (omegamacs-theme-with-colors theme
      ;; Configure colors...
      ))
  
  (omegamacs-theme-add-hook #'my-package--configure-colors)
  (my-package--configure-colors omegamacs-theme-current))
```

## Adding New Themes

Adding a new theme is simple - just extend the color palette:

```elisp
(setq omegamacs-theme--color-palettes
  (append omegamacs-theme--color-palettes
    '((my-theme . ((background . "#123456")
                   (foreground . "#abcdef")
                   ;; ... define all required colors
                   )))))
```

The new theme will automatically:
- Appear in theme cycling
- Be available in interactive completion
- Work with all existing integrations

## Dynamic Theme System

The theme system is fully dynamic:

- **Theme cycling** automatically discovers all available themes
- **Interactive completion** lists all defined themes
- **Adding themes** requires no code changes to management functions

## Current Integration Status

The following configuration files have been updated to use the theme system:

### ✅ Integrated Files
- **`theme.el`** - Core theme system with built-in face support
- **`settings.el`** - Basic UI colors (trailing-whitespace, hl-line, etc.)
- **`programming.el`** - which-func and highlight-indent-guides colors
- **`languages/python.el`** - eglot header-line colors

### 🔄 Color Sources
Each integrated file maintains its color settings visibly in the configuration, but references theme colors instead of hard-coding them.

## Implementation Notes

### Redraw Behavior
Theme switching includes automatic display refresh, though occasionally cursor movement may be needed for complete visual updates (documented TODO for future investigation).

### Terminal Compatibility
The `dark-x11` theme uses only X11 named colors, ensuring reliable appearance in terminal mode where hex colors might not be supported.

### Classic Theme
The `classic` theme recreates the original omegamacs color scheme, including:
- Gray20 background with gray80 foreground
- Cyan cursor
- Gray mode line
- Reddish-orange comments and strings
- Traditional Emacs syntax highlighting

## Architecture

The system follows modern Emacs Lisp practices:

- **Lexical binding** enabled throughout
- **Proper namespacing** with `omegamacs-theme-` prefix
- **Hook-based architecture** for extensibility
- **Comprehensive documentation** and error handling
- **Clean separation of concerns** between colors and functionality

## Benefits

### 1. Consistency
- All packages use the same color vocabulary
- Coherent visual experience across all interfaces
- Easy to ensure color accessibility

### 2. Maintainability  
- Color changes in one place affect entire configuration
- Clear dependencies between modules
- Easy to debug color-related issues

### 3. Flexibility
- Add new themes without touching existing code
- Customize colors without modifying package configurations
- Support for both GUI and terminal environments

### 4. Usability
- Instant theme switching with visual feedback
- Discoverable theme management via hydra
- Automatic theme cycling through all available options

The omegamacs theme system provides a robust foundation for visual customization while maintaining clean, maintainable code organization.