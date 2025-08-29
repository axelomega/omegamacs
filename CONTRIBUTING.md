# Contributing to Omegamacs

Thank you for your interest in contributing to Omegamacs! 

Omegamacs has been my personal Emacs configuration that I've developed and refined for my own workflow. However, I believe it has grown into something that could benefit the broader Emacs community, so I'm opening it up for contributions from others. This guide will help you understand how to contribute effectively while maintaining the configuration's core principles and quality that I've established.
## Philosophy

Before contributing, please understand the core philosophy I've established for Omegamacs:

- **Code as Config**: I write real Emacs Lisp, not framework-specific abstractions
- **Transparency**: Every behavior should be explicit and traceable
- **Modern Packages**: I prefer current-generation packages over legacy alternatives
- **Clean Separation**: Configuration files stay separate from user data and secrets

## Types of Contributions

I welcome several types of contributions:

### 1. Language Support
**Most needed!** Add support for new programming languages by creating files in the `languages/` directory.

### 2. Bug Fixes
Fix issues in existing configurations or improve error handling. I also appreciate bug reports from users - please file [Issues](https://github.com/axelomega/omegamacs/issues) if you encounter problems, even if you can't fix them yourself.

### 3. Feature Enhancements
Improve existing features while maintaining the core philosophy I've established. Check the [Issues](https://github.com/axelomega/omegamacs/issues) section for features I've requested and enhancement ideas.

### 4. Documentation
Improve the README, create usage examples, or start a wiki page if the README is getting too long.

### 5. Performance Improvements
Optimize startup time or runtime performance.

## Adding Language Support

This is the most common and welcome type of contribution I'm looking for. Here's how:

### Structure
Create a new file: `languages/{language}.el`

### Template
```elisp
;;; -*- lexical-binding: t -*-
;;; {Language} Language Configuration

;; Use use-package for all package configurations
(use-package {major-mode-package}
  :ensure t
  :mode ("\\.{ext}\\'" . {major-mode})
  :config
  ;; Configuration here
  )

;; LSP support (if applicable)
(use-package lsp-mode
  :hook ({major-mode} . lsp-deferred)
  :config
  ;; Language-specific LSP settings
  )

;; Additional packages as needed
```

### Examples
Look at existing files for patterns:
- `languages/cpp.el` - LSP with language server setup
- `languages/python.el` - Multiple package integration
- `languages/elisp.el` - Built-in mode enhancements
- `languages/xml.el` - Performance optimization example

### Language Server Setup
If your language uses an LSP server:
1. Document installation in the README's Language Support section
2. Use `lsp-deferred` for better startup performance
3. Configure language-specific settings in the `:config` section

## Code Style Guidelines

### General Principles
- **Use lexical binding**: Always include `;;; -*- lexical-binding: t -*-` header
- **Use use-package**: All package configurations should use `use-package`
- **Be explicit**: Avoid magic numbers or unclear variable names
- **Comment complex logic**: Explain why, not just what

### File Organization
- One primary concern per file
- Related functionality can be grouped in the same file
- Use clear, descriptive file names
- Place language-specific configs in `languages/` directory

### Package Configuration Pattern
```elisp
(use-package package-name
  :ensure t                    ; Always explicit about installation
  :pin melpa                   ; Pin to specific archive if needed
  :mode ("\\.ext\\'" . mode)   ; File associations
  :hook (mode . function)      ; Mode hooks
  :bind (:map mode-map         ; Key bindings
         ("C-c k" . command))
  :init
  ;; Code to run before package loads

  :config
  ;; Code to run after package loads
  ;; Configuration goes here
  )
```

### Variable Naming
- Use `my-` prefix for custom variables and functions
- Use `my--` prefix for internal/private variables
- Be descriptive: `my-python-test-command` not `my-pytest`

## Testing Your Changes

### Basic Testing
1. **Clean startup test**:
   ```bash
   emacs -Q --load ~/.emacs.d/init.el
   ```

2. **Check startup time**:
   ```bash
   time emacs --eval "(kill-emacs)"
   ```

3. **Test your specific language/feature**:
   - Open relevant file types
   - Test key bindings
   - Verify LSP functionality if applicable

### Performance Testing
- Monitor startup time with `M-x benchmark-init/show-durations-tabulated`
- Ensure your changes don't significantly impact startup performance
- Test with both daemon and regular startup modes

## Submission Guidelines

### Before Submitting
1. **Test thoroughly**: Ensure your changes work in both server and regular modes
2. **Check existing patterns**: Follow conventions used in similar files
3. **Update documentation**: Add language to README's Language Support section if applicable
4. **Minimal scope**: Keep changes focused on a single concern

### Pull Request Process
1. **Fork the repository**
2. **Create a feature branch**: `git checkout -b add-rust-support`
3. **Make your changes** following the guidelines above
4. **Test your changes** with a clean Emacs configuration
5. **Update documentation** as needed
6. **Submit pull request** with clear description

**Note on Commit History:** I use commit squashing to keep the master branch clean with one commit per pull request. Feel free to make multiple commits during development - I'll squash them when merging to maintain a linear, readable history.

### Pull Request Description
Please include:
- **What**: Brief description of the change
- **Why**: Motivation or issue being solved
- **Testing**: How you tested the changes
- **Dependencies**: Any external tools/packages required

Example:
```
Add Rust language support

- Adds rust-mode with LSP support via rust-analyzer
- Includes cargo integration and formatting
- Tested with Rust 1.70+ and rust-analyzer

Requires: cargo install rust-analyzer
```

## Common Pitfalls to Avoid

### Performance Issues
- Don't use `require` at top level - use `use-package` instead
- Avoid blocking operations in `:init` sections
- Use `:defer t` or mode hooks for lazy loading
- Don't add unnecessary dependencies

### Configuration Conflicts
- Check for existing language configurations before adding new ones
- Don't override core Emacs bindings without good reason
- Test with minimal configuration to avoid interactions

### Style Issues
- Don't mix different configuration styles in the same file
- Avoid overly complex configurations - keep it readable
- Don't hardcode paths - use appropriate Emacs functions

## Getting Help

### Questions
- **GitHub Issues**: For bugs or feature discussions
- **Code Review**: Request feedback on your approach before implementing large changes

### Resources
- **Existing code**: Best reference for patterns and style
- **use-package documentation**: https://github.com/jwiegley/use-package
- **Emacs Manual**: https://www.gnu.org/software/emacs/manual/

## Recognition

Contributors will be acknowledged in commit messages and release notes. Significant contributions may be mentioned in the README.

Thank you for helping make Omegamacs better!
