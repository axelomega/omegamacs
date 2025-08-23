# Omegamacs

A modern, modular Emacs configuration with comprehensive support for multiple programming languages, completion frameworks, and development tools.

This is my personal Emacs setup that I've iteratively refined over time. I'm sharing it to provide a useful starting point for others' configurations or to offer specific feature insights. Feel free to use, modify, or adapt any components that align with your workflow!

## ⚠️ Important: Designed for Emacs Server Mode

This configuration is optimized for running as an Emacs server, prioritizing comprehensive functionality over rapid startup times. The initial launch can be lengthy due to the extensive package set and language server integrations. For the best experience, run as a daemon and connect with `emacsclient`. See the [Emacs Server Mode](#emacs-server-mode-recommended) section for detailed setup instructions.

**Quick test:** `emacs --fg-daemon` then `emacsclient -c`

### For Simple Text Editing (EDITOR variable)

If you need Emacs for quick terminal tasks (git commits, etc.), this configuration provides a minimal mode that loads only essential features for fast startup:

```bash
export EDITOR="emacs -nw --minimal"
```

This loads a lightweight configuration with:
- **Basic settings** from your main config
- **Windmove** for easy window navigation
- **Spell checking** with ispell/aspell
- **Essential editing features** (electric-pair-mode, show-paren-mode, auto-revert)
- **Line numbers** in programming modes

The `--minimal` flag loads only the `minimal.el` configuration file, giving you a fast-starting Emacs with useful features instead of the bare-bones `-q` option.

### Modern Emacs Build Required

This configuration is designed for modern Emacs builds with advanced features like native compilation and tree-sitter support. See [Recommended Emacs Build Features](#recommended-emacs-build-features) for detailed build requirements and feature list.

## Features

- **Modern completion** with Vertico, Marginalia, and Consult
- **LSP support** for C/C++, Python, and Verilog
- **AI-powered coding assistance** with GitHub Copilot (optional)
- **Git integration** with Magit
- **Project management** with Projectile
- **Syntax checking** with Flycheck
- **Code completion** with Company
- **Snippet support** with Yasnippet
- **LaTeX support** with AUCTeX
- **Terminal integration** with VTerm
- **Undo tree** with persistent history
- **Indentation guides** for better code visualization

## Quick Start

1. **Clone this repository** to your preferred location:
   ```bash
   git clone https://github.com/axelomega/omegamacs.git ~/omegamacs
   cd ~/omegamacs
   ```

2. **Copy and customize the init file**:
   ```bash
   cp templates/init.el ~/.emacs.d/init.el
   ```
   Then edit `~/.emacs.d/init.el` to uncomment and customize settings as needed.

3. **Start Emacs** - the configuration will automatically install required packages on first run.

**That's it!** The configuration works automatically when placed in `~/omegamacs` (the default location).

### Package Updates

By default, packages use cached lists for faster startup. To check for and install package updates:

```bash
EMACS_PACKAGE_UPDATE_ENABLE=1 emacs
```

This enables automatic package refreshing and weekly update checks. For normal usage, omit the environment variable to prevent unexpected updates.

### Alternative: Custom Location

If you prefer a different location, edit `~/.emacs.d/init.el`:
```elisp
;; Only needed if you didn't clone to ~/omegamacs
(setq my-emacs-config-dir "~/my-custom-emacs-config")
```

## Local Customization

Edit `~/.emacs.d/init.el` to customize settings for your environment:

- **Configuration directory**: Set `my-emacs-config-dir` only if you didn't use `~/omegamacs`
- **Local data directory**: Configure `my-user-emacs-directory-local` for better performance (see below)
- **JIRA integration**: Configure `my-settings-jira-*` variables if using JIRA
- **Projectile**: Set `my-settings-projectile-generic-command` for custom file filtering
- **Development tools**: Configure paths to language servers and other tools if necessary

### Performance Optimization for Network-Mounted Home Directories

If your `${HOME}` directory is mounted via NFS or another network filesystem, you may experience slow file operations. Omegamacs supports redirecting data files (backups, undo history, auto-saves, and native compilation cache) to a local disk location for better performance.

**Setup:**
1. **Copy the early-init template**:
   ```bash
   cp templates/early-init.el ~/.emacs.d/early-init.el
   ```

2. **Edit `~/.emacs.d/early-init.el`** to set your local directory:
   ```elisp
   ;; Set to a local disk location for better performance
   (setq my-user-emacs-directory-local "/path/to/local/disk/.emacs.d.local")
   ```

**What gets redirected to the local directory:**
- **Backups and auto-saves**: File backups and automatic saves
- **Undo-tree history**: Persistent undo history files
- **Native compilation cache**: ELN cache for compiled Emacs Lisp files

**Example configurations:**
```elisp
;; For a dedicated local SSD mount
(setq my-user-emacs-directory-local "/local/ssd/.emacs.d.local")

;; For a local tmp directory (loses data on reboot)
(setq my-user-emacs-directory-local "/tmp/.emacs.d.local")

;; Default: use standard ~/.emacs.d (no performance benefit)
(setq my-user-emacs-directory-local user-emacs-directory)
```

Example `~/.emacs.d/init.el` customizations:
```elisp
;; Configuration directory (only needed if not using ~/omegamacs)
;; (setq my-emacs-config-dir "~/my-custom-location")

;; Optional: GitHub Copilot configuration
;; (setq my-copilot-config 'setup)    ; or 'full, or 'none

;; Optional: JIRA integration
;; (setq my-settings-jira-url "https://your-company.atlassian.net"
;;       my-settings-jira-username "your-username"
;;       my-settings-jira-project "PROJECT")

;; Optional: Custom projectile command
;; (setq my-settings-projectile-generic-command "find . -type f -not -path '*/node_modules/*' -print0")
```

## File Structure

- `emacs_init.el` - Main initialization file
- `packages.el` - Package management and archives
- `settings.el` - General Emacs settings and key bindings
- `programming.el` - Programming language configurations
- `completion.el` - Completion framework setup (Vertico/Consult)
- `development.el` - Development tools (undo-tree, helpful, etc.)
- `compilation.el` - Build and error navigation
- `magit.el` - Git integration
- `projectile.el` - Project management
- `company.el` - Code completion
- `flycheck.el` - Syntax checking
- `frame_buffer_handling.el` - Window and buffer management
- `ido.el` - IDO configuration (legacy)
- `tramp.el` - Remote file access configuration
- `minimal.el` - Lightweight configuration for `--minimal` mode
- `version-check.el` - Package version checking utilities
- `copilot/` - GitHub Copilot integration
  - `copilot-setup.el` - Minimal Copilot setup
  - `copilot.el` - Complete Copilot configuration
- `languages/` - Language-specific configurations
  - `cpp.el` - C/C++ settings
  - `python.el` - Python development setup
  - `verilog.el` - Verilog/SystemVerilog configuration
  - `latex.el` - LaTeX support

## Template Files

- `templates/init.el` - Copy to `~/.emacs.d/init.el` and customize as needed
- `templates/early-init.el` - Optional: Copy to `~/.emacs.d/early-init.el` to configure local data directory

## Directory Organization

Omegamacs uses a clean separation between configuration files (the git repository) and user data (in `~/.emacs.d/`). This allows you to:

- **Keep the git repo anywhere** (e.g., `~/omegamacs`, `~/projects/my-emacs`, etc.)
- **Version control your config** without mixing in user data
- **Easily update** by pulling from git
- **Backup user data separately** from configuration

```
~/.emacs.d/
├── init.el                    # Main entry point with your local settings (copied from init-template.el)
├── early-init.el              # Optional: Configure local data directory (copied from early-init-template.el)
├── backups/                   # File backups and auto-saves (or redirected to local directory)
├── undo-tree/                 # Persistent undo history files (or redirected to local directory)
├── snippets/                  # YASnippet templates
├── cache/                     # IDO and other cache files
│   └── ido.last              # IDO file history
├── elpa/                     # Installed packages (managed automatically)
└── eln-cache/                # Native compilation cache (or redirected to local directory)

~/omegamacs/                   # Configuration files (default location)
├── emacs_init.el             # Main configuration loader
├── packages.el               # Package management
├── settings.el               # General Emacs settings
├── programming.el            # Language-specific configurations
├── completion.el             # Completion framework setup
├── development.el            # Development tools
├── compilation.el            # Build and error navigation
├── magit.el                  # Git integration
├── projectile.el             # Project management
├── company.el                # Code completion
├── flycheck.el               # Syntax checking
├── frame_buffer_handling.el  # Window and buffer management
├── ido.el                    # IDO configuration (legacy)
├── tramp.el                  # Remote file access configuration
├── minimal.el                # Lightweight configuration for --minimal mode
├── version-check.el          # Package version checking utilities
├── templates/                # Template files
│   ├── init.el               # Template for ~/.emacs.d/init.el
│   └── early-init.el         # Template for ~/.emacs.d/early-init.el
├── copilot/                  # GitHub Copilot integration
│   ├── copilot-setup.el      # Minimal Copilot setup
│   └── copilot.el            # Complete Copilot configuration
└── languages/                # Language-specific configurations
    ├── cpp.el                # C/C++ settings
    ├── python.el             # Python development setup
    ├── verilog.el            # Verilog/SystemVerilog configuration
    └── latex.el              # LaTeX support
```

**Key Benefits:**
- **Git-friendly**: Repository contains only configuration files, no user data
- **Flexible location**: Clone the repo anywhere you want
- **Easy updates**: `git pull` to get latest config improvements
- **Clean backups**: User data (`~/.emacs.d/`) separate from config repo
- **No conflicts**: Generated files, caches, and personal data stay out of version control

## Emacs Server Mode (Recommended)

### Why Use Server Mode?

- **Fast client connections**: After initial startup, new frames open instantly
- **Persistent state**: Keep your buffers, undo history, and session between frames
- **Better performance**: Language servers and packages stay loaded
- **Seamless workflow**: Close and reopen editor windows without losing context

### Setting Up Emacs Server

**Option 1: Manual start**
```bash
# Start daemon
emacs --daemon

# Connect with new frame
emacsclient -c

# Connect in terminal
emacsclient -t
```

**Option 2: Auto-start with systemd (Linux)**
Create `~/.config/systemd/user/emacs.service`:
```ini
[Unit]
Description=Emacs text editor
Documentation=info:emacs man:emacs(1) https://gnu.org/software/emacs/

[Service]
Type=notify
ExecStart=/usr/bin/emacs --fg-daemon
ExecStop=/usr/bin/emacsclient --eval "(kill-emacs)"
Environment=SSH_AUTH_SOCK=%t/keyring/ssh
Restart=on-failure

[Install]
WantedBy=default.target
```

Then enable:
```bash
systemctl --user enable emacs.service
systemctl --user start emacs.service
```

**Option 3: macOS with launchd**
Create `~/Library/LaunchAgents/gnu.emacs.daemon.plist`:
```xml
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>Label</key>
  <string>gnu.emacs.daemon</string>
  <key>ProgramArguments</key>
  <array>
    <string>/usr/local/bin/emacs</string>
    <string>--fg-daemon</string>
  </array>
  <key>RunAtLoad</key>
  <true/>
  <key>ServiceDescription</key>
  <string>Emacs Daemon</string>
</dict>
</plist>
```

Load with:
```bash
launchctl load ~/Library/LaunchAgents/gnu.emacs.daemon.plist
```

### Useful Resources

- [Emacs Manual: Using Emacs as a Server](https://www.gnu.org/software/emacs/manual/html_node/emacs/Emacs-Server.html)
- [EmacsWiki: Emacs Client](https://www.emacswiki.org/emacs/EmacsClient)
- [Mastering Emacs: Working with Emacs Server](https://www.masteringemacs.org/article/working-with-emacs-server-and-emacsclient)

## Requirements

- **Emacs 27.1 or later** with recommended features (see below)
- **Git** (for package management and Magit)
- **Optional**: Language servers (clangd, pyright, etc.) for LSP features

### Recommended Emacs Build Features

This configuration is developed and tested on **GNU Emacs 31.0.50** (development version 097b685aa1c7, built 2024-11-22) with a feature-rich custom build. It works best with similar capabilities:

**Key Features:**
- `NATIVE_COMP` - Native compilation for better performance
- `TREE_SITTER` - Modern syntax highlighting and parsing
- `IMAGEMAGICK` - Image display and manipulation
- `LIBXML2` - XML/HTML parsing for web browsing
- `GNUTLS` - Secure connections for package downloads
- `MODULES` - Dynamic module loading
- `JSON` - Fast JSON parsing

**Full feature list:**
```
CAIRO DBUS FREETYPE GIF GLIB GMP GNUTLS GPM GSETTINGS HARFBUZZ 
IMAGEMAGICK JPEG LCMS2 LIBOTF LIBSELINUX LIBSYSTEMD LIBXML2 M17N_FLT 
MODULES NATIVE_COMP NOTIFY INOTIFY PDUMPER PNG RSVG SECCOMP SOUND 
THREADS TIFF TOOLKIT_SCROLL_BARS TREE_SITTER X11 XDBE XIM XINPUT2 
XPM LUCID ZLIB
```

**Build configuration:**
```bash
--with-x-toolkit=lucid --with-imagemagick --with-xft --with-tree-sitter
```

### Checking Your Emacs Build

To see what features your Emacs has:
```bash
emacs -Q --batch --eval "(print system-configuration-features)"
```

**Note:** The configuration will work with standard Emacs builds, but some features (like tree-sitter modes, native compilation performance, and image display) may not be available without these build options.

**Building from source:** For instructions on building Emacs with custom features, see the [official build documentation](https://www.gnu.org/software/emacs/manual/html_node/efaq/Installing-Emacs.html).

## GitHub Copilot Setup (Optional)

Omegamacs includes optional GitHub Copilot integration for AI-powered coding assistance using [copilot.el](https://github.com/copilot-emacs/copilot.el).

### Prerequisites

1. **GitHub Copilot subscription**: You need an active GitHub Copilot subscription
2. **Node.js**: Required by the Copilot Emacs package (version 18+ recommended)

### Quick Setup

1. **Enable Copilot** in your `~/.emacs.d/init.el`:
   ```elisp
   ;; Start with minimal setup to install the server and to authenticate
   (setq my-copilot-config 'setup)
   ```

2. **Start Emacs** and install the Copilot server:
   ```
   M-x copilot-install-server
   ```

3. **Authenticate with GitHub**:
   ```
   M-x copilot-login
   ```
   This opens a browser for GitHub authentication and provides a device code.

### Configuration Options

Set `my-copilot-config` in `~/.emacs.d/init.el`:

- **`'none`** (default): No Copilot support
- **`'setup`**: Minimal configuration for initial server installation and authentication
- **`'full`**: Complete configuration with advanced features
  - Enhanced keybindings (`C-c c` prefix for Copilot commands)
  - Smart TAB behavior (completion + indentation)
  - Mode line indicator
  - Language-specific optimizations
  - Better integration with LSP and completion systems
