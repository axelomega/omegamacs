# Omegamacs

A modern, modular Emacs configuration with comprehensive support for multiple programming languages, completion frameworks, and development tools.

**Real Emacs for real developers - no training wheels, no abstractions, just powerful, readable code you can own and control.**

## Introduction

This is my personal Emacs setup that I've iteratively refined over time to work well for my development workflow. I'm happy to share it with the wider Emacs community as it has proven to be a robust, capable configuration that strikes a good balance between power and maintainability.

The configuration emphasizes transparency, modern packages, and real Emacs Lisp code over abstractions. A key design principle is **clean separation**: Omegamacs strives to separate the shareable configuration files (which you can check out from this repository) from user-specific config files that contain local settings, secrets, or proprietary information that Omegamacs can't possibly know about.

Whether you use it as-is, adapt parts for your own setup, or simply browse for ideas, I hope you find it useful for your Emacs journey.

## Rationale: Why Choose Omegamacs?

### Philosophy: Code as Config

Omegamacs follows a **"code as config"** philosophy - I'm not afraid of actual Emacs Lisp code. This fundamental approach sets it apart from other configurations:

**🔍 Transparency & Control**
- **No abstractions or DSLs** - you write real Emacs Lisp, not framework-specific syntax
- **No magic** - every behavior is explicit and traceable in readable code
- **Direct access** to Emacs' full capabilities without artificial limitations

**⚡ Modern Without Bloat**
- **Current-generation packages** (Vertico/Consult vs older Helm/Ivy)
- **Performance-optimized** from day one with proper startup tuning
- **Professional development ready** with comprehensive LSP and tool integration

**🎯 The Goldilocks Principle**
- **More capable** than basic configs like Prelude
- **Less overwhelming** than Doom Emacs or Spacemacs
- **Just right** for developers who want power with simplicity

**🚀 Future-Proof Architecture**
- **Pure Emacs Lisp** means compatibility with any Emacs version
- **No framework dependencies** to break or become obsolete
- **Direct upgrade path** as Emacs evolves

**🧠 Learn Real Emacs**
- Users become better at Emacs Lisp and understand their editor deeply
- No need to learn meta-configuration systems or framework-specific abstractions
- Template system teaches good Emacs practices while remaining transparent

**Versus the Competition:**
- **Doom Emacs**: Heavy abstraction layers and custom DSLs hide the actual Emacs underneath
- **Spacemacs**: Layers upon layers of abstraction make basic customization difficult
- **Prelude**: Good but dated package choices and less comprehensive language support

## Quick Start

**Requirements:** Emacs 27.1+ with Git installed

**⚠️ Important:** This configuration is designed for Emacs server mode. The initial startup may be slow due to comprehensive packages and LSP integrations, but subsequent connections are instant. See [Startup Performance](#startup-performance) for timing details and optimization tips.

1. **Clone and setup**:
   ```bash
   git clone https://github.com/axelomega/omegamacs.git ~/omegamacs
   cp ~/omegamacs/templates/init.el ~/.emacs.d/init.el
   cp ~/omegamacs/templates/early-init.el ~/.emacs.d/early-init.el
   ```

2. **Start Emacs** - packages install automatically on first run

3. **Start in server mode** (recommended):
   ```bash
   emacs --daemon (or --fg-daemon)
   emacsclient -c
   ```

4. **Minimal mode** (for using as simple editor):
   ```bash
   export EDITOR="emacs -nw --minimal"
   ```

**See [Installation Options](#installation-options) for detailed setup, [Emacs Server Mode](#emacs-server-mode-detailed) for advanced server configuration, and [Requirements](#requirements) for full build recommendations.**

## Installation Options

### Default Installation

```bash
git clone https://github.com/axelomega/omegamacs.git ~/omegamacs
cp ~/omegamacs/templates/init.el ~/.emacs.d/init.el
cp ~/omegamacs/templates/early-init.el ~/.emacs.d/early-init.el
```

Edit `~/.emacs.d/init.el` to customize settings as needed, then start Emacs.

### Custom Location

```bash
git clone https://github.com/axelomega/omegamacs.git ~/my-custom-emacs
cp ~/my-custom-emacs/templates/init.el ~/.emacs.d/init.el
cp ~/my-custom-emacs/templates/early-init.el ~/.emacs.d/early-init.el
```

Edit `~/.emacs.d/init.el` and set:
```elisp
(setq my-emacs-config-dir "~/my-custom-emacs")
```

### Performance Optimization (Network-Mounted Home)

If your `$HOME` is on NFS or network storage, you may experience slow file operations. Using local storage for frequently-accessed files (backups, undo history, native compilation cache) can dramatically improve performance by avoiding network I/O for these operations.

Edit `~/.emacs.d/early-init.el` and set, for example:
```elisp
(setq my-user-emacs-directory-local "/local/ssd/.emacs.d.local")
```

This redirects backups, undo history, and native compilation cache to local storage.

### Package Updates

By default, packages use cached lists for faster startup. To update:

```bash
EMACS_PACKAGE_UPDATE_ENABLE=1 emacs
```

## Feature Overview

**Core Features:**
- **Modern completion** with Vertico, Marginalia, and Consult
- **LSP support** for C/C++, Python, Verilog, Emacs Lisp, and XML - see [Language Support](#language-support)
- **Git integration** with Magit and Forge
- **Project management** with Projectile
- **Syntax checking** with Flycheck
- **Code completion** with Company
- **AI assistance** with [GitHub Copilot](#github-copilot-integration) (optional)

**Advanced Features:**
- **GTD-based org-mode** with comprehensive [task and project management](#gtd-task-management-system)
- **Hydra menus** for [quick access to common operations](#hydra-menus)
- **Terminal integration** with VTerm
- **LaTeX support** with AUCTeX
- **Undo tree** with persistent history
- **Indentation guides** and syntax highlighting
- **Minimal mode** for fast terminal editing - see [Server Mode](#emacs-server-mode-detailed)

**See individual sections below for detailed descriptions of complex features.**

## Emacs Server Mode (Detailed)

### Why Use Server Mode?

- **Fast client connections**: After initial startup, new frames open instantly
- **Persistent state**: Keep buffers, undo history, and session between frames
- **Better performance**: Language servers and packages stay loaded
- **Seamless workflow**: Close and reopen editor windows without losing context

### Configuration

**This configuration is optimized for server mode**, prioritizing comprehensive functionality over rapid startup times. The initial launch can be lengthy due to extensive packages and language server integrations.

**Quick test:** `emacs --fg-daemon` then `emacsclient -c`

### Advanced Server Setup

**Recommendation:** Test manually first with `emacs --fg-daemon` to ensure your configuration loads properly before setting up automated startup.

**Linux with systemd:**
```bash
# Create ~/.config/systemd/user/emacs.service
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

# Enable and start:
systemctl --user enable emacs.service
systemctl --user start emacs.service
```

**macOS with launchd:**
```xml
<!-- ~/Library/LaunchAgents/gnu.emacs.daemon.plist -->
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

# Load with:
launchctl load ~/Library/LaunchAgents/gnu.emacs.daemon.plist
```

### Alternative: Minimal Mode for Quick Edits

For terminal tasks (git commits, etc.) where you need a fast-starting editor, use minimal mode:

```bash
export EDITOR="emacs -nw --minimal"
```

This loads a lightweight configuration with essential features:
- Basic settings from main config
- Windmove for window navigation
- Spell checking with ispell/aspell
- Essential editing features (electric-pair, show-paren, auto-revert)
- Line numbers in programming modes

**Useful Resources:**
- [Emacs Manual: Using Emacs as a Server](https://www.gnu.org/software/emacs/manual/html_node/emacs/Emacs-Server.html)
- [EmacsWiki: Emacs Client](https://www.emacswiki.org/emacs/EmacsClient)
- [Mastering Emacs: Working with Emacs Server](https://www.masteringemacs.org/article/working-with-emacs-server-and-emacsclient)

## Hydra Menus

Omegamacs includes Hydra menus for quick access to common operations. Hydras provide transient keymaps that stay active until you explicitly exit.

**Available Hydras:**
- **Org-mode** (`C-c o`): Complete GTD workflow commands
- **Additional menus**: Run `M-x my-list-hydras` to see all available hydras

## GitHub Copilot Integration

Optional AI-powered coding assistance using [copilot.el](https://github.com/copilot-emacs/copilot.el).

**Prerequisites:** GitHub Copilot subscription and Node.js 18+

### Configuration Options

Set `my-copilot-config` in `~/.emacs.d/init.el`:

- **`'none`** (default): No Copilot support
- **`'setup`**: Minimal configuration for server installation and authentication
- **`'full`**: Complete configuration with advanced features:
  - Enhanced keybindings (`C-c c` prefix)
  - Smart TAB behavior (completion + indentation)
  - Mode line indicator
  - Language-specific optimizations
  - Better integration with LSP and completion systems

### Setup Process

1. **Enable Copilot**:
   ```elisp
   ;; In ~/.emacs.d/init.el
   (setq my-copilot-config 'setup)
   ```

2. **Install server**: `M-x copilot-install-server`
3. **Authenticate**: `M-x copilot-login` (opens browser)
4. **Upgrade to full config**
   ```elisp
   (setq my-copilot-config 'full)
   ```

**Note:** Requires active GitHub Copilot subscription and Node.js 18+. For Node.js installation/upgrade, see [Node Version Manager (nvm)](https://github.com/nvm-sh/nvm) or [official Node.js downloads](https://nodejs.org/en/download/).

## Language Support

These are the languages I have needed so far in my development work. I would appreciate contributions to extend language support in Omegamacs for additional programming languages.

**Currently Supported Languages:**
- **C/C++**: LSP support with clangd
- **Python**: LSP support with pyright
- **Verilog/SystemVerilog**: Language-specific configuration
- **LaTeX**: AUCTeX integration
- **Emacs Lisp**: Enhanced evaluation and documentation
- **XML**: Performance-optimized nxml-mode

**Language Server Setup:**
Most language servers need separate installation. The configuration will work with standard package managers:

```bash
# C/C++
sudo apt install clangd  # or brew install llvm

# Python
pip install pyright

# Verilog (optional)
# Install verilator or other SystemVerilog tools
```

## Local Customization

Edit `~/.emacs.d/init.el` to customize for your environment:

```elisp
;; Configuration directory (only if not using ~/omegamacs)
;; (setq my-emacs-config-dir "~/my-custom-location")

;; Performance optimization for network home directories
;; (setq my-user-emacs-directory-local "/local/ssd/.emacs.d.local")

;; GitHub Copilot integration
;; (setq my-copilot-config 'setup)    ; 'none, 'setup, or 'full

;; JIRA integration (optional)
;; (setq my-settings-jira-url "https://company.atlassian.net"
;;       my-settings-jira-username "username"
;;       my-settings-jira-project "PROJECT")

;; Custom projectile file filtering
;; (setq my-settings-projectile-generic-command
;;       "find . -type f -not -path '*/node_modules/*' -print0")
```

## GTD Task Management System

### Overview
**Status:** Experimental - feedback and contributions welcome!

Omegamacs includes a comprehensive Getting Things Done (GTD) implementation using org-mode with advanced agenda views powered by org-super-agenda.

**Quick Access:** `C-c o` opens the org-mode Hydra menu with all commands

### Essential Commands

- `C-c o c` - Quick capture (add items to inbox)
- `C-c o a` - Main GTD dashboard agenda view
- `C-c o r` - Refile items from inbox to appropriate locations
- `M-x my-list-hydras` - View all available hydra menus

### GTD Workflow Structure

The system is organized around these core files:
- **inbox.org** - Capture all new tasks and ideas here
- **projects.org** - Multi-step outcomes and project planning
- **next.org** - Single next actions ready to be done
- **someday.org** - Items for future consideration
- **journal.org** - Meeting notes and daily reflections

### Capture Templates

Access via `C-c o` then the appropriate letter:

- **`t`** - **Todo → Inbox**: Quick task capture with creation timestamp
- **`n`** - **Next Action**: Direct entry to next actions with context tags
- **`p`** - **Project**: New project with initial next action
- **`s`** - **Someday**: Future items tagged as someday
- **`j`** - **Journal**: Daily journal entries with timestamps
- **`m`** - **Meeting**: Meeting notes with automatic time tracking

### Agenda Views

The system provides specialized agenda views optimized for GTD workflow:

#### **GTD Dashboard** (`C-c o a` → `g`)
Your main daily view showing:
- 📅 **Today's Schedule**: Calendar items and appointments
- 🔥 **Important**: High-priority tasks (Priority A)
- ⚠️ **Overdue**: Past-due items requiring attention
- 📅 **Due Today**: Items due today
- ⏰ **Due Soon**: Upcoming deadlines (next 7 days)
- ➡️ **Next Actions**: Tasks ready to be done
- 📋 **Projects**: Active projects requiring attention
- ⏳ **Waiting**: Items waiting for others
- 📥 **Inbox**: Uncategorized items needing processing

#### **Context-Based Next Actions** (`C-c o a` → `n`)
Focuses on actionable items grouped by context:
- 🏠 **@home** tasks
- 💼 **@work** tasks
- 💻 **@computer** tasks
- 📞 **@phone** tasks
- 🚗 **@errand** tasks

#### **Project Review** (`C-c o a` → `p`)
Dedicated project management view:
- 🔥 **High Priority Projects**
- 📋 **Active Projects**
- ⚠️ **Stalled Projects** (no recent activity)
- 🎯 **Project Next Actions**

#### **Weekly Review** (`C-c o a` → `w`)
Comprehensive weekly planning view:
- 📅 **This Week's Agenda**
- ✅ **Completed This Week**
- 📋 **Active Projects Status**
- ⏳ **Waiting Items to Follow Up**

### Task States and Workflow

**TODO Keywords:**
- **TODO** - Initial state for all captured items
- **NEXT** - Single actions ready to be done
- **PROJECT** - Multi-step outcomes with sub-tasks
- **WAIT** - Waiting for someone else (requires note)
- **DONE** - Completed tasks (timestamped)
- **CANCELLED** - Abandoned tasks (requires note)

**Priority Levels:**
- **[#A]** - High priority (red in agenda)
- **[#B]** - Normal priority (orange in agenda)
- **[#C]** - Low priority (green in agenda)

### Context Tags

Use `C-c C-q` in any org file to add context tags:
- **@home** - Tasks requiring your home environment
- **@work** - Work-related tasks
- **@computer** - Tasks requiring a computer
- **@phone** - Phone calls to make
- **@errand** - Tasks to do while out
- **someday** - Future consideration items

### GTD Processing Workflow

1. **Capture** (`C-c o c`): Add everything to inbox.org
2. **Process** (`C-c o a` → `g`): Review inbox items in dashboard
3. **Organize** (`C-c o r`): Refile items to appropriate locations
4. **Review** (`C-c o a` → `w`): Weekly review of all commitments
5. **Do** (`C-c o a` → `n`): Work from context-based next actions

### Advanced Features

**Smart Scheduling:**
- `C-c o d` - Add deadline to current task
- Agenda views automatically group by urgency

**Project Management:**
- Automatic detection of stalled projects
- Project templates with built-in next actions
- Hierarchical project organization

**Time Tracking:**
- Meeting capture template includes automatic clocking
- Time tracking for any task with `C-c C-x C-i/C-o`

**File Organization:**
All org files are automatically created in `~/.emacs.d/org/` directory and are immediately available in agenda views. Note that even when using local storage optimization for NFS-mounted home directories, org files intentionally remain in your `~/.emacs.d/` directory since you likely want access to your tasks and notes across different hosts.

### GTD Workflow Tips

1. **Daily**: Use GTD Dashboard (`C-c o a g`) to plan your day
2. **Weekly**: Use Weekly Review (`C-c o a w`) for broader planning
3. **Context Switching**: Use Next Actions by Context (`C-c o a n`) when changing environments
4. **Projects**: Use Project Review (`C-c o a p`) for periodic project health checks
5. **Inbox Zero**: Regularly process and refile inbox items to keep system current

**All org files are automatically created in `~/.emacs.d/org/` and immediately available in agenda views.**

## Requirements

### Minimum Requirements

- **Emacs 27.1 or later**
- **Git** (for package management and Magit)
- **Basic system packages**: Most distributions include required libraries

### Optional Language Servers

For full LSP functionality, install language servers separately:

```bash
# C/C++
sudo apt install clangd    # Ubuntu/Debian
brew install llvm          # macOS

# Python
pip install pyright

# Other languages work with built-in modes
```

### Recommended Emacs Build Features

**Developed and tested on:** GNU Emacs 31.0.50 (development build) with feature-rich configuration.

**Key Features for Best Experience:**
- `NATIVE_COMP` - Native compilation for better performance
- `TREE_SITTER` - Modern syntax highlighting and parsing
- `IMAGEMAGICK` - Image display and manipulation
- `GNUTLS` - Secure connections for package downloads
- `JSON` - Fast JSON parsing
- `LIBXML2` - XML/HTML parsing

**Check your build features:**
```bash
emacs -Q --batch --eval "(print system-configuration-features)"
```

**Standard Emacs builds work fine** - advanced features gracefully degrade if not available.

### Full Feature List (Reference)

```
CAIRO DBUS FREETYPE GIF GLIB GMP GNUTLS GPM GSETTINGS HARFBUZZ
IMAGEMAGICK JPEG LCMS2 LIBOTF LIBSELINUX LIBSYSTEMD LIBXML2 M17N_FLT
MODULES NATIVE_COMP NOTIFY INOTIFY PDUMPER PNG RSVG SECCOMP SOUND
THREADS TIFF TOOLKIT_SCROLL_BARS TREE_SITTER X11 XDBE XIM XINPUT2
XPM LUCID ZLIB
```

**Custom build configuration:**
```bash
--with-x-toolkit=lucid --with-imagemagick --with-xft --with-tree-sitter
```

For building instructions, see [official Emacs build documentation](https://www.gnu.org/software/emacs/manual/html_node/efaq/Installing-Emacs.html).

## Startup Performance

Omegamacs tracks startup times to help monitor configuration performance and identify potential issues.

### Checking Startup Time

**Built-in timing display:**
Startup times are tracked but not automatically displayed. You can view startup profiling with:
- `M-x benchmark-init/show-durations-tree` - Tree view of package load times
- `M-x benchmark-init/show-durations-tabulated` - Tabulated view sorted by duration
- `M-x emacs-init-time` - Show total startup time

### Typical Performance

**Expected startup times (cold start):**
- **Modern SSD with native compilation**: 3-8 seconds
- **Network-mounted home directory**: 8-15 seconds
- **Older hardware or HDDs**: 10-20+ seconds

**Factors affecting startup time:**
- **Native compilation**: Significantly improves performance after initial compile
- **Storage type**: SSDs much faster than HDDs, local faster than network
- **Package count**: Full configuration loads 50+ packages
- **LSP servers**: Language servers add initialization overhead

### Optimization Tips

**For best performance:**
1. **Use server mode**: Start once, connect many times
2. **Enable local data directory**: Use `early-init.el` template for network homes
3. **Native compilation**: Use Emacs build with `NATIVE_COMP` support
4. **SSD storage**: Local SSD dramatically improves load times
5. **Minimal mode**: Use `--minimal` flag for quick terminal edits

**Server mode eliminates startup concerns** - after the initial daemon start, new frames open in ~0.1 seconds.

---

*This is my personal Emacs setup that I've iteratively refined over time. I'm sharing it to provide a useful starting point for others' configurations or to offer specific feature insights. Feel free to use, modify, or adapt any components that align with your workflow!*
