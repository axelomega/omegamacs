#!/bin/bash
# Omegamacs Installation Script
# Simple setup script that copies templates and sets basic paths
# Following the "code as config" philosophy - you'll edit real Emacs Lisp, not answer prompts

set -e  # Exit on error

# Check for Linux
OS_TYPE="$(uname -s)"
if [[ "$OS_TYPE" != "Linux" ]]; then
    echo "╔═══════════════════════════════════════════════════════════════╗"
    echo "║                      PLATFORM NOTICE                          ║"
    echo "╚═══════════════════════════════════════════════════════════════╝"
    echo ""
    echo "This installation script is currently tested on Debian Linux only."
    echo "Detected platform: $OS_TYPE"
    echo ""
    echo "The script uses 'sed -i' which has different syntax on macOS,"
    echo "Windows (Git Bash/MSYS2/Cygwin), and other Unix systems."
    echo "Other Linux distributions and WSL should work but are untested."
    echo "We welcome contributions to test and improve portability!"
    echo ""
    echo "To contribute:"
    echo "  1. Test and modify the script for your platform"
    echo "  2. Submit a pull request at:"
    echo "     https://github.com/axelomega/omegamacs"
    echo ""
    echo "For now, please use manual installation:"
    echo "  See INSTALL file or README.md for instructions"
    echo ""
    exit 1
fi

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

info() {
    echo -e "${BLUE}==>${NC} $1"
}

success() {
    echo -e "${GREEN}✓${NC} $1"
}

warning() {
    echo -e "${YELLOW}!${NC} $1"
}

error() {
    echo -e "${RED}✗${NC} $1"
}

ask_yes_no() {
    local prompt=$1
    local default=${2:-n}
    local answer

    if [ "$default" = "y" ]; then
        prompt="$prompt [Y/n]: "
    else
        prompt="$prompt [y/N]: "
    fi

    while true; do
        read -p "$prompt" answer
        answer=${answer:-$default}
        case $answer in
            [Yy]* ) return 0;;
            [Nn]* ) return 1;;
            * ) echo "Please answer yes or no.";;
        esac
    done
}

ask_input() {
    local prompt=$1
    local default=$2
    local answer

    if [ -n "$default" ]; then
        read -p "$prompt [$default]: " answer
        echo "${answer:-$default}"
    else
        read -p "$prompt: " answer
        echo "$answer"
    fi
}

main() {
    echo ""
    echo "╔═════════════════════════════════════════════════════╗"
    echo "║           Omegamacs Installation Script             ║"
    echo "║   Real Emacs for real developers - code as config   ║"
    echo "╚═════════════════════════════════════════════════════╝"
    echo ""

    # Check if Emacs is installed
    if ! command -v emacs &> /dev/null; then
        error "Emacs is not installed. Please install Emacs 27.1 or later first."
        exit 1
    fi

    local emacs_version=$(emacs --version | head -n1)
    success "Found: $emacs_version"
    echo ""

    # Determine installation directory
    local script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
    local install_dir="$script_dir"

    info "Omegamacs directory: $install_dir"
    echo ""

    # Check templates exist
    if [ ! -f "$install_dir/templates/init.el" ] || [ ! -f "$install_dir/templates/early-init.el" ]; then
        error "Templates not found. Are you running this from the omegamacs directory?"
        exit 1
    fi

    local emacs_d="${HOME}/.emacs.d"
    local emacs_file="${HOME}/.emacs"

    # Check for existing .emacs file and offer backup
    if [ -f "$emacs_file" ]; then
        warning "Existing .emacs file detected: $emacs_file"
        echo "Omegamacs uses .emacs.d/init.el instead of .emacs"
        echo ""
        if ask_yes_no "Backup existing .emacs file before continuing?" "y"; then
            local default_backup="${emacs_file}.backup.$(date +%Y%m%d-%H%M%S)"
            local backup_file=$(ask_input "Backup location" "$default_backup")
            backup_file="${backup_file/#\~/$HOME}"
            info "Moving to $backup_file..."
            mv "$emacs_file" "$backup_file"
            success "Backup created: $backup_file"
            echo ""
        else
            warning "No backup will be created."
            if ask_yes_no "Delete existing .emacs file?" "n"; then
                info "Deleting $emacs_file..."
                rm -f "$emacs_file"
                success "Deleted existing .emacs"
                echo ""
            else
                error "Installation cancelled. Cannot proceed with existing .emacs file"
                exit 1
            fi
        fi
    fi

    # Check for existing .emacs.d and offer backup
    if [ -d "$emacs_d" ]; then
        warning "Existing .emacs.d directory detected: $emacs_d"
        echo ""
        if ask_yes_no "Backup existing .emacs.d before continuing?" "y"; then
            local default_backup="${emacs_d}.backup.$(date +%Y%m%d-%H%M%S)"
            local backup_dir=$(ask_input "Backup location" "$default_backup")
            backup_dir="${backup_dir/#\~/$HOME}"
            info "Moving to $backup_dir..."
            mv "$emacs_d" "$backup_dir"
            success "Backup created: $backup_dir"
            echo ""
        else
            warning "No backup will be created."
            if ask_yes_no "Delete existing .emacs.d directory?" "n"; then
                info "Deleting $emacs_d..."
                rm -rf "$emacs_d"
                success "Deleted existing .emacs.d"
                echo ""
            else
                error "Installation cancelled. Cannot proceed without removing existing .emacs.d"
                exit 1
            fi
        fi
    fi

    # Create fresh .emacs.d directory
    mkdir -p "$emacs_d"

    # Copy templates
    info "Copying configuration templates..."
    cp "$install_dir/templates/init.el" "$emacs_d/init.el"
    cp "$install_dir/templates/early-init.el" "$emacs_d/early-init.el"
    success "Templates copied"
    echo ""

    # Set the installation directory in init.el
    if [ "$install_dir" != "$HOME/omegamacs" ]; then
        info "Setting custom installation directory in init.el..."
        # Uncomment and set the directory
        sed -i "s|^;; (setq omegamacs-emacs-config-dir \"~/omegamacs\")|(setq omegamacs-emacs-config-dir \"$install_dir\")|" "$emacs_d/init.el"
        success "Set omegamacs-emacs-config-dir to $install_dir"
        echo ""
    fi

    # Ask about NFS/network home directory for performance optimization
    local use_local_storage=false
    local local_storage_path=""

    echo ""
    info "Performance Optimization"
    echo "If your home directory is on a network filesystem (NFS, etc.),"
    echo "using local storage for Emacs data can significantly improve performance."
    echo ""
    if ask_yes_no "Is your \$HOME on a network filesystem?" "n"; then
        local_storage_path=$(ask_input "Enter local storage path for Emacs data" "/tmp/.emacs.d.local")
        local_storage_path="${local_storage_path/#\~/$HOME}"

        info "Setting local storage path in early-init.el..."
        sed -i "s|^(setq omegamacs-user-emacs-directory-local user-emacs-directory)|(setq omegamacs-user-emacs-directory-local \"$local_storage_path\")|" "$emacs_d/early-init.el"
        success "Set omegamacs-user-emacs-directory-local to $local_storage_path"
        use_local_storage=true
    fi

    # Installation complete
    echo ""
    success "Installation complete!"
    echo ""
    echo "╔════════════════════════════════════════════════════════════╗"
    echo "║                    Next Steps                              ║"
    echo "╚════════════════════════════════════════════════════════════╝"
    echo ""
    info "1. Review and customize your configuration:"
    echo "     \$EDITOR $emacs_d/init.el"
    echo ""
    echo "   The init.el file contains all configuration options with"
    echo "   comments explaining each setting. Uncomment and modify"
    echo "   what you need - it's real Emacs Lisp, no abstractions!"
    echo ""
    info "2. Start Emacs - packages will install automatically:"
    echo "     emacs"
    echo ""
    info "3. For best performance, use server mode:"
    echo "     emacs --daemon      # Start daemon"
    echo "     emacsclient -c      # Connect to daemon"
    echo ""
    info "4. For quick terminal edits, use minimal mode:"
    echo "     export EDITOR=\"emacs -nw --minimal\""
    echo ""
    info "Configuration files:"
    echo "     Init file: $emacs_d/init.el"
    echo "     Early init: $emacs_d/early-init.el"
    echo "     Omegamacs: $install_dir"
    echo ""
    info "Documentation:"
    echo "     README: $install_dir/README.md"
    echo "     Online: https://github.com/axelomega/omegamacs"
    echo ""
}

main "$@"
