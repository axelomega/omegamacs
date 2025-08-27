#!/bin/bash
# Email setup verification script for omegamacs

echo "=== Email Setup Verification ==="
echo

# Check if mu is installed
echo "1. Checking mu installation..."
if command -v mu &> /dev/null; then
    echo "   ✓ mu is installed: $(mu --version | head -1)"
else
    echo "   ✗ mu is not installed"
    echo "     Install with: sudo apt install mu4e maildir-utils"
fi

# Check if mbsync is available
echo "2. Checking mail sync tools..."
if command -v mbsync &> /dev/null; then
    echo "   ✓ mbsync (isync) is installed: $(mbsync --version 2>&1 | head -1)"
elif command -v offlineimap &> /dev/null; then
    echo "   ✓ offlineimap is installed: $(offlineimap --version 2>&1 | head -1)"
else
    echo "   ✗ No mail sync tool found"
    echo "     Install mbsync with: sudo apt install isync"
    echo "     Or install offlineimap with: sudo apt install offlineimap"
fi

# Check OAuth2 support
echo "3. Checking OAuth2 support..."
if command -v oauth2ms &> /dev/null; then
    echo "   ✓ oauth2ms is installed: $(oauth2ms --version 2>&1 | head -1)"
    echo "     OAuth2 authentication available for Gmail"
else
    echo "   ⚠ oauth2ms not found (optional)"
    echo "     Install for OAuth2 support: pip install oauth2ms"
fi

# Check mail directory
echo "4. Checking mail directory..."
if [ -d "$HOME/mail" ]; then
    echo "   ✓ Mail directory exists: $HOME/mail"
    echo "     Folders found: $(ls -1 $HOME/mail 2>/dev/null | wc -l)"
else
    echo "   ✗ Mail directory not found"
    echo "     Create with: mkdir -p ~/mail/{INBOX,Sent,Drafts,Trash,Archive}"
fi

# Check mu database
echo "5. Checking mu database..."
if mu info &> /dev/null; then
    echo "   ✓ mu database initialized"
    echo "     Total messages: $(mu find '*' 2>/dev/null | wc -l)"
else
    echo "   ✗ mu database not initialized"
    echo "     Initialize with: mu init --maildir=~/mail && mu index"
fi

# Check configuration files
echo "6. Checking configuration files..."
if [ -f "$HOME/.mbsyncrc" ]; then
    echo "   ✓ mbsync configuration found"
    # Check if OAuth2 is configured
    if grep -q "AuthMechs XOAUTH2" "$HOME/.mbsyncrc" 2>/dev/null; then
        echo "     ✓ OAuth2 authentication detected"
    elif grep -q "PassCmd.*oauth2ms" "$HOME/.mbsyncrc" 2>/dev/null; then
        echo "     ✓ OAuth2 authentication detected"
    else
        echo "     ⚠ Traditional authentication (consider OAuth2)"
    fi
elif [ -f "$HOME/.offlineimaprc" ]; then
    echo "   ✓ offlineimap configuration found"
else
    echo "   ✗ No mail sync configuration found"
    echo "     See EMAIL_SETUP.md for configuration examples"
fi

if [ -f "$HOME/.authinfo" ]; then
    echo "   ✓ authinfo file found"
    # Check permissions
    PERMS=$(stat -c "%a" "$HOME/.authinfo" 2>/dev/null)
    if [ "$PERMS" = "600" ]; then
        echo "     ✓ Correct permissions (600)"
    else
        echo "     ⚠ Incorrect permissions ($PERMS), should be 600"
        echo "       Fix with: chmod 600 ~/.authinfo"
    fi
else
    echo "   ✗ authinfo file not found"
    echo "     Create ~/.authinfo with SMTP credentials"
fi

# Check Emacs can find mu4e
echo "7. Checking Emacs mu4e availability..."
if emacs --batch --eval "(require 'mu4e)" 2>/dev/null; then
    echo "   ✓ mu4e is available in Emacs"
elif [ -d "/usr/share/emacs/site-lisp/mu4e" ]; then
    echo "   ✓ mu4e found in system location"
else
    echo "   ✗ mu4e not found in Emacs"
    echo "     Ensure mu4e is installed with mu"
fi

echo
echo "=== Setup Summary ==="
if command -v mu &> /dev/null && [ -d "$HOME/mail" ]; then
    echo "✓ Basic requirements met"
    echo "➤ Next steps:"
    echo "  1. Configure your mail sync (see EMAIL_SETUP.md)"
    echo "  2. Run initial sync: mbsync -a (or offlineimap)"
    echo "  3. Initialize mu: mu index"
    echo "  4. Test in Emacs: C-c e"
else
    echo "✗ Setup incomplete"
    echo "➤ Please install missing components and follow EMAIL_SETUP.md"
fi