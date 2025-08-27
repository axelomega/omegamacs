# Email Setup Guide for Omegamacs

This guide will help you set up comprehensive email support in your Emacs configuration using mu4e.

## Prerequisites Installation

### 1. Install mu and mu4e

#### On Ubuntu/Debian:
```bash
sudo apt update
sudo apt install mu4e maildir-utils
```

#### On Arch Linux:
```bash
sudo pacman -S mu
```

#### On macOS (with Homebrew):
```bash
brew install mu
```

### 2. Install Mail Synchronization Tool

Choose one of these tools to sync your email:

#### Option A: mbsync (recommended)
```bash
# Ubuntu/Debian
sudo apt install isync

# Arch Linux
sudo pacman -S isync

# macOS
brew install isync
```

#### Option B: OfflineIMAP
```bash
# Ubuntu/Debian
sudo apt install offlineimap

# Arch Linux
sudo pacman -S offlineimap

# macOS
brew install offlineimap
```

## Email Account Configuration

### 1. Gmail Setup Example

#### Create ~/.mbsyncrc:
```
IMAPAccount gmail
Host imap.gmail.com
User your.email@gmail.com
PassCmd "gpg --quiet --for-your-eyes-only --no-tty --decrypt ~/.password-store/gmail.gpg"
SSLType IMAPS
CertificateFile /etc/ssl/certs/ca-certificates.crt

IMAPStore gmail-remote
Account gmail

MaildirStore gmail-local
Subfolders Verbatim
Path ~/mail/
Inbox ~/mail/INBOX

Channel gmail
Far :gmail-remote:
Near :gmail-local:
Patterns * !"[Google Mail]/All Mail"
Create Both
SyncState *
```

#### Create ~/.authinfo (for SMTP):
```
machine smtp.gmail.com login your.email@gmail.com password your-app-password port 587
```

Make sure to set proper permissions:
```bash
chmod 600 ~/.authinfo ~/.mbsyncrc
```

### 2. Other Email Providers

#### Microsoft/Outlook:
```
IMAPAccount outlook
Host outlook.office365.com
User your.email@outlook.com
PassCmd "gpg --quiet --for-your-eyes-only --no-tty --decrypt ~/.password-store/outlook.gpg"
SSLType IMAPS
```

#### Custom IMAP Server:
```
IMAPAccount custom
Host mail.yourdomain.com
User your.email@yourdomain.com
Pass your-password
SSLType IMAPS
Port 993
```

## Initial Setup Steps

### 1. Create Mail Directory Structure
```bash
mkdir -p ~/mail/{INBOX,Sent,Drafts,Trash,Archive}
```

### 2. Initial Mail Sync
```bash
mbsync -a
```

### 3. Initialize mu Database
```bash
mu init --maildir=~/mail
mu index
```

### 4. Test mu4e in Emacs
1. Start Emacs
2. Press `C-c e` to open the email hydra
3. Press `i` for inbox or `m` for main view

## Configuration Customization

### Multiple Email Accounts

If you have multiple email accounts, uncomment and modify the `mu4e-contexts` section in `email.el`:

```elisp
(setq mu4e-contexts
      `(,(make-mu4e-context
          :name "work"
          :match-func (lambda (msg) (when msg (string-prefix-p "/work" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "you@work.com")
                  (user-full-name . "Your Name")
                  (mu4e-drafts-folder . "/work/Drafts")
                  (mu4e-sent-folder . "/work/Sent")
                  (mu4e-trash-folder . "/work/Trash")
                  (mu4e-refile-folder . "/work/Archive")
                  (smtpmail-smtp-server . "smtp.work.com")
                  (smtpmail-smtp-service . 587)))
        ,(make-mu4e-context
          :name "personal"
          :match-func (lambda (msg) (when msg (string-prefix-p "/personal" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "you@gmail.com")
                  (user-full-name . "Your Name")
                  (mu4e-drafts-folder . "/personal/Drafts")
                  (mu4e-sent-folder . "/personal/Sent")
                  (mu4e-trash-folder . "/personal/Trash")
                  (mu4e-refile-folder . "/personal/Archive")
                  (smtpmail-smtp-server . "smtp.gmail.com")
                  (smtpmail-smtp-service . 587)))))
```

### Customize Folders

Adjust the folder paths in `email.el` to match your mail setup:

```elisp
(setq my--email-sent-folder "/Sent Items"    ; or "/Sent"
      my--email-drafts-folder "/Draft"       ; or "/Drafts"
      my--email-trash-folder "/Deleted Items" ; or "/Trash"
      my--email-refile-folder "/Archive")    ; or "/All Mail"
```

## Key Bindings and Usage

### Main Hydra: `C-c e`
- `i`: Open inbox
- `c`: Compose new email
- `t`: Today's emails
- `w`: This week's emails
- `s`: Search emails
- `u`: Update mail and index

### In Headers View:
- `c`: Capture email to org-mode
- `r`: Reply
- `f`: Forward
- `d`: Delete
- `m`: Move to folder
- `+`: Mark as flagged
- `RET`: View message

### In Message View:
- `c`: Capture to org-mode
- `r`: Reply
- `f`: Forward
- `a`: View attachments
- `o`: Open attachment
- `q`: Quit

### Org-mode Integration:
- Email capture templates are automatically added
- `Et`: Email → Todo
- `En`: Email → Next Action
- `Ef`: Email → Follow Up
- `Er`: Email → Reference

## Troubleshooting

### Common Issues:

1. **mu4e not found**: Ensure mu is installed and mu4e is in your load-path
2. **No mail synced**: Check your mbsync configuration and run `mbsync -a` manually
3. **Authentication issues**: Verify your authinfo file and use app passwords for Gmail
4. **SMTP errors**: Check your SMTP server settings and authentication

### Debug Commands:
```bash
# Test mbsync
mbsync -V -a

# Check mu index
mu find from:yourself

# Test SMTP (in Emacs)
M-x smtpmail-send-queued-mail
```

## Security Considerations

1. **Use GPG for passwords**:
   ```bash
   echo "your-password" | gpg --encrypt --armor -r your@email.com > ~/.password-store/gmail.gpg
   ```

2. **Set proper file permissions**:
   ```bash
   chmod 600 ~/.authinfo ~/.mbsyncrc
   ```

3. **Use OAuth2 or app passwords** for Gmail/Google accounts instead of your main password

4. **Consider using pass** (the standard Unix password manager) for better password management

## OAuth2 Authentication for Gmail

For enhanced security, you can use OAuth2 instead of app passwords for Gmail access. This method is more secure and doesn't require generating app-specific passwords.

### 1. Install oauth2ms

First, install the oauth2ms tool for handling OAuth2 authentication:

#### On Ubuntu/Debian:
Install Prerequisites.
```bash
sudo apt update
sudo apt install python3-pyxdg python3-msal python3-gnupg


Obtain the oauth2ms script by cloning its Git repository.
```bash
git clone https://github.com/harishkrupo/oauth2ms.git
```

Install any remaining Python dependencies using pip.
```bash
cd oauth2ms
pip install -r requirements.txt
```

Make oauth2ms Executable and Accessible.
Copy the oauth2ms script to a directory included in your system's $PATH so you can execute it from any location. A common choice is /usr/local/bin/.
```
sudo cp oauth2ms /usr/local/bin/
sudo chmod +x /usr/local/bin/oauth2ms
```


```bash
# Using pip
pip install oauth2ms

# Or using your package manager on some distributions
# Check if available in your distro's repos
```

### 2. Register OAuth2 Application

#### Step 1: Create Google Cloud Project
1. Go to the [Google Cloud Console](https://console.cloud.google.com/)
2. Click "Select a project" → "New Project"
3. Enter a project name (e.g., "Personal Email Client")
4. Click "Create"

#### Step 2: Enable Gmail API
1. In the Google Cloud Console, go to "APIs & Services" → "Library"
2. Search for "Gmail API"
3. Click on "Gmail API" and then "Enable"

#### Step 3: Configure OAuth Consent Screen
1. Go to "APIs & Services" → "OAuth consent screen"
2. Choose "External" user type (unless you have a Google Workspace account)
3. Fill in the required fields:
   - App name: "Personal Email Client" (or your preference)
   - User support email: your email address
   - Developer contact information: your email address
4. Click "Save and Continue"
5. Skip the "Scopes" step (click "Save and Continue")
6. Add yourself as a test user in the "Test users" section
7. Click "Save and Continue"

#### Step 4: Create OAuth2 Credentials
1. Go to "APIs & Services" → "Credentials"
2. Click "Create Credentials" → "OAuth client ID"
3. Choose "Desktop application" as application type
4. Enter a name (e.g., "Email Client")
5. Click "Create"
6. Copy the **Client ID** and **Client secret** (you'll need these)
7. Optionally download the JSON file for backup

### 3. Generate OAuth2 Token

#### Step 1: Extract Credentials
From the Google Cloud Console, you should have:
- **Client ID**: looks like `123456789-abcdefgh.apps.googleusercontent.com`
- **Client Secret**: looks like `GOCSPX-abcdefghijklmnop`

#### Step 2: Generate Initial Token
```bash
# Replace with your actual credentials
oauth2ms --generate-oauth2-token \
  --client-id="123456789-abcdefgh.apps.googleusercontent.com" \
  --client-secret="GOCSPX-abcdefghijklmnop" \
  --scope="https://mail.google.com/"
```

#### Step 3: Complete Browser Authentication
1. The command will open your default browser
2. Sign in to your Google account
3. Click "Continue" to approve the app (it may show as "unverified")
4. Grant permission to access your Gmail
5. Copy the authorization code from the browser
6. Paste it back into the terminal when prompted

#### Step 4: Save the Tokens
The tool will output:
- **Access Token**: Short-lived token (expires in ~1 hour)
- **Refresh Token**: Long-lived token (save this securely!)

Example output:
```
Access Token: ya29.a0AfH6SMC...
Refresh Token: 1//04xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
```

**Important**: Save the refresh token securely - you'll need it for configuration.

### 4. Test OAuth2 Connection

Before configuring mail sync, test that oauth2ms works with your credentials:

#### Test Token Generation
```bash
# Test that you can generate access tokens
oauth2ms --client-id="123456789-abcdefgh.apps.googleusercontent.com" \
         --client-secret="GOCSPX-abcdefghijklmnop" \
         --refresh-token="1//04xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
```

This should output a fresh access token. If it fails, double-check your credentials.

#### Test IMAP Connection
You can manually test IMAP access using openssl:

```bash
# Get an access token first
ACCESS_TOKEN=$(oauth2ms --quiet \
  --client-id="123456789-abcdefgh.apps.googleusercontent.com" \
  --client-secret="GOCSPX-abcdefghijklmnop" \
  --refresh-token="1//04xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")

# Test IMAP connection (replace your.email@gmail.com with your actual email)
echo -e "a001 AUTHENTICATE XOAUTH2\na002 CAPABILITY\na003 LOGOUT" | \
  openssl s_client -connect imap.gmail.com:993 -quiet 2>/dev/null | \
  grep -E "(CAPABILITY|OK|BAD)"
```

### 5. Configure mbsync for OAuth2

Create or update your `~/.mbsyncrc` with your actual credentials:

```
IMAPAccount gmail-oauth
Host imap.gmail.com
User your.email@gmail.com
AuthMechs XOAUTH2
PassCmd "oauth2ms --quiet --client-id='123456789-abcdefgh.apps.googleusercontent.com' --client-secret='GOCSPX-abcdefghijklmnop' --refresh-token='1//04xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'"
SSLType IMAPS
CertificateFile /etc/ssl/certs/ca-certificates.crt

IMAPStore gmail-remote
Account gmail-oauth

MaildirStore gmail-local
Subfolders Verbatim
Path ~/mail/
Inbox ~/mail/INBOX

Channel gmail-oauth
Far :gmail-remote:
Near :gmail-local:
Patterns * !"[Google Mail]/All Mail"
Create Both
SyncState *
```

#### Secure Credential Storage

For better security, store your credentials in a script:

```bash
# Create ~/bin/gmail-oauth-token.sh
mkdir -p ~/bin
cat > ~/bin/gmail-oauth-token.sh << 'EOF'
#!/bin/bash
oauth2ms --quiet \
  --client-id='123456789-abcdefgh.apps.googleusercontent.com' \
  --client-secret='GOCSPX-abcdefghijklmnop' \
  --refresh-token='1//04xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
EOF

chmod 700 ~/bin/gmail-oauth-token.sh
```

Then update your mbsync configuration:
```
PassCmd "~/bin/gmail-oauth-token.sh"
```

#### Test mbsync Configuration

```bash
# Test the configuration
mbsync -V gmail-oauth

# If successful, do a full sync
mbsync gmail-oauth
```

### 6. Configure SMTP OAuth2

For sending emails via OAuth2, update your `~/.authinfo`:

```
machine smtp.gmail.com login your.email@gmail.com password oauth2:your-access-token port 587
```

Or use a script to generate fresh tokens:

```bash
#!/bin/bash
# save as ~/bin/gmail-oauth2-token.sh
oauth2ms --quiet \
  --client-id=your-client-id \
  --client-secret=your-client-secret \
  --refresh-token=your-refresh-token \
  --scope=https://mail.google.com/
```

Then reference it in mbsync:
```
PassCmd "~/bin/gmail-oauth2-token.sh"
```

### 7. Security Benefits

OAuth2 provides several advantages over app passwords:

- **Granular permissions**: Only grant access to email, not full account
- **Revocable access**: Can revoke access without changing main password
- **Temporary tokens**: Access tokens expire and refresh automatically
- **Audit trail**: Better logging of access in Google Account settings
- **No password storage**: Eliminates need to store app passwords

### 8. Troubleshooting OAuth2

Common OAuth2 issues:

1. **Token expired**: Re-run oauth2ms to refresh tokens
2. **Scope issues**: Ensure correct Gmail scope is requested
3. **Client ID/Secret**: Verify credentials are correct and project has Gmail API enabled
4. **Rate limiting**: Google may throttle requests; add delays between syncs

```bash
# Test OAuth2 authentication
oauth2ms --test --client-id=your-client-id --client-secret=your-client-secret --refresh-token=your-refresh-token

# Debug mbsync with OAuth2
mbsync -V gmail-oauth
```

## Advanced Features

### Email Alerts
The configuration includes mu4e-alert for desktop notifications when new emails arrive.

### HTML Email Viewing
The mu4e-views package provides better HTML email rendering.

### Org-mode Integration
- Store email links in org files
- Capture emails directly to your GTD system
- Schedule follow-ups from emails

### Threading and Search
- Full-text search across all emails
- Intelligent threading
- Bookmarks for common searches

## Maintenance

### Regular Tasks:
1. **Update mail index**: `mu index` (or `C-c e u` in Emacs)
2. **Clean up**: Periodically clean old emails and compact mailboxes
3. **Backup**: Include ~/mail in your backup strategy

### Automation:
Consider setting up a cron job to sync mail automatically:
```bash
# Add to crontab (crontab -e)
*/5 * * * * /usr/bin/mbsync -a
```
