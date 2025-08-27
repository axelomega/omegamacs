;;; -*- lexical-binding: t -*-

;; Comprehensive mu4e email configuration for omegamacs
;; Integrates with existing org-mode GTD workflow and hydra system

(use-package mu4e
  :ensure nil  ; mu4e comes with mu installation
  :bind ("C-c e" . hydra-email/body)
  :init
  ;; Email directories and files
  (setq my--email-dir (expand-file-name "mail" (getenv "HOME"))
        my--email-sent-folder "/Sent"
        my--email-drafts-folder "/Drafts"
        my--email-trash-folder "/Trash"
        my--email-refile-folder "/Archive")

  ;; Ensure mu4e is in load path if installed via system package manager
  (when (file-directory-p "/usr/share/emacs/site-lisp/mu4e")
    (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e"))

  :config
  ;; --- Core mu4e settings ---
  (setq mu4e-maildir my--email-dir
        mu4e-get-mail-command "mbsync -a"  ; or "offlineimap" or "fetchmail"
        mu4e-update-interval 60  ; Update every minute
        mu4e-compose-format-flowed t
        mu4e-view-show-addresses t
        mu4e-view-show-images t
        mu4e-view-image-max-width 800
        mu4e-compose-dont-reply-to-self t
        mu4e-headers-auto-update t
        mu4e-headers-skip-duplicates t
        mu4e-change-filenames-when-moving t
        mu4e-attachment-dir (expand-file-name "Downloads" (getenv "HOME")))

  ;; --- Folder configuration (adjust paths according to your mail setup) ---
  (setq mu4e-drafts-folder my--email-drafts-folder
        mu4e-sent-folder my--email-sent-folder
        mu4e-trash-folder my--email-trash-folder
        mu4e-refile-folder my--email-refile-folder)

  ;; --- Headers view customization ---
  (setq mu4e-headers-fields
        '((:human-date . 12)
          (:flags . 6)
          (:from-or-to . 25)
          (:subject . nil)))

  ;; --- Message view customization ---
  (setq mu4e-view-fields
        '(:from :to :cc :subject :flags :date :maildir :mailing-list :tags :attachments :signature :decryption))

  ;; --- Compose settings ---
  (setq mu4e-compose-signature-auto-include t
        mu4e-compose-signature (concat
                               "Best regards,\n"
                               user-full-name "\n"))

  ;; --- Context switching for multiple accounts (example) ---
  ;; Uncomment and modify for multiple email accounts
  ;; (setq mu4e-contexts
  ;;       `(,(make-mu4e-context
  ;;           :name "work"
  ;;           :match-func (lambda (msg) (when msg (string-prefix-p "/work" (mu4e-message-field msg :maildir))))
  ;;           :vars '((user-mail-address . "you@work.com")
  ;;                   (user-full-name . "Your Name")
  ;;                   (mu4e-drafts-folder . "/work/Drafts")
  ;;                   (mu4e-sent-folder . "/work/Sent")
  ;;                   (mu4e-trash-folder . "/work/Trash")
  ;;                   (mu4e-refile-folder . "/work/Archive")))
  ;;         ,(make-mu4e-context
  ;;           :name "personal"
  ;;           :match-func (lambda (msg) (when msg (string-prefix-p "/personal" (mu4e-message-field msg :maildir))))
  ;;           :vars '((user-mail-address . "you@personal.com")
  ;;                   (user-full-name . "Your Name")
  ;;                   (mu4e-drafts-folder . "/personal/Drafts")
  ;;                   (mu4e-sent-folder . "/personal/Sent")
  ;;                   (mu4e-trash-folder . "/personal/Trash")
  ;;                   (mu4e-refile-folder . "/personal/Archive")))))

  ;; --- Bookmarks for quick access ---
  (setq mu4e-bookmarks
        '((:name "Unread messages" :query "flag:unread AND NOT flag:trashed" :key ?u)
          (:name "Today's messages" :query "date:today..now" :key ?t)
          (:name "Last 7 days" :query "date:7d..now" :hide-unread t :key ?w)
          (:name "Messages with images" :query "mime:image/*" :key ?p)
          (:name "Messages with attachments" :query "flag:attach" :key ?a)
          (:name "Flagged messages" :query "flag:flagged" :key ?f)
          (:name "Large messages (>5MB)" :query "size:5M..500M" :key ?l)))

  ;; --- Sending mail configuration ---
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-stream-type 'starttls
        smtpmail-default-smtp-server "smtp.gmail.com"  ; Adjust for your provider
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        smtpmail-debug-info t)

  ;; --- Integration with org-mode ---
  (setq mu4e-org-contacts-file (expand-file-name "contacts.org" my--org-dir))

  ;; Store link to messages for org-mode
  (require 'org-mu4e nil t)

  ;; --- Actions for messages ---
  (add-to-list 'mu4e-headers-actions
               '("org capture message" . mu4e-action-capture-message) t)
  (add-to-list 'mu4e-view-actions
               '("org capture message" . mu4e-action-capture-message) t)

  ;; --- Appearance customization ---
  (setq mu4e-headers-date-format "%Y-%m-%d %H:%M"
        mu4e-headers-time-format "%H:%M"
        mu4e-view-date-format "%Y-%m-%d %H:%M")

  ;; Use fancy characters
  (setq mu4e-use-fancy-chars t
        mu4e-headers-has-child-prefix '("├>" . "├▶")
        mu4e-headers-empty-parent-prefix '("└>" . "└▶")
        mu4e-headers-first-child-prefix '("├>" . "├▶")
        mu4e-headers-duplicate-prefix '("=" . "≡")
        mu4e-headers-default-prefix '("|" . "┃"))

  ;; --- Performance optimizations ---
  (setq mu4e-headers-buffer-max-size 1000)

  ;; --- Alerts integration (optional) ---
  (when (require 'mu4e-alert nil t)
    (mu4e-alert-set-default-style 'notifier)
    (mu4e-alert-enable-notifications)
    (mu4e-alert-enable-mode-line-display)))

;; Enhanced mu4e configuration with additional packages
(use-package mu4e-alert
  :ensure t
  :after mu4e
  :config
  (setq mu4e-alert-interesting-mail-query "flag:unread AND NOT flag:trashed AND NOT maildir:/Spam")
  (mu4e-alert-enable-mode-line-display)
  (mu4e-alert-enable-notifications))

;; Better HTML email viewing
(use-package mu4e-views
  :ensure t
  :after mu4e
  :config
  (setq mu4e-views-completion-method 'ivy)  ; or 'helm if you use helm
  (setq mu4e-views-default-view-method "html")
  (mu4e-views-mu4e-use-view-msg-method "html"))

;; Org-mode integration for email capture
(with-eval-after-load 'org-capture
  ;; Add email capture templates to existing org-capture-templates
  (add-to-list 'org-capture-templates
               '("E" "Email Templates"))
  (add-to-list 'org-capture-templates
               '("Et" "Email → Todo" entry
                 (file+headline my--org-file-inbox "Inbox")
                 "* TODO %a\n:PROPERTIES:\n:Created: %U\n:END:\n%?\n" :empty-lines 1))
  (add-to-list 'org-capture-templates
               '("En" "Email → Next Action" entry
                 (file my--org-file-next)
                 "* NEXT %a %^g\n%?\n" :empty-lines 1))
  (add-to-list 'org-capture-templates
               '("Ef" "Email → Follow Up" entry
                 (file+headline my--org-file-inbox "Follow Up")
                 "* TODO Follow up: %a\nSCHEDULED: %^t\n:PROPERTIES:\n:Created: %U\n:END:\n%?\n" :empty-lines 1))
  (add-to-list 'org-capture-templates
               '("Er" "Email → Reference" entry
                 (file+headline my--org-file-projects "Reference")
                 "* %a\n:PROPERTIES:\n:Created: %U\n:END:\n%?\n" :empty-lines 1)))

;; Hydra for email operations
(defhydra hydra-email (:color blue :hint nil)
  "
^Email Dashboard^    ^Compose^        ^Search^         ^Actions^
^^^^^^^^------------------------------------------------------------------------
_i_: inbox          _c_: compose     _s_: search      _u_: update mail
_t_: today          _r_: reply       _/_: mu4e-search _q_: quit mu4e
_w_: week          _f_: forward     _b_: bookmarks   _k_: kill buffers
_a_: all mail      _d_: draft       _j_: jump        _!_: shell command
_U_: unread        _m_: mail dir    _g_: goto        _h_: help
"
  ("i" (mu4e-headers-search "maildir:/INBOX"))
  ("t" (mu4e-headers-search "date:today..now"))
  ("w" (mu4e-headers-search "date:7d..now"))
  ("a" (mu4e-headers-search "maildir:/"))
  ("U" (mu4e-headers-search "flag:unread"))
  ("c" mu4e-compose-new)
  ("r" mu4e-compose-reply)
  ("f" mu4e-compose-forward)
  ("d" mu4e-compose-edit)
  ("s" mu4e-search)
  ("/" mu4e-search-next)
  ("b" mu4e-bookmarks)
  ("j" mu4e~headers-jump-to-maildir)
  ("g" mu4e-select-other-view)
  ("m" mu4e-main-view)
  ("u" mu4e-update-mail-and-index)
  ("q" mu4e-quit)
  ("k" mu4e-kill-update-mail)
  ("!" mu4e-shell-command)
  ("h" mu4e-display-manual)
  ("C-c" nil "quit"))

;; Custom functions for enhanced workflow
(defun my-mu4e-action-capture-message (msg)
  "Capture email to org-mode with different templates."
  (interactive)
  (let ((org-capture-templates
         '(("t" "Task" entry (file+headline my--org-file-inbox "Inbox")
            "* TODO %a\n:PROPERTIES:\n:Created: %U\n:END:\n%?")
           ("n" "Next Action" entry (file my--org-file-next)
            "* NEXT %a %^g\n%?")
           ("f" "Follow Up" entry (file+headline my--org-file-inbox "Follow Up")
            "* TODO Follow up: %a\nSCHEDULED: %^t\n:PROPERTIES:\n:Created: %U\n:END:\n%?")
           ("r" "Reference" entry (file+headline my--org-file-projects "Reference")
            "* %a\n:PROPERTIES:\n:Created: %U\n:END:\n%?"))))
    (org-capture)))

;; Better email threading
(setq mu4e-headers-include-related t)
(setq mu4e-headers-show-threads t)

;; Auto-complete addresses from org contacts
(when (require 'org-contacts nil t)
  (setq mu4e-org-contacts-file (expand-file-name "contacts.org" my--org-dir))
  (add-to-list 'mu4e-headers-actions
               '("add contact" . mu4e-action-add-org-contact) t)
  (add-to-list 'mu4e-view-actions
               '("add contact" . mu4e-action-add-org-contact) t))

;; Security enhancements
(setq mu4e-compose-crypto-reply-encrypted-policy 'sign-and-encrypt
      mu4e-compose-crypto-reply-plain-policy 'sign
      mu4e-view-auto-mark-as-read nil)  ; Don't auto-mark as read

;; Custom key bindings for mu4e
(with-eval-after-load 'mu4e
  (define-key mu4e-headers-mode-map (kbd "c") 'my-mu4e-action-capture-message)
  (define-key mu4e-view-mode-map (kbd "c") 'my-mu4e-action-capture-message)
  (define-key mu4e-headers-mode-map (kbd "C-c C-c") 'mu4e-org-store-and-capture)
  (define-key mu4e-view-mode-map (kbd "C-c C-c") 'mu4e-org-store-and-capture))

;; Performance: don't keep too many headers buffers
(setq mu4e-headers-buffer-max-size 500)

;; Email signature based on context (if using contexts)
(defun my-mu4e-set-signature ()
  "Set signature based on current context."
  (cond
   ((string-match-p "work" (or mu4e-compose-parent-message-id ""))
    (setq mu4e-compose-signature
          (concat "Best regards,\n"
                  user-full-name "\n"
                  "Work Phone: +1-555-0123\n")))
   (t
    (setq mu4e-compose-signature
          (concat "Best regards,\n"
                  user-full-name "\n")))))

(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-signature)
