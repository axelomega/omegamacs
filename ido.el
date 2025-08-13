;;; -*- lexical-binding: t -*-

;; ido makes competing buffers and finding files easier
;; http://www.emacswiki.org/cgi-bin/wiki/InteractivelyDoThings
(use-package ido
  :ensure t
  :config
  (ido-mode 'both) ;; for buffers and files
  (setq
   ido-save-directory-list-file "~/.emacs.d/cache/ido.last"

   ido-ignore-buffers ;; ignore these guys
   '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace" "^\*GTAGS" "^session\.*" "^\*" ".*\.mak")
                                        ;  ido-work-directory-list '("~/" "~/Desktop" "~/Documents" "~src")
   ido-case-fold  t                 ; be case-insensitive
   ido-everywhere t                 ; use for many file dialogs
   ido-enable-last-directory-history t ; remember last used dirs
   ido-max-work-directory-list 30   ; should be enough
   ido-max-work-file-list      50   ; remember many
   ido-use-filename-at-point nil    ; don't use filename at point (annoying)
   ido-use-url-at-point nil         ; don't use url at point (annoying)

   ido-enable-flex-matching t     ; be smart
   ido-max-prospects 16              ; don't spam my minibuffer
   ido-confirm-unique-completion nil) ; wait for RET, even with unique completion

  ;; when using ido, the confirmation is rather annoying...
  (setq confirm-nonexistent-file-or-buffer nil))
