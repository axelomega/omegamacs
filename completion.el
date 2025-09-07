;;; -*- lexical-binding: t -*-

;; Modern completion with vertico and friends
;; Replaces the old ido-mode configuration with modern alternatives

;; Vertico - vertical completion UI (replaces ido-mode)
(use-package vertico
  :ensure t
  :init (vertico-mode)
  :config
  (setq vertico-cycle t
        vertico-resize t
        vertico-count 20))

;; Vertico directory extension for better file navigation
(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Savehist - persist minibuffer history (replaces ido-save-directory-list-file)
(use-package savehist
  :ensure nil
  :init
  (savehist-mode)
  :config
  (setq savehist-file (my-user-emacs-file-local "savehist" "cache")
        savehist-additional-variables '(search-ring regexp-search-ring)
        savehist-autosave-interval 60))

;; Recentf - track recent files (enhances file completion)
(use-package recentf
  :ensure nil
  :init
  (recentf-mode)
  :config
  (setq recentf-save-file (my-user-emacs-file-local "recentf" "cache")
        recentf-max-saved-items 200
        recentf-max-menu-items 50
        recentf-exclude '("^\*trace" "^\*GTAGS" "^session\\..*" "^\*"
                          ".*\\.mak$" "/tmp/" "/var/" "COMMIT_EDITMSG"
                          "\\.gz$" "\\.elc$" "~$")))

;; Orderless - flexible completion matching (replaces ido-enable-flex-matching)
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))
                                      (command (styles orderless))
                                      (buffer (styles orderless))
                                      (symbol (styles orderless)))))

;; Marginalia - rich annotations for completions
(use-package marginalia
  :ensure t
  :init (marginalia-mode)
  :config
  (setq marginalia-align 'right))

;; Consult - enhanced completion commands
(use-package consult
  :ensure t
  :defer my-enable-lazy-loading
  :bind (;; Replace common ido bindings
         ("C-x b" . consult-buffer)                ; replaces ido-switch-buffer
         ("C-x C-b" . consult-buffer)              ; alternative buffer switching
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Enhanced file finding (complements find-file)
         ("M-s f" . consult-find)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-y" . consult-yank-pop)
         ;; History and navigation
         ("C-c h" . consult-history)
         ("M-g g" . consult-goto-line)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; Error navigation
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake))
  :config
  ;; Configure preview
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))

  ;; Use consult for xref (similar to ido-everywhere behavior)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

;; Embark - act on completion targets
(use-package embark
  :ensure t
  :defer my-enable-lazy-loading
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide mode line in embark buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Embark-consult integration
(use-package embark-consult
  :ensure t
  :defer my-enable-lazy-loading
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Enhanced minibuffer behavior
(setq enable-recursive-minibuffers t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t
      ;; Don't confirm for new files/buffers (like ido setting)
      confirm-nonexistent-file-or-buffer nil
      ;; Use completing-read for various operations
      use-file-dialog nil)

;; Improve default completion behavior
(setq completion-cycle-threshold 3
      tab-always-indent 'complete
      completions-detailed t)

;; Better minibuffer completion display
(setq resize-mini-windows t
      max-mini-window-height 0.33)

;; Corfu - in-buffer completion popup (complements company-mode)
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :config
  (setq corfu-cycle t
        corfu-auto t
        corfu-auto-delay 0.2
        corfu-auto-prefix 2
        corfu-quit-no-match 'separator)

  ;; Make corfu work better with orderless
  (setq corfu-separator ?\s))

;; Cape - completion at point extensions
(use-package cape
  :ensure t
  :init
  ;; Add useful completion functions to completion-at-point-functions
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  :config
  ;; Silence the pcomplete capf, no errors or messages!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))
