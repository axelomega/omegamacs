;;; -*- lexical-binding: t -*-

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :config
  ;; Performance optimizations
  (setq magit-diff-refine-hunk nil)           ; Disable word-level highlighting
  (setq magit-revision-show-gravatars nil)    ; Disable gravatar loading
  (setq magit-refresh-status-buffer nil)      ; Don't auto-refresh status buffer
  (setq magit-refresh-verbose nil)            ; Reduce refresh verbosity
  (setq magit-section-initial-visibility-alist
        '((stashes . hide)                    ; Hide stashes by default
          (untracked . hide)                  ; Hide untracked files by default
          (unpushed . show)))                 ; Show unpushed commits
  (setq magit-save-repository-buffers 'dontask) ; Don't ask to save buffers
  (setq magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1)) ; Faster buffer display

(defhydra hydra-git (:color teal :hint nil)
  "
 Git: _s_ status   _l_ log   _d_ diff   _c_ commit   _p_ push   _b_ blame   _q_ quit
"
  ("s" magit-status)
  ("l" magit-log-current)
  ("d" magit-diff-buffer-file)
  ("c" magit-commit-create)
  ("p" magit-push-current)
  ("b" magit-blame)
  ("q" nil :exit t))

(global-set-key (kbd "C-c g") 'hydra-git/body)
