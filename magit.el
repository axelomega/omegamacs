;;; -*- lexical-binding: t -*-

;; Git integration enhancements
(use-package diff-hl
  :ensure t
  :hook ((prog-mode . diff-hl-mode)
         (vc-dir-mode . diff-hl-dir-mode))
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode))

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

;; For initial setup see https://magit.vc/manual/forge/Initial-Setup.html
(use-package forge
  :ensure t
  :defer omegamacs-enable-lazy-loading
  :after magit)

(defhydra hydra-git (:color teal :hint nil)
  "
 Git: _s_ status   _l_ log    _d_ diff   _c_ commit   _P_ push    _F_ fetch
      _b_ blame    _B_ branch _S_ stash  _m_ merge    _p_ pull    _q_ quit
"
  ("s" magit-status)
  ("l" magit-log-current)
  ("d" magit-diff-buffer-file)
  ("c" magit-commit)
  ("P" magit-push)
  ("F" magit-fetch)
  ("p" magit-pull)
  ("b" magit-blame)
  ("B" magit-checkout)
  ("S" magit-stash)
  ("m" magit-merge)
  ("q" nil :exit t))

(global-set-key (kbd "C-c g") 'hydra-git/body)
