;;; -*- lexical-binding: t -*-
;;; Full Copilot configuration for Omegamacs
;;; This is the complete configuration - switch to this once basic setup works

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el" :rev :newest :branch "main")
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)
              ("M-<right>" . 'copilot-accept-completion-by-word)
              ("M-<down>" . 'copilot-next-completion)
              ("M-<up>" . 'copilot-previous-completion)
              ("C-g" . 'copilot-clear-overlay))
  :config
  ;; Performance and behavior settings
  (setq copilot-idle-delay 0.2)                    ; Delay before showing suggestions
  (setq copilot-max-char -1)                       ; No character limit
  (setq copilot-log-max 1000)                      ; Keep more logs for debugging

  ;; Visual settings
  (setq copilot-completion-face 'shadow)           ; Use shadow face for suggestions

  ;; Language-specific settings
  (add-to-list 'copilot-major-mode-alist '("verilog" . "systemverilog"))
  (add-to-list 'copilot-major-mode-alist '("verilog-mode" . "systemverilog"))

  ;; Enable for specific modes
  (add-hook 'c-mode-hook 'copilot-mode)
  (add-hook 'c++-mode-hook 'copilot-mode)
  (add-hook 'python-mode-hook 'copilot-mode)
  (add-hook 'verilog-mode-hook 'copilot-mode)
  (add-hook 'emacs-lisp-mode-hook 'copilot-mode)
  (add-hook 'latex-mode-hook 'copilot-mode)
  (add-hook 'markdown-mode-hook 'copilot-mode)
  (add-hook 'sh-mode-hook 'copilot-mode)
  (add-hook 'makefile-mode-hook 'copilot-mode)

  ;; Disable in certain modes/contexts
  (add-to-list 'copilot-disable-predicates
               (lambda () (derived-mode-p 'shell-mode)))
  (add-to-list 'copilot-disable-predicates
               (lambda () (derived-mode-p 'eshell-mode)))
  (add-to-list 'copilot-disable-predicates
               (lambda () (derived-mode-p 'term-mode)))
  (add-to-list 'copilot-disable-predicates
               (lambda () (derived-mode-p 'vterm-mode)))

  ;; Custom functions for better integration
  (defun my-copilot-toggle ()
    "Toggle Copilot mode on/off."
    (interactive)
    (if copilot-mode
        (progn
          (copilot-mode -1)
          (message "Copilot disabled"))
      (progn
        (copilot-mode 1)
        (message "Copilot enabled"))))

  (defun my-copilot-complete-or-accept ()
    "Accept Copilot suggestion or trigger completion if no suggestion."
    (interactive)
    (if (copilot--overlay-visible)
        (copilot-accept-completion)
      (if (fboundp 'company-manual-begin)
          (company-manual-begin)
        (completion-at-point))))

  (defun my-copilot-accept-word-or-line ()
    "Accept Copilot suggestion by word, or full line if at end."
    (interactive)
    (if (copilot--overlay-visible)
        (if (eolp)
            (copilot-accept-completion)
          (copilot-accept-completion-by-word))
      (message "No Copilot suggestion available")))

  ;; Global keybindings for Copilot control
  (global-set-key (kbd "C-c c t") 'my-copilot-toggle)
  (global-set-key (kbd "C-c c c") 'copilot-complete)
  (global-set-key (kbd "C-c c d") 'copilot-diagnose)
  (global-set-key (kbd "C-c c l") 'copilot-login)
  (global-set-key (kbd "C-c c s") 'copilot-logout)

  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))

  ;; Enhanced TAB behavior that works with indentation
  (defun my-copilot-tab-or-indent ()
    "Accept Copilot completion or indent line."
    (interactive)
    (if (copilot--overlay-visible)
        (copilot-accept-completion)
      (indent-for-tab-command)))

  ;; Override TAB in programming modes
  (define-key prog-mode-map (kbd "<tab>") 'my-copilot-tab-or-indent)
  (define-key prog-mode-map (kbd "TAB") 'my-copilot-tab-or-indent)

  ;; Mode line indicator
  (defun my--copilot-mode-line ()
    "Return mode line string for Copilot status."
    (when copilot-mode
      (if (copilot--overlay-visible)
          " ✓Co"
        " Co")))

  ;; Add to mode line
  (add-to-list 'mode-line-misc-info '(:eval (my--copilot-mode-line)))

  ;; Advice to improve interaction with other completion systems
  (advice-add 'copilot-accept-completion :around
              (lambda (orig-fun &rest args)
                "Clear other completion overlays when accepting Copilot."
                (when (fboundp 'company-abort)
                  (company-abort))
                (apply orig-fun args)))

  ;; Better integration with LSP
  (when (boundp 'lsp-completion-at-point-functions)
    (setq copilot-disable-predicates
          (append copilot-disable-predicates
                  '((lambda () (and (boundp 'lsp-mode) lsp-mode
                                   (not (eq (point) (line-end-position)))))))))

  ;; Customize faces for better visibility
  (custom-set-faces
   '(copilot-overlay-face ((t (:foreground "#6272A4" :italic t)))))

  ;; Diagnostic function for troubleshooting
  (defun my-copilot-status ()
    "Show detailed Copilot status information."
    (interactive)
    (message "Copilot mode: %s | Logged in: %s | Version: %s | Node.js: %s"
             (if copilot-mode "ON" "OFF")
             (if (copilot-logged-in) "YES" "NO")
             (or copilot-version "unknown")
             (or copilot--node-executable "not found")))

  (global-set-key (kbd "C-c c i") 'my-copilot-status))
