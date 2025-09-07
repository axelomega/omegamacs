;;; -*- lexical-binding: t -*-

;; Load only essential settings
(load (omegamacs-get-fullpath "settings"))

(windmove-default-keybindings)

(when (executable-find "aspell")
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")))

(electric-pair-mode 1)
(show-paren-mode 1)
(global-auto-revert-mode 1)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
