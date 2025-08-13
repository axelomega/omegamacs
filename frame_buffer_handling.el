;;; -*- lexical-binding: t -*-

;;Unique buffer names
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator "#")
  (setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
  (setq uniquify-ignore-buffers-re "^\\*")) ; don't muck with special buffers

;;Easy buffer switching
(use-package windmove
  :ensure t
  :config
  (windmove-default-keybindings 'meta))

;;No scroll bar, info to mode line
(use-package sml-modeline
  :ensure t
  :config
  (sml-modeline-mode 1) ;; show buffer pos in the mode line
  (scroll-bar-mode -1)) ;; turn off the scrollbar

;;Easy frame switching
;;;(global-set-key (kbd "C-c o") 'other-frame)
;;;(require 'framemove)
;;;(windmove-default-keybindings)
;;;(setq framemove-hook-into-windmove t)

;;Show paranthesis
(setq show-paren-delay 0)           ; how long to wait?
(show-paren-mode t)                 ; turn paren-mode on
(setq show-paren-style 'expression) ; alternatives are 'parenthesis' and 'mixed'
;;;(set-face-background 'show-paren-match-face "#000033")
;;;(set-face-attribute 'show-paren-match-face nil
;;;        :weight 'bold :underline nil :overline nil :slant 'normal)

