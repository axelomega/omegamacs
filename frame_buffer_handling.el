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

;;Show paranthesis
(setq show-paren-delay 0)           ; how long to wait?
(show-paren-mode t)                 ; turn paren-mode on
(setq show-paren-style 'expression) ; alternatives are 'parenthesis' and 'mixed'

(defhydra hydra-window (:color amaranth :hint nil)
  "
 Movement^^        ^Split^         ^Resize^
----------------------------------------------------------------
 _h_ ←             _v_ vertical    _H_ shrink horizontal
 _j_ ↓              _x_ horizontal  _L_ enlarge horizontal
 _k_ ↑              _s_ swap        _J_ shrink vertical
 _l_ →             _d_ delete      _K_ enlarge vertical
 ^ ^                ^ ^             _=_ balance
 ^ ^                ^ ^             ^q^ quit
"
    ;; Movement
    ("h" windmove-left)
    ("j" windmove-down)
    ("k" windmove-up)
    ("l" windmove-right)
    ;; Split
    ("v" split-window-right)
    ("x" split-window-below)
    ("s" window-swap-states)
    ("d" delete-window)
    ;; Resize (with larger steps for better UX)
    ("H" (shrink-window-horizontally 3))
    ("L" (enlarge-window-horizontally 3))
    ("J" (shrink-window 3))
    ("K" (enlarge-window 3))
    ("=" balance-windows)
    ;; Exit
    ("q" nil :exit t))

  ;; Bind it globally (C-c w)
(global-set-key (kbd "C-c w") 'hydra-window/body)

(defhydra hydra-buffer (:color pink :hint nil)
  "
 Buffers: _n_ next   _p_ prev   _b_ switch   _k_ kill   _q_ quit
"
  ("n" next-buffer)
  ("p" previous-buffer)
  ("b" switch-to-buffer)
  ("k" kill-current-buffer)
  ("q" nil :exit t))

(global-set-key (kbd "C-c b") 'hydra-buffer/body)
