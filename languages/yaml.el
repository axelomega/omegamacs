;;; -*- lexical-binding: t -*-

;; YAML mode for editing YAML files
(use-package yaml-mode
  :ensure t
  :defer t
  :mode ("\\.ya?ml\\'" . yaml-mode)
  :config
  (add-hook 'yaml-mode-hook
            (lambda ()
              (define-key yaml-mode-map "\C-m" 'newline-and-indent))))
