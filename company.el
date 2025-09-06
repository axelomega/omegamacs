;;; -*- lexical-binding: t -*-

(use-package company
  :ensure t
  :init (global-company-mode)
  :config
  ;; Performance and behavior settings
  (setq company-minimum-prefix-length 2
        company-idle-delay 0.3
        company-selection-wrap-around t
        company-show-numbers t
        company-tooltip-align-annotations t
        company-tooltip-maximum-width 100
        company-tooltip-minimum-width 20)

  ;; Better sorting - show frequently used completions first
  (setq company-transformers '(company-sort-by-occurrence))

  ;; Improved keybindings
  :bind (:map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("C-d" . company-show-doc-buffer)
         ("M-." . company-show-location)
         ("<tab>" . company-complete-selection)))

;; Enhanced company UI with icons and better styling
(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-icons-alist 'company-box-icons-all-the-icons
        company-box-max-candidates 50
        company-box-doc-delay 0.5))
