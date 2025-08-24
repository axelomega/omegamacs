;;; -*- lexical-binding: t -*-

(use-package org
  :ensure t
  :pin gnu
  :bind ("C-c o" . hydra-org/body)
  :init
  ;; Org files should be accessible from $HOME in general
  (setq my--org-dir (my-user-emacs-subdirectory "org")
        my--org-file-inbox (expand-file-name "inbox.org" my--org-dir)
        my--org-file-projects (expand-file-name "projects.org" my--org-dir)
        my--org-file-next (expand-file-name "next.org" my--org-dir)
        my--org-file-someday (expand-file-name "someday.org" my--org-dir)
        my--org-file-journal (expand-file-name "journal.org" my--org-dir))

  ;; Where your GTD files live
  (setq org-directory my--org-dir
        org-agenda-files (list my--org-file-inbox
                               my--org-file-projects
                               my--org-file-next
                               my--org-file-someday
                               my--org-file-journal))

  ;; [Claude] ensure that all org files exist
  (dolist (file org-agenda-files)
    (unless (file-exists-p file)
      (with-temp-buffer
        (write-file file))))

  :config
  ;; --- Core GTD knobs ---
  (setq org-log-done 'time
        org-log-into-drawer t
        org-startup-indented t
        org-hide-emphasis-markers t
        org-use-fast-todo-selection t
        org-tags-column 0)

  ;; TODO workflow
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "PROJECT(j)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)")))
  (setq org-todo-keyword-faces
        '(("NEXT" . (:weight bold))
          ("WAIT" . (:slant italic :inherit warning))))
  ;; Context tags (press C-c C-q)
  (setq org-tag-alist
        '((:startgroup) ("@home" . ?h) ("@work" . ?w) ("@computer" . ?c) ("@phone" . ?p) ("@errand" . ?e) (:endgroup)
          ("someday" . ?s)))

  ;; Refile: move items from inbox to project/next/someday
  (setq org-refile-targets
     `((,my--org-file-projects :maxlevel . 3)
       (,my--org-file-next     :maxlevel . 2)
       (,my--org-file-someday  :maxlevel . 2)))

  (setq org-outline-path-complete-in-steps nil
        org-refile-use-outline-path 'file) ;; completion like: projects.org/Projects/…

  ;; Capture (C-c c)
  (setq org-default-notes-file my--org-file-inbox)
  (setq org-capture-templates
        `(
          ;; Quick inbox task
          ("t" "Todo → Inbox" entry
           (file+headline my--org-file-inbox "Inbox")
           "* TODO %?\n:PROPERTIES:\n:Created: %U\n:END:\n%a\n" :empty-lines 1)

          ;; Next action to Next file
          ("n" "Next action" entry
           (file my--org-file-next)
           "* NEXT %? %^g\n%a\n" :empty-lines 1)

          ;; New project skeleton
          ("p" "Project" entry
           (file+headline my--org-file-projects "Projects")
           "* PROJECT %^{Project name}\n** NEXT %?\n" :empty-lines 1)

          ;; Someday/Maybe
          ("s" "Someday" entry
           (file my--org-file-someday)
           "* TODO %? :someday:\n" :empty-lines 1)

          ;; Journal
          ("j" "Journal" entry
           (file+datetree my--org-file-journal)
           "* %U %?\n%i\n" :empty-lines 1)

          ;; Meeting note + clocking
          ("m" "Meeting (clocked)" entry
           (file+datetree my--org-file-journal)
           "* %^{Title}\n:PROPERTIES:\n:Participants: %^{Who}\n:END:\n%U\n%?\n"
           :clock-in t :clock-resume t :empty-lines 1)
          ))

  ;; Agenda custom views (C-c a a or C-c a g)
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 50)

  (setq org-agenda-custom-commands
        '(("g" "GTD"
           ((agenda "" ((org-agenda-span 7)))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Next actions")
                   (org-agenda-sorting-strategy '(priority-down todo-state-down))))
            (todo "WAIT"
                  ((org-agenda-overriding-header "Waiting for")))
            (todo "TODO"
                  ((org-agenda-overriding-header "Inbox (unsorted)")
                   (org-agenda-files (list my--org-file-inbox)))))
           nil)))


  ;; Optional: export PDF via LaTeX (install TeX separately)
  ;; (setq org-latex-compiler "xelatex")
)

(use-package org-super-agenda
  :ensure t
  :after org
  :config
  (org-super-agenda-mode)
  (setq org-super-agenda-groups
        '((:name "Today"
                 :time-grid t
                 :date today)
          (:name "Important"
                 :priority "A")
          (:name "Overdue"
                 :deadline past)
          (:name "Due today"
                 :deadline today)
          (:name "Due soon"
                 :deadline future)
          (:name "Next actions"
                 :todo "NEXT")
          (:name "Projects"
                 :todo "PROJECT")
          (:name "Waiting"
                 :todo "WAIT")
          (:name "Inbox"
                 :todo "TODO"))))

(defhydra hydra-org (:color blue :hint nil)
  "
^Capture^           ^Navigate^          ^View^
^^^^^^^^--------------------------------------------
_c_: capture        _a_: agenda         _l_: store link
_j_: journal        _r_: refile         _i_: insert link
_t_: todo           _g_: goto           _o_: open at point
_p_: project        _s_: search
_m_: meeting        _d_: deadline
"
  ("c" org-capture)
  ("j" (org-capture nil "j"))
  ("t" (org-capture nil "t"))
  ("p" (org-capture nil "p"))
  ("m" (org-capture nil "m"))
  ("a" org-agenda)
  ("r" org-refile)
  ("g" org-goto)
  ("s" org-search-view)
  ("d" org-deadline)
  ("l" org-store-link)
  ("i" org-insert-link)
  ("o" org-open-at-point)
  ("q" nil "quit"))
