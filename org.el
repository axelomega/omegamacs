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
        my--org-file-journal (expand-file-name "journal.org" my--org-dir)
        my--org-file-contacts (expand-file-name "contacts.org" my--org-dir))

  ;; Where your GTD files live
  (setq org-directory my--org-dir
        org-agenda-files (list my--org-file-inbox
                               my--org-file-projects
                               my--org-file-next
                               my--org-file-someday
                               my--org-file-journal))

  ;; [Claude] ensure that all org files exist
  (dolist (file (append org-agenda-files (list my--org-file-contacts)))
    (unless (file-exists-p file)
      (with-temp-buffer
        (write-file file))))

  ;; Enhanced agenda settings for super-agenda
  (setq org-agenda-block-separator ?─
        org-agenda-window-setup 'current-window
        org-agenda-restore-windows-after-quit t
        org-agenda-compact-blocks t
        org-agenda-skip-scheduled-if-deadline-is-shown t
        org-agenda-skip-deadline-prewarning-if-scheduled t)

  :config
  ;; --- Core GTD knobs ---
  (setq org-log-done 'time
        org-log-into-drawer t
        org-startup-indented t
        org-hide-emphasis-markers t
        org-use-fast-todo-selection t
        org-tags-column 0
        ;; Priority settings for better super-agenda integration
        org-priority-faces '((?A . (:foreground "red" :weight bold))
                            (?B . (:foreground "orange" :weight bold))
                            (?C . (:foreground "green")))
        org-agenda-deadline-leaders '("Overdue: " "Due in %2d days: " "")
        org-agenda-scheduled-leaders '("Scheduled: " "Sched %2dx: ")
        ;; Show context in agenda
        org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
                                  (todo . " %i %-12:c")
                                  (tags . " %i %-12:c")
                                  (search . " %i %-12:c")))

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
        '(("g" "GTD Dashboard"
           ((agenda "" ((org-agenda-span 7)
                        (org-super-agenda-groups
                         '((:name "Today's Schedule"
                                  :time-grid t
                                  :date today)
                           (:name "⚡ Overdue"
                                  :deadline past
                                  :face error)
                           (:name "📅 Due Today"
                                  :deadline today)
                           (:name "⏰ Due Soon"
                                  :deadline future
                                  :scheduled future)
                           (:name "Habit Tracker"
                                  :habit t)
                           (:auto-category t)))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '((:name "🚨 Critical & Urgent"
                                   :priority "A"
                                   :todo "NEXT")
                            (:name "🎯 Next Actions"
                                   :todo "NEXT"
                                   :order 1)
                            (:name "📋 Active Projects"
                                   :todo "PROJECT"
                                   :order 2)
                            (:name "⏳ Waiting For"
                                   :todo "WAIT"
                                   :order 3)
                            (:name "📥 Inbox (Process These!)"
                                   :file-path "inbox.org"
                                   :order 4)
                            (:name "🏠 @Home Context"
                                   :tag "@home"
                                   :todo ("NEXT" "TODO")
                                   :order 5)
                            (:name "💼 @Work Context"
                                   :tag "@work"
                                   :todo ("NEXT" "TODO")
                                   :order 6)
                            (:name "💻 @Computer Tasks"
                                   :tag "@computer"
                                   :todo ("NEXT" "TODO")
                                   :order 7)
                            (:name "📞 @Phone Context"
                                   :tag "@phone"
                                   :todo ("NEXT" "TODO")
                                   :order 8)
                            (:name "🚗 @Errands"
                                   :tag "@errand"
                                   :todo ("NEXT" "TODO")
                                   :order 9)
                            (:name "💭 Someday/Maybe"
                                   :tag "someday"
                                   :order 10)
                            (:discard (:anything t)))))))
           nil)

          ("n" "Next Actions by Context"
           ((todo "NEXT" ((org-agenda-overriding-header "Next Actions by Context")
                          (org-super-agenda-groups
                           '((:name "🏠 @Home" :tag "@home")
                             (:name "💼 @Work" :tag "@work")
                             (:name "💻 @Computer" :tag "@computer")
                             (:name "📞 @Phone" :tag "@phone")
                             (:name "🚗 @Errands" :tag "@errand")
                             (:name "Other" :anything t))))))
           nil)

          ("p" "Projects Review"
           ((todo "PROJECT" ((org-agenda-overriding-header "Project Status")
                             (org-super-agenda-groups
                              '((:name "🔥 High Priority Projects"
                                       :priority "A")
                                (:name "📝 Active Projects"
                                       :priority "B")
                                (:name "📋 Standard Projects"
                                       :priority "C")
                                (:name "🔄 Projects Needing Next Action"
                                       :pred (lambda (item)
                                               (let ((marker (get-text-property 0 'org-marker item)))
                                                 (when (and marker (marker-buffer marker))
                                                   (with-current-buffer (marker-buffer marker)
                                                     (save-excursion
                                                       (goto-char marker)
                                                       (condition-case nil
                                                           (not (org-goto-first-child))
                                                         (error nil))))))))
                                (:auto-category t))))))
           nil)

          ("w" "Weekly Review"
           ((agenda "" ((org-agenda-span 'week)
                        (org-agenda-start-on-weekday 1)
                        (org-agenda-overriding-header "📅 Week at a Glance")
                        (org-super-agenda-groups
                         '((:name "📌 Scheduled"
                                  :scheduled t)
                           (:name "⏰ Deadlines"
                                  :deadline t)
                           (:auto-ts t)))))
            (todo "DONE" ((org-agenda-overriding-header "✅ Completed This Week")
                          (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp "CLOSED:.*\\[.*\\]"))
                          (org-agenda-sorting-strategy '(ts-down))
                          (org-super-agenda-groups
                           '((:auto-category t))))))
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
        '((:name "🚨 Overdue!"
                 :deadline past
                 :face (:background "red" :foreground "white"))
          (:name "⚡ Critical Today"
                 :and (:deadline today :priority "A"))
          (:name "📅 Today's Schedule"
                 :time-grid t
                 :date today)
          (:name "🎯 High Priority Next"
                 :and (:todo "NEXT" :priority "A"))
          (:name "📋 Due Today"
                 :deadline today)
          (:name "⏰ Due Soon (3 days)"
                 :deadline (before ,(org-read-date nil nil "+3d")))
          (:name "🔥 Next Actions"
                 :todo "NEXT")
          (:name "📂 Active Projects"
                 :todo "PROJECT")
          (:name "⏳ Waiting For"
                 :todo "WAIT")
          (:name "📥 Process Inbox"
                 :file-path "inbox.org"
                 :face (:background "yellow" :foreground "black"))
          (:name "💭 Someday/Maybe"
                 :tag "someday")
          (:auto-category t))))

(defhydra hydra-org (:color blue :hint nil)
  "
^Capture^           ^Navigate^          ^Agenda Views^        ^Other^
^^^^^^^^--------------------------------------------------------------------------
_c_: capture        _r_: refile         _a_: default         _l_: store link
_j_: journal        _g_: goto           _G_: GTD dash        _i_: insert link
_t_: todo           _s_: search         _n_: next actions    _o_: open at point
_p_: project        _d_: set deadline   _P_: projects        _S_: schedule
_m_: meeting        _T_: set tags       _w_: weekly review
"
  ("c" org-capture)
  ("j" (org-capture nil "j"))
  ("t" (org-capture nil "t"))
  ("p" (org-capture nil "p"))
  ("m" (org-capture nil "m"))
  ("a" org-agenda)
  ("G" (org-agenda nil "g"))
  ("n" (org-agenda nil "n"))
  ("P" (org-agenda nil "p"))
  ("w" (org-agenda nil "w"))
  ("r" org-refile)
  ("g" org-goto)
  ("s" org-search-view)
  ("d" org-deadline)
  ("S" org-schedule)
  ("T" org-set-tags-command)
  ("l" org-store-link)
  ("i" org-insert-link)
  ("o" org-open-at-point)
  ("q" nil "quit"))

;; org-contacts for email integration
(use-package org-contacts
  :ensure t
  :after org
  :config
  (setq org-contacts-files (list my--org-file-contacts))
  (add-to-list 'org-capture-templates
               '("C" "Contact" entry
                 (file my--org-file-contacts)
                 "* %(org-contacts-template-name)
:PROPERTIES:
:EMAIL: %(org-contacts-template-email)
:PHONE:
:ALIAS:
:NICKNAME:
:IGNORE:
:ICON:
:NOTE:
:ADDRESS:
:BIRTHDAY:
:END:" :empty-lines 1)))
