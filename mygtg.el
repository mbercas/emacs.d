(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
   (setq org-default-notes-file (concat org-directory "C:/Users/manuel/Dropbox/Apps/org/notes.org"))
   (setq org-capture-templates
      '(("t" "Todo" entry (file+headline "C:/Users/manuel/Dropbox/Apps/org/gtd.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("n" "Note" entry (file "C:/Users/manuel/Dropbox/Apps/org/notes.org")
         "* NOTE %?\n  %i\n %a")
        ("p" "Proj" entry (file "C:/Users/manuel/Dropbox/Apps/org/projects.org")
         "** TODO %?\n  %i\n %a")
        ("j" "Journal" entry (file+olp+datetree "C:/Users/manuel/Dropbox/Apps/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a"))))
 ((string-equal system-type "gnu/linux") ; linux
    (setq org-default-notes-file (concat org-directory "~/org/notes.org"))

    (setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("n" "Note" entry (file "~/org/notes.org")
         "* NOTE %?\n  %i\n %a")
        ("p" "Proj" entry (file "~/org/projects.org")
         "** TODO %?\n  %i\n %a")
        ("j" "Journal" entry (file+olp+datetree "~/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")))))

; Refile on top of file max
(setq org-refile-use-outline-path 'file)
; use a depth level of 6 max
(setq org-refile-targets
      '((org-agenda-files . (:maxlevel . 2))))
(setq org-outline-path-complete-in-steps nil)

(setq org-log-done 'time)

(setq org-agenda-overriding-columns-format "%58ITEM(Details) %PRIORITY(P) %TAGS(Context) %7TODO(To Do) %5Effort(Time){:} %6CLOCKSUM{Total}")

(setq org-tags-match-list-sublevels 'indented)

;; == bh/helper-functions ==
(defun bh/is-project-p ()
  "Any task with a todo keyword subtask."
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))
(defun bh/find-project-task ()
  "Move point to the parent (project) task if any."
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))
(defun bh/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (bh/find-project-task)
      (if (equal (point) task)
          nil
        t))))



;; == Custom State Keywords ==
(setq org-use-fast-todo-selection t)

(setq org-todo-keywords
      '((sequence "TODO(t)" "PROJ(p)" "STARTED(s)" "NEXT(n)" "DELEGATED(g)" "MEETING(m)" "NOTE(o)" "|" "DONE(d)")
	(sequence "WAITING(w@/!)" "INACTIVE(i)" "|" "CANCELLED(c@/!)" )))

;; Custom colors for the keywords
(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
	("PROJ" :foreground "blue" :weight bold)
	("NEXT" :foreground "blue" :weight bold)
	("DONE" :foreground "forest green" :weight bold)
	("WAITING" :foreground "orange" :weight bold)
	("INACTIVE" :foreground "magenta" :weight bold)
	("CANCELLED" :foreground "forest green" :weight bold)
	("MEETING" :foreground "forest green" :weight bold)
        ("NOTE" :foreground "dark violet" :weight bold)
        ("DELEGATED" :foreground "dark orange" :weight bold)
        ("STARTED" :foreground "dark orange" :weight bold)))

;; Auto-update tags whenever the state is changed
(setq org-todo-state-tags-triggers
      '(("CANCELLED" ("CANCELLED" . t))
	("WAITING" ("WAITING" . t))
	("INACTIVE" ("WAITING") ("INACTIVE" . t))
	(done ("WAITING") ("INACTIVE"))
	("TODO" ("WAITING") ("CANCELLED") ("INACTIVE"))
	("NEXT" ("WAITING") ("CANCELLED") ("INACTIVE"))
	("DONE" ("WAITING") ("CANCELLED") ("INACTIVE"))))

(defun gs/mark-next-done-parent-tasks-todo ()
  "Visit each parent task and change NEXT (or DONE) states to TODO."
  ;; Don't change the value if new state is "DONE"
  (let ((mystate (or (and (fboundp 'org-state)
                          (member state
				  (list "NEXT" "TODO")))
                     (member (nth 2 (org-heading-components))
			     (list "NEXT" "TODO")))))
    (when mystate
      (save-excursion
        (while (org-up-heading-safe)
          (when (member (nth 2 (org-heading-components)) (list "NEXT" "DONE"))
            (org-todo "TODO")))))))
(add-hook 'org-after-todo-state-change-hook 'gs/mark-next-done-parent-tasks-todo 'append)


(defvar org-my-archive-expiry-days 4.0
  "The number of days after which a completed task should be auto-archived.
   This can be 0 for immediate, or a floating point value.")

(defun org-my-archive-done-tasks ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((done-regexp
           (concat "\\* \\(" (regexp-opt org-done-keywords) "\\) "))
          (state-regexp
           (concat "- State \"\\(" (regexp-opt org-done-keywords)
                   "\\)\"\\s-*\\[\\([^]\n]+\\)\\]")))
      (while (re-search-forward done-regexp nil t)
        (let ((end (save-excursion
                     (outline-next-heading)
                     (point)))
              begin)
          (goto-char (line-beginning-position))
          (setq begin (point))
          (if (re-search-forward state-regexp end t)
              (let* ((time-string (match-string 2))
                     (when-closed (org-parse-time-string time-string)))
                (if (>= (time-to-number-of-days
                         (time-subtract (current-time)
                                        (apply #'encode-time when-closed)))
                        org-my-archive-expiry-days)
                    (org-archive-subtree)))
            (goto-char end)))))
    (save-buffer)))

(setq safe-local-variable-values (quote ((after-save-hook archive-done-tasks))))

(defalias 'archive-done-tasks 'org-my-archive-done-tasks)

(defun org-archive-done-tasks-file ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE" 'file))


(defun org-archive-done-tasks-agenda ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE" 'agenda))

(autoload 'org-present "org-present" nil t)

(add-hook 'org-present-mode-hook
          (lambda ()
            (org-present-big)
            (org-display-inline-images)))

(add-hook 'org-present-mode-quit-hook
          (lambda ()
            (org-present-small)
            (org-remove-inline-images)))

(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key org-mode-map "\C-cn" 'org-mactions-new-numbered-action)
(define-key global-map  "\C-cg" (lambda() 
                                    (interactive)
                                    (find-file "~/org/gtd.org")))
(define-key global-map  "\C-c j" (lambda()
                                    (interactive)
                                    (find-file "~/org/journal.org")))
