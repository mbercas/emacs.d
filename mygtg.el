(setq org-default-notes-file (concat org-directory "~/org/notes.org"))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("n" "Note" entry (file+headline "~/org/notes.org")
         "* NOTE %?\n  %i\n %a")
        ("p" "Proj" entry (file+headline "~/org/projects.org")
         "* PROJ %?\n  %i\n %a")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")))

(setq org-mobile-directory "/sandbox/Dropbox/MobileOrg")

(setq org-log-done 'time)

(defcustom org-mactions-numbered-action-format "TODO Action #%d "
  "Default structure of the headling of a new action.
    %d will become the number of the action."
  :group 'org-edit-structure
  :type 'string)

(defcustom org-mactions-change-id-on-copy t
  "Non-nil means make new IDs in copied actions.
If an action copied with the command `org-mactions-collect-todos-in-subtree'
contains an ID, that ID will be replaced with a new one."
  :group 'org-edit-structure
  :type 'string)


(defun org-mactions-new-numbered-action (&optional inline)
  "Insert a new numbered action, using `org-mactions-numbered-action-format'.
    With prefix argument, insert an inline task."
  (interactive "P")
  (let* ((num (let ((re "\\`#\\([0-9]+\\)\\'"))
                (1+ (apply 'max 0
                           (mapcar
                            (lambda (e)
                              (if (string-match re (car e))
                                  (string-to-number (match-string 1 (car e)))
                                0))
                            (org-get-buffer-tags))))))
         (tag (concat "#" (number-to-string num))))
    (if inline
        (org-inlinetask-insert-task)
      (org-insert-heading 'force))
    (unless (eql (char-before) ?\ ) (insert " "))
    (insert (format org-mactions-numbered-action-format num))
    (org-toggle-tag tag 'on)
    (if (= (point-max) (point-at-bol))
        (save-excursion (goto-char (point-at-eol)) (insert "\n")))
    (unless (eql (char-before) ?\ ) (insert " "))))

(defun org-mactions-collect-todos-in-subtree ()
  "Collect all TODO items in the current subtree into a flat list."
  (interactive)
  (let ((buf (get-buffer-create "Org TODO Collect"))
        (cnt 0) beg end string s)
    (with-current-buffer buf (erase-buffer) (org-mode))
    (org-map-entries
     (lambda ()
       (setq beg (point) end (org-end-of-subtree t t) cnt (1+ cnt)
             string (buffer-substring beg end)
             s 0)
       (when org-mactions-change-id-on-copy
         (while (string-match "^\\([ \t]*:ID:\\)[ \t\n]+\\([^ \t\n]+\\)[ \t]*$"
                              string s)
           (setq s (match-end 1)
                 string (replace-match (concat "\\1 "
                                               (save-match-data (org-id-new)))
                                       t nil string))))
       (with-current-buffer buf (org-paste-subtree 1 string)
                            (goto-char (point-max))))
     (format "TODO={%s}" (regexp-opt org-not-done-keywords))
     'tree)
    (if (= cnt 0)
        (message "No TODO items in subtree")
      (message "%d TODO entries copied to kill ring" cnt)
      (prog1 (with-current-buffer buf
               (kill-new (buffer-string)))
        (kill-buffer buf)))))

(setq org-agenda-overriding-columns-format "%58ITEM(Details) %PRIORITY(P) %TAGS(Context) %7TODO(To Do) %5Effort(Time){:} %6CLOCKSUM{Total}")

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "medium blue" :weight bold)
              ("APPT" :foreground "medium blue" :weight bold)
              ("NOTE" :foreground "dark violet" :weight bold)
              ("STARTED" :foreground "dark orange" :weight bold)
              ("WAITING" :foreground "red" :weight bold)
              ("DELEGATED" :foreground "dark orange" :weight bold))))

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
