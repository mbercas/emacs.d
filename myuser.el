
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")

(require 'mu4e)
(require 'org-mu4e)

(setq send-mail-function 'smtpmail-send-it)

(setq mu4e-maildir "~/Maildir")

;; show full addresses in view message (instead of just names)
;; toggle per name with M-RET
(setq mu4e-view-show-addresses t)

;; Do not show related messages by default (toggle with =W= works
;; anyway)
(setq mu4e-headers-include-related nil)


(setq user-full-name  "Manuel Berrocal"
      mu4e-sent-folder "/eZono/Sent"
      mu4e-drafts-folder "/eZono/Drafts"
      mu4e-trash-folder "/eZono/Junk")


(defvar my-mu4e-account-alist
  '(("eZono"
     (user-full-name  "Manuel Berrocal")
     (mu4e-compose-signature "eZono AG\nDevelopment Manager\n\nmanuel@ezono.com\n")
     (mu4e-compose-signature-auto-include t)
     (mu4e-sent-folder "/eZono/Sent")
     (mu4e-drafts-folder "/eZono/Drafts")
     (mu4e-trash-folder "/eZono/Trash")
     (user-mail-address "manuel@ezono.com")
     (smtpmail-default-smtp-server "mail.ezono.com")
     (smtpmail-local-domain "ezono.com")
     (smtpmail-smtp-user "manuel")
     (smtpmail-smtp-server "mail.ezono.com")
     (smtpmail-stream-type starttls)
     (smtpmail-smtp-service 587)) ;;; 25 / 587
))

;; Whenever a new mail is to be composed, change all relevant
;; configuration variables to the respective account. This method is
;; taken from the MU4E documentation:
;; http://www.djcbsoftware.nl/code/mu/mu4e/Multiple-accounts.html#Multiple-accounts
(defun my-mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var))
                                                my-mu4e-account-alist "/"))
                             (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                             nil t nil nil (caar my-mu4e-account-alist))))
         (account-vars (cdr (assoc account my-mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error "No email account found"))))


(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)
(add-hook 'mu4e-compose-mode-hook (lambda ()
                                   (ispell-change-dictionary "english")))

(setq mu4e-refile-folder
      (lambda (msg)
        (cond
         ((string-match "^/eZono.*"
                        (mu4e-message-field msg :maildir))
          "/eZono/INBOX.Archive")
         ;; everything else goes to /archive
         (t  "/archive"))))


;; Empty the initial bookmark list
(setq mu4e-bookmarks '())

;; Re-define all standard bookmarks to not include the spam folders
;; for searches
(defvar d-spam "NOT (maildir:/eZono/Junk)")

;; All archived folders
(defvar d-archive "NOT (maildir:/eZono/Archive)")

(defvar inbox-folders (string-join '("maildir:/eZono/INBOX")
                                   " OR "))

(defvar draft-folders (string-join '("maildir:/eZono/Drafts")
                                   " OR "))

(defvar spam-folders (string-join '("maildir:/eZono/Junk")
                                  " OR "))

(add-to-list 'mu4e-bookmarks
             '((concat d-spam " AND date:today..now")                  "Today's messages"     ?t))
(add-to-list 'mu4e-bookmarks
             '((concat d-spam " AND date:7d..now")                     "Last 7 days"          ?w))
(add-to-list 'mu4e-bookmarks
             '((concat d-spam " AND flag:flagged")                     "Flagged"              ?f))
(add-to-list 'mu4e-bookmarks
             '((concat d-spam " AND mime:image/*")                     "Messages with images" ?p))
(add-to-list 'mu4e-bookmarks
             '(spam-folders "All spambuckets"     ?S))
(add-to-list 'mu4e-bookmarks
             '(draft-folders "All drafts"     ?d))
(add-to-list 'mu4e-bookmarks
             '(inbox-folders "All inbox mails"     ?i))
(add-to-list 'mu4e-bookmarks
             '((concat d-spam d-archive " AND (flag:unread OR flag:flagged) AND NOT flag:trashed")
               "Unread messages"      ?u))

(defvar my-message-attachment-regexp "\\(
                                      [Ww]e send\\|
                                      [Ii] send\\|
                                      attach\\|
                                      [aA]ngehängt\\|
                                      [aA]nhang\\|
                                      [sS]chicke\\|
                                      angehaengt\\|
                                      haenge\\|
                                      hänge\\)")

(defun my-message-check-attachment nil
  "Check if there is an attachment in the message if I claim it."
  (save-excursion
    (message-goto-body)
    (when (search-forward-regexp my-message-attachment-regexp nil t nil)
      (message-goto-body)
      (unless (or (search-forward "<#part" nil t nil)
                  (message-y-or-n-p
                   "No attachment. Send the message ?" nil nil))
        (error "No message sent")))))
(add-hook 'message-send-hook 'my-message-check-attachment)

(setq mu4e-compose-complete-only-after (format-time-string
                                        "%Y-%m-%d"
                                        (time-subtract (current-time) (days-to-time 150))))

  (setq mu4e-html2text-command 'mu4e-shr2text)
;;(setq mu4e-html2text-command "iconv -c -t utf-8 | pandoc -f html -t plain")
  (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)

(setq mu4e-view-html-plaintext-ratio-heuristic  most-positive-fixnum)
(setq mu4e-compose-format-flowed t)
(add-hook 'mu4e-compose-mode-hook 'visual-clean)
(add-hook 'mu4e-compose-mode-hook 'flyspell-mode)

(setq
  mu4e-get-mail-command "offlineimap"  ;; or fetchmail, or ...
  mu4e-update-interval 300             ;; update every 5 minutes
  mu4e-index-update-in-background t)   ;; update in the background

(use-package mu4e-alert
  :ensure t
  :after mu4e
  :init
  (setq mu4e-alert-interesting-mail-query
    (concat
     "flag:unread maildir:/eZono/INBOX "
     ))
  (mu4e-alert-enable-mode-line-display)
  (defun gjstein-refresh-mu4e-alert-mode-line ()
    (interactive)
    (mu4e~proc-kill)
    (mu4e-alert-enable-mode-line-display)
    )
  (run-with-timer 0 60 'gjstein-refresh-mu4e-alert-mode-line)
  )

(require 'org-mu4e)
(setq org-mu4e-link-query-in-headers-mode nil)
