;; update package-archive list
(require 'package)
(setq package-enable-at-startup nil)
;;(add-to-list 'package-archives
;;             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("gnu" . "http://elpa.gnu.org/packages/") t)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
  ;; and `package-pinned-packages`. Most users will not need or want to do this.
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (setq package-archive-priorities
  '(("melpa-stable" . 10)
    ("melpa"        .  0)))
  )


(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))



;; Enable use-package
(eval-when-compile
  (require 'use-package))


(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
    (auto-package-update-maybe))

;(use-package diminish
;  :ensure t
;  )

;;; init-use-package.el ends here


(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/themes/")
(let ((default-directory "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))

                                        ; Configuration files
(require 'org)

(org-babel-load-file (expand-file-name "~/.emacs.d/myinit.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/mylang.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/myprojectile.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/myuser.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/mygtg.org"))


;; integrate highlight-parentheses with autopair mode:
(add-hook 'highlight-parentheses-mode-hook
          '(lambda ()
             (setq autopair-handle-action-fns
                   (append
                    (if autopair-handle-action-fns
                        autopair-handle-action-fns
                      '(autopair-default-handle-action))
                    '((lambda (action pair pos-before)
                        (hl-paren-color-update)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(deft-default-extension "org" t)
 '(deft-directory "~/org-roam" t)
 '(deft-recursive t t)
 '(deft-use-filter-string-for-filename t t)
 '(inhibit-startup-screen t)
 '(markdown-command "pandoc -f markdown+smart -t html")
 '(org-agenda-files
   (quote
    ("~/org/mbo.org" "~/org/projects.org" "~/org/gtd.org" "~/org/journal.org")))
 '(org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . "browse-url %s")
     ("\\.pdf\\'" . xdg-open))))
 '(org-roam-directory "~/org-roam")
 '(package-selected-packages
   (quote
    (pandoc-mode ivy-rich ivy-posframe git-commit treemacs-magit ein htmlize lsp-rust flycheck-rust company-lsp company-backends cargo rust-mode ess-smart-equals ess-smart-underscore ess org-pdfview pdf-tools spaceline-all-the-icons ivy-prescient prescient wgrep counsel-projectile flx which-key use-package try shell-toggle org-bullets mu4e-alert mode-icons markdown-mode elpy auto-complete ace-window)))
 '(send-mail-function (quote sendmail-send-it))
 '(spice-output-local "Gnucap")
 '(spice-simulator "Gnucap")
 '(spice-waveform-viewer "Gwave")
 '(treemacs-git-integration t t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0))))
 '(swiper-line-face ((t (:foreground "#ffffff" :background "#60648E")))))
