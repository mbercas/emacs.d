;; update package-archive list
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("gnu" . "http://elpa.gnu.org/packages/") t)
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))



;; Enable use-package
(eval-when-compile
  (require 'use-package))

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
 '(package-selected-packages
   (quote
    (spaceline-all-the-icons ivy-prescient prescient wgrep counsel-projectile flx which-key use-package try shell-toggle org-bullets mu4e-alert mode-icons markdown-mode magit elpy auto-complete ace-window)))
 '(send-mail-function (quote sendmail-send-it))
 '(spice-output-local "Gnucap")
 '(spice-simulator "Gnucap")
 '(spice-waveform-viewer "Gwave")
 '(treemacs-git-integration t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0))))
 '(swiper-line-face ((t (:foreground "#ffffff" :background "#60648E")))))
