
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;;(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/themes/")
(let ((default-directory "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))

                                        ; Configuration files

(org-babel-load-file (expand-file-name "~/.emacs.d/myinit.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/mygtg.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/mylang.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/myprojectile.org"))


(require 'mode-icons)
(mode-icons-mode)

;; Enable line numbers and column numbers.
;; 
(line-number-mode 1)
(column-number-mode 1)
(setq fill-column 90) ;; M-q should fill at 90 chars, not 75


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


(require 'ido)
(ido-mode t)






(load (expand-file-name "~/quicklisp/slime-helper.el"))



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
 '(org-agenda-files
   (quote
    ("/sandbox/workspace/repos/education/planning/education_minutes.org" "/sandbox/workspace/repos/partnerprojects/telemed/planning/minutes/telemed_minutes.org" "/sandbox/workspace/repos/partnerprojects/samsung/planning/minutes/samsung-medison_minutes.org" "/sandbox/workspace/repos/partnerprojects/hansono/planning/minutes/hansono_minutes.org" "/sandbox/workspace/repos/partnerprojects/sonoscape/planning/minutes/sonoscape_minutes.org" "/sandbox/workspace/repos/partnerprojects/ecm/planning/minutes/ecm_minutes.org" "/sandbox/workspace/repos/bard_siterite_ngs/trunk/planning/minutes/bard_siterite_ngs_minutes.org" "~/org/gtd.org" "/sandbox/workspace/repos/partnerprojects/ezono/planning/minutes/partner_status_meeting.org" "/sandbox/workspace/repos/partnerprojects/wisonic/planning/minutes/wisonic_minutes.org" "/sandbox/workspace/repos/partnerprojects/vinno/planning/minutes/vinno_minutes.org" "/sandbox/workspace/repos/partnerprojects/fcu/planning/minutes/fcu_minutes.org" "/sandbox/workspace/sensor_boards/faulty_probes_plan.org" "~/org/journal.org")))
 '(org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . "browse-url %s")
     ("\\.pdf\\'" . xdg-open))))
 '(package-selected-packages
   (quote
    (treemacs treemacs-projectile counsel-gtags gtags marmalade anything-exuberant-ctags helm-gtags mode-icons helm-projectile spacemacs-theme org-ac org-agenda-property org-autolist org-beautify-theme org-clock-csv org-gcal org-gnome org-journal org-pdfview org-projectile org-table-comment org-table-sticky-header org-transform-tree-table org-vcard aggressive-indent projectile-variable projectile-git-autofetch projectile python-x qml-mode pyvenv csv-mode puml-mode graphviz-dot-mode zenburn-theme writegood-mode which-key vhdl-tools use-package try slime-company pycomplete pungi paredit org-tree-slide org-bullets magit ical-pull highlight-blocks git flycheck fill-column-indicator elpy elisp-slime-nav eink-theme ein ecb doctags desktop-registry counsel company-jedi color-theme autopair anaconda-mode ace-window ac-slime ac-python)))
 '(spice-output-local "Gnucap")
 '(spice-simulator "Gnucap")
 '(spice-waveform-viewer "Gwave")
 '(treemacs-git-integration t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))
