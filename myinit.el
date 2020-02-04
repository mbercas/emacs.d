
(setq inhibit-startup-message t)
;; Enable line numbers and column numbers.
;; 
(line-number-mode 1)
(column-number-mode 1)
(setq fill-column 90) ;; M-q should fill at 90 chars, not 75
(tool-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(set-variable 'confirm-kill-emacs 'yes-or-no-p)
(global-set-key (kbd "<f5>") 'revert-buffer)

(use-package recentf
  :ensure t
  :config
    (progn
       (recentf-mode 1)
       (setq recentf-max-menu-items 25)
       (global-set-key "\C-x\ \C-r" 'recentf-open-files)))

(use-package ido
  :ensure t
  :config
  (setq ido-enable-flex-matching t)
  (ido-everywhere t)
  (ido-mode 1))

(use-package try
        :ensure t)

(use-package which-key
      :ensure t 
      :config
      (which-key-mode))

(use-package org-bullets
  :ensure t
  :config
  (progn
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
    ;;(add-hook 'org-mode-hook (lambda () (flyspell-mode t)))
    (add-hook 'org-mode-hook (lambda () (linum-mode 1)))
    (add-hook 'org-mode-hook (lambda () (show-paren-mode 1)))

    ;; configure the calendar
    (setq calendar-week-start-day 1)
    (setq calendar-intermonth-text
       '(propertize
        (format "%2d"
               (car
               (calendar-iso-from-absolute
               (calendar-absolute-from-gregorian (list month day year)))))
      'font-lock-face 'font-lock-warning-face))

    (setq calendar-intermonth-header
      (propertize "Wk"                  ; or e.g. "KW" in Germany
                'font-lock-face 'font-lock-keyword-face))

    ;; 
    (use-package flyspell
       :ensure t
       :config
         (progn
           (flyspell-mode 1)
           (add-hook 'org-mode-hook (lambda () (flyspell-mode t)))
         )
    )

  )
)

(org-babel-do-load-languages
  'org-babel-load-languages
  '((python .t)))

(use-package shell-toggle
  :ensure t
  :bind (("M-<f1>" . shell-toggle)
         ("C-<f1>" . shell-toggle-cd))
         )

(use-package ace-window
:ensure t
:init
(progn
  (global-set-key [remap other-window] 'ace-window)
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 3.0))))) 
  ))

(use-package flx
  :ensure)

(use-package counsel
  :diminish
  :hook (ivy-mode . counsel-mode)
  :config
  (global-set-key (kbd "s-P") #'counsel-M-x)
  (global-set-key (kbd "s-f") #'counsel-grep-or-swiper)
  (setq counsel-rg-base-command "rg --vimgrep %s"))

(use-package counsel-projectile
  :ensure
  :config (counsel-projectile-mode +1))

(use-package ivy
  :diminish
  :hook (after-init . ivy-mode)
  :config
  (setq ivy-display-style nil)
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "<escape>") #'minibuffer-keyboard-quit)
  (setq ivy-re-builders-alist
        '((counsel-rg . ivy--regex-plus)
          (counsel-projectile-rg . ivy--regex-plus)
          (counsel-ag . ivy--regex-plus)
          (counsel-projectile-ag . ivy--regex-plus)
          (swiper . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        ivy-initial-inputs-alist nil))

(use-package swiper
  :after ivy
  :custom-face (swiper-line-face ((t (:foreground "#ffffff" :background "#60648E"))))
  :config
  (setq swiper-action-recenter t)
  (setq swiper-goto-start-of-match t))

;(use-package ivy-posframe
;  :after ivy
;  :diminish
;  :config
;  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center))
;        ivy-posframe-height-alist '((t . 20))
;        ivy-posframe-parameters '((internal-border-width . 10)))
;  (setq ivy-posframe-width 70)
;  (ivy-posframe-mode +1))

;(use-package ivy-rich
;  :preface
;  :ensure
;  (defun ivy-rich-switch-buffer-icon (candidate)
;    (with-current-buffer
;        (get-buffer candidate)
;      (all-the-icons-icon-for-mode major-mode)))
;  :init
;  (setq ivy-rich-display-transformers-list ; max column width sum = (ivy-poframe-width - 1)
;        '(ivy-switch-buffer
;          (:columns
;           ((ivy-rich-switch-buffer-icon (:width 2))
;            (ivy-rich-candidate (:width 35))
;            (ivy-rich-switch-buffer-project (:width 15 :face success))
;            (ivy-rich-switch-buffer-major-mode (:width 13 :face warning)))
;           :predicate
;           #'(lambda (cand) (get-buffer cand)))
;          counsel-M-x
;          (:columns
;           ((counsel-M-x-transformer (:width 35))
;            (ivy-rich-counsel-function-docstring (:width 34 :face font-lock-doc-face))))
;          counsel-describe-function
;          (:columns
;           ((counsel-describe-function-transformer (:width 35))
;            (ivy-rich-counsel-function-docstring (:width 34 :face font-lock-doc-face))))
;          counsel-describe-variable
;          (:columns
;           ((counsel-describe-variable-transformer (:width 35))
;            (ivy-rich-counsel-variable-docstring (:width 34 :face font-lock-doc-face))))
;          package-install
;          (:columns
;           ((ivy-rich-candidate (:width 25))
;            (ivy-rich-package-version (:width 12 :face font-lock-comment-face))
;            (ivy-rich-package-archive-summary (:width 7 :face font-lock-builtin-face))
;            (ivy-rich-package-install-summary (:width 23 :face font-lock-doc-face))))))
;  :config
;  (ivy-rich-mode +1)
;  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package projectile
  :ensure
  :diminish
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") #'projectile-command-map)
  (define-key projectile-mode-map (kbd "s-p") #'projectile-find-file) ; counsel
  (define-key projectile-mode-map (kbd "s-F") #'projectile-ripgrep) ; counsel
  (setq projectile-sort-order 'recentf
        projectile-indexing-method 'hybrid
        projectile-completion-system 'ivy))

(use-package wgrep
  :ensure
  :config
  (setq wgrep-enable-key (kbd "C-c C-w")) ; change to wgrep mode
  (setq wgrep-auto-save-buffer t))

(use-package prescient
  :ensure
  :config
  (setq prescient-filter-method '(literal regexp initialism fuzzy))
  (prescient-persist-mode +1))

(use-package ivy-prescient
  :ensure
  :after (prescient ivy)
  :config
  (setq ivy-prescient-sort-commands
        '(:not swiper counsel-grep ivy-switch-buffer))
  (setq ivy-prescient-retain-classic-highlighting t)
  (ivy-prescient-mode +1))

(use-package company-prescient
  :after (prescient company)
  :config (company-prescient-mode +1))

(use-package avy
:ensure t
:bind ("M-s" . avy-goto-word-1)) ;; changed from char as per jcs

(use-package auto-complete
:ensure t
:init
(progn
  (ac-config-default)
  (global-auto-complete-mode t)
  ))

(use-package magit
  :ensure t
  :commands magit-status
  :bind (("C-x g" . magit-status))
  )

;;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
  
;;  (use-package color-theme
;;  :ensure t)

;;  (use-package zenburn-theme
;;  :ensure t
;;  :config (load-theme 'zenburn t))

(use-package mode-icons
  :ensure t
  :init (mode-icons-mode))

;; spacemacs look
(use-package spacemacs-theme
  :defer t
  :init
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
  (load-theme 'spacemacs-dark t)
  (setq spacemacs-theme-org-agenda-height nil)
  (setq spacemacs-theme-org-height nil)
  :config
  ;; set sizes here to stop spacemacs theme resizing these
    (set-face-attribute 'org-level-1 nil :height 1.0)
    (set-face-attribute 'org-level-2 nil :height 1.0)
    (set-face-attribute 'org-level-3 nil :height 1.0)
    (set-face-attribute 'org-scheduled-today nil :height 1.0)
    (set-face-attribute 'org-agenda-date-today nil :height 1.1)
    (set-face-attribute 'org-table nil :foreground "#008787"))

(use-package spaceline-all-the-icons
  :ensure
  :demand t)

(use-package spaceline
  :ensure
  :demand t
  :init
  (setq powerline-default-separator 'arrow-fade)
  :config
  (require 'spaceline-config)
  (spaceline-emacs-theme))
