
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

(use-package ace-window
:ensure t
:init
(progn
  (global-set-key [remap other-window] 'ace-window)
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 3.0))))) 
  ))

(use-package counsel
:ensure t
)

(use-package ivy
:ensure t
:diminish (ivy-mode)
:bind (("C-x b" . ivy-switch-buffer))
:config
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-display-style 'fancy))
(define-key ivy-minibuffer-map (kbd "C-w") 'ivy-yank-word)

(use-package swiper
:ensure try
:bind (("C-s" . swiper)
       ("C-r" . swiper)
       ("C-c C-r" . ivy-resume)
       ("M-x" . counsel-M-x)
       ("C-x C-f" . counsel-find-file))
:config
(progn
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-display-style 'fancy)
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
  ))

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
  :bind ("C-x g" . magit-status))

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
  :demand t)

(use-package spaceline
  :demand t
  :init
  (setq powerline-default-separator 'arrow-fade)
  :config
  (require 'spaceline-config)
  (spaceline-emacs-theme))
