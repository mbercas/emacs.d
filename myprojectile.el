
;;(projectile-global-mode)

;; helm autocompletion mode and integration with projectile
;;(use-package helm-config
;;  :ensure t
;;  :defer t)

;;;(require 'helm-config)
;;;(require 'helm-projectile)

(use-package helm-projectile
  :ensure t
  :defer t
  :config
  (progn
     (setq projectile-completion-system 'helm)
     (helm-projectile-on)
     (setq projectile-switch-project-action 'helm-projectile)
     (setq projectile-enable-idle-timer t)
     (setq projectile-globally-unignored-files (quote ("*.o" "*.pyc" "*~")))
     (setq projectile-tags-backend (quote find-tag))
     (setq projectile-enable-caching t))

(use-package treemacs
  :ensure t
  :defer t
  :config
  (progn
    ;(use-package treemacs-evil
    ;  :ensure t
    ;  :demand t)
    (setq treemacs-follow-after-init          t
          treemacs-width                      35
          treemacs-indentation                2
          treemacs-git-integration            t
          treemacs-collapse-dirs              3
          treemacs-silent-refresh             nil
          treemacs-change-root-without-asking nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-show-hidden-files          t
          treemacs-never-persist              nil
          treemacs-is-never-other-window      nil
          treemacs-goto-tag-strategy          'refetch-index)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t))
  :bind
  (:map global-map
        ([f8]        . treemacs-toggle)
        ("M-0"       . treemacs-select-window)
        ("C-c 1"     . treemacs-delete-other-windows)
        ("M-n ft"    . treemacs-toggle)
        ("M-n fT"    . treemacs)
        ("M-n f C-t" . treemacs-find-file)))
(use-package treemacs-projectile
  :defer t
  :ensure t
  :config
  (setq treemacs-header-function #'treemacs-projectile-create-header)
  :bind (:map global-map
              ("M-n fP" . treemacs-projectile)
              ("M-n fp" . treemacs-projectile-toggle)))
