
;; Default offset in all languages is 4 spaces
;;
(setq c-basic-offset 4)

;; cc-mode (the mode you're in when editing C, C++, and Objective C files)
;;
(setq c-default-style "linux")
(setq-default indent-tabs-mode nil)

;; Tell cc-mode not to check for old-style (K&R) function declarations.
;; This speeds up indenting a lot.
(setq c-recognize-knr-p nil)

;; Change the indentation amount to 4 spaces instead of 2.
;; You have to do it in this complicated way because of the
;; strange way the cc-mode initializes the value of `c-basic-offset'.
(add-hook 'c-mode-hook (lambda () (setq c-basic-offset 4)))
(add-hook 'c++-mode-hook (lambda () (setq c-basic-offset 4)))

(add-hook 'c++-mode-hook (lambda () (highlight-lines-matching-regexp ".\{91\}" "hi-green-b")))

(add-hook 'c-mode-hook (lambda () (linum-mode 1)))
(add-hook 'cc-mode-hook (lambda () (linum-mode 1)))
(add-hook 'c++-mode-hook (lambda () (linum-mode 1)))

(add-hook 'c-mode-hook (lambda () (show-paren-mode 1)))
(add-hook 'cc-mode-hook (lambda () (show-paren-mode 1)))
(add-hook 'c++-mode-hook (lambda () (show-paren-mode 1)))

(add-hook 'c-mode-hook 'projectile-mode)
(add-hook 'cc-mode-hook 'projectile-mode)
(add-hook 'c++-mode-hook 'projectile-mode)

;;; ********************
;;; Sets the Python mode
;;;
(use-package python
  :ensure t
  :defer t
  :mode ("\\.py\\'" . python-mode))

;; ensure:
;;; pip install jedi
;;  pip install flake8
;;  pip install importmagic
;;  pip install autopep8
;;  pip install yapf

(use-package elpy
  :ensure t
  :defer t
  :after python
  :init (advice-add 'python-mode :before 'elpy-enable)
  :hook (linum-mode)
  :config
  (progn 
    (setq
      python-shell-interpreter "ipython3"
      python-shell-interpreter-args "--simple-prompt -i"
      elpy-rpc-backend "jedi"
      elpy-rpc-project-specfic 't)
    (when (fboundp 'flycheck-mode)
      (setq elpy-modules (delete 'elpy-module-flymake elpy-modules)))
    (add-hook 'elpy-mode-hook
      (lambda ()
        (set (make-local-variable 'company-backends)
          (append company-backends '(company-yasnippet)))))
  )
)

(use-package flycheck
  :init
  :after elpy
  :config
  ;;(setq elpy-default-minor-modes (delete 'flymake-mode elpy-default-minor-modes))
  ;;(add-to-list 'elpy-default-minor-modes 'flycheck-mode) 
  ;;(setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(use-package pyvenv
  :ensure t
  :after python)

(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

(add-hook 'lua-mode-hook 'turn-on-font-lock)
;;;If you want to use hideshow, turn on hs-minor-mode or add this:
;(add-hook 'lua-mode-hook 'hs-minor-mode)
(add-hook 'lua-mode-hook (lambda () (linum-mode 1)))

(use-package markdown-mode
  :ensure  markdown-mode
  :defer   t
  :mode    ("\\.\\(markdown\\|mdown\\|md\\)$" . markdown-mode)
  :config  
  (progn
    (add-hook 'markdown-mode-hook
             (lambda ()
               (visual-line-mode t)
               (writegood-mode t)
               (flyspell-mode t)))

    (setq markdown-command "pandoc --smart -f markdown -t html")
  )
)
