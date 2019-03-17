
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
  :after python
  :init  (elpy-enable)
  :config
  (progn 
    (setq
      python-shell-interpreter "ipython"
      python-shell-interpreter-args "--simple-prompt -i")
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

(require 'taskjuggler-mode)
(add-hook 'taskjuggler-mode-hook (lambda () (linum-mode 1)))
(add-hook 'taskjuggler-mode-hook (lambda () (setq tab-width 4)))
(add-hook 'taskjuggler-mode-hook (lambda () (show-parent-mode 1)))
(add-hook 'taskjuggler-mode-hook (lambda () (highlight-blocks-mode 1)))

(use-package markdown-mode
  :ensure  markdown-mode
  :defer   t
  :mode    ("\\.\\(markdown\\|mdown\\|md\\)$" . markdown-mode)
  :config  (add-hook 'markdown-mode-hook
             (lambda ()
               (visual-line-mode t)
               (writegood-mode t)
               (flyspell-mode t))))

(setq markdown-command "pandoc --smart -f markdown -t html")

(use-package slime
  :ensure t
  :after lisp
  :config
  (progn
    (add-hook
     'slime-load-hook
     #'(lambda ()
         (slime-setup 
          '(slime-fancy
            slime-repl
            slime-fuzzy))))
    (setq slime-net-coding-system 'utf-8-unix)
    (add-hook 'lisp-mode-hook (lambda () (linum-mode 1)))

    ;; Slime and Auto-Complete
    (use-package ac-slime
      :ensure t
      :init
      (progn
        (add-hook 'slime-mode-hook 'set-up-slime-ac)
        (add-hook 'slime-repl-mode-hook 'set-up-slime-ac))
      :config
      (progn
        (eval-after-load "auto-complete"
          '(add-to-list 'ac-modes 'slime-repl-mode))))))



(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code."
  t)
(add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)
(add-hook 'lisp-mode-hook             'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'scheme-mode-hook           'enable-paredit-mode)
;; slime



;;(load (expand-file-name "~/quicklisp/slime-helper.el"))

(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)


(setq inferior-lisp-program "sbcl")

;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
;;(defun override-slime-repl-bindings-with-paredit ()
;;  (define-key slime-repl-mode-map
;;    (read-kbd-macro paredit-backward-delete-key)
;;    nil))
;;(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)


;;(add-hook 'lisp-mode-hook (lambda () (linum-mode 1)))
