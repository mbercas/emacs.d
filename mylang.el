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

;;; ********************
;;; Sets the Python mode
;;;


;(autoload 'python-mode "python-mode" "Python editing mode." t)
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(setq py-electric-colon-active t)
(setq interpreter-mode-alist (cons '("python" . python-mode) interpreter-mode-alist))
;;;(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(defun run-python-once ()
  (remove-hook 'python-mode-hook 'run-python-once)
  (run-python))

(add-hook 'python-mode-hook 'run-python-once)

;(require 'pymacs)
;(pymacs-load "ropemacs" "rope-")
;(setq ropemacs-enable-autoimport t)

(add-hook 'python-mode-hook (lambda () (linum-mode 1)))

(require 'jedi)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories' "/usr/share/emacs/site-lisp/auto-complete/ac-dict")
(ac-config-default)
(setq ac-auto-start 2
      ac-override-local-map nil
      ac-use-menu-map t
      ac-candidate-limit 20)

(add-hook 'python-mode-hook 'jedi:setup)
(autoload 'jedi:setup "jedi" nil t)
(setq jedi:complete-on-dot t)

(add-hook 'python-mode-hook 'company-mode)
;(add-hook 'python-mode-hook 'anaconda-mode)
;(add-hook 'python-mode-hook 'eldoc-mode)

;; enable server for integration with ipython
;;
(defvar server-buffer-clients)
(when (and (fboundp 'server-start) (string-equal (getenv "TERM") 'xterm))
  (server-start)
  (defun fp-kill-server-with-buffer-routine ()
    (and server-buffer-clients (server-done)))
  (add-hook 'kill-buffer-hook 'fp-kill-server-with-buffer-routine))

(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

(add-hook 'lua-mode-hook 'turn-on-font-lock)
;;;If you want to use hideshow, turn on hs-minor-mode or add this:
;(add-hook 'lua-mode-hook 'hs-minor-mode)
(add-hook 'lua-mode-hook (lambda () (linum-mode 1)))

(require 'taskjuggler-mode)
(add-hook 'taskjuggler-mode-hook (lambda () (linum-mode 1)
                                   (setq tab-width 4)))

(autoload 'markdown-mode "markdown-mode" 
          "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown$" . markdown-mode))
(add-hook 'markdown-mode-hook
          (lambda ()
            (visual-line-mode t)
            (writegood-mode t)
            (flyspell-mode t)))
(setq markdown-command "pandoc --smart -f markdown -t html")

(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code."
  t)
(add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)
(add-hook 'lisp-mode-hook             'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'scheme-mode-hook           'enable-paredit-mode)
;; slime


(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)

(add-to-list 'load-path "~/.emacs/elpa/slime-2.17/")
(setq inferior-lisp-program "sbcl")
(require 'slime)
(slime-setup)
;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key)
    nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)


(add-hook 'lisp-mode-hook (lambda () (linum-mode 1)))
