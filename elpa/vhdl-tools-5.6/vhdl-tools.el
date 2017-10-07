;;; vhdl-tools.el --- Utilities for navigating vhdl sources. -*- lexical-binding: t; -*-

;; Based on `vhdl-goto-def' at `http://www.emacswiki.org/emacs/vhdl-goto-def.el'

;; Copyright (C) 2003 Free Software Foundation, Inc.
;; Copyright (C) 2015-2017 Cayetano Santos

;; Original author:  wandad guscheh <wandad.guscheh@fh-hagenberg.at>
;; Author:           Cayetano Santos
;; Keywords: vhdl
;; Package-Version: 5.6

;; Filename: vhdl-tools.el
;; Description: Utilities for navigating vhdl sources.
;; URL: https://csantosb.github.io/vhdl-tools/
;; Keywords: convenience
;; Compatibility: GNU Emacs >= 25.2
;; Version: 5.6
;; Package-Requires: ((ggtags "0.8.12") (emacs "25.2") (outshine "2.0") (helm "2.8.0"))

;;; License:
;;
;; This file is NOT part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Vhdl-tools provide a minor mode to be used under `vhdl-mode' major mode.
;; It adds a layer of functionality on top of `ggtags' and relies on and extends
;; `imenu' and `outshine' features to ease navigating vhdl sources.  It expects
;; a working setup of `ggtags'.
;;
;; For details, refer to  https://csantosb.github.io/vhdl-tools/

;;; Install:
;;
;; To install, proceed as usual: add to path and require after loading `vhdl-mode'
;;
;; (with-eval-after-load 'vhdl-mode
;;   (add-to-list 'load-path "...")
;;   (require 'vhdl-tools))
;;
;; or install from Melpa
;;
;;   M-x package-install RET vhdl-tools
;;
;; Then, activate the minor mode by completing the `vhdl-mode' hook.
;;
;; (add-hook 'vhdl-mode-hook
;; 	  (lambda ()
;; 	    (vhdl-tools-mode 1)))

;;; Use:
;;
;; The minor mode provide utilities to ease navigating vhdl sources beyond what
;; is available with plain `ggtags'.
;;
;;   - Jumping into instances
;;   - Jump to upper level
;;   - Searching for references
;;   - Link management
;;   - Custom use of imenu
;;
;; Open any vhdl file and invoke the following keybinds
;;
;;   C-c M-D     jumps to the definition of symbol at point
;;   C-c M-j     follows the link at point
;;   C-c M-w     stores a link
;;   C-c M-y     pastes a link
;;   C-c M-.     jumps into the instance at point
;;   C-c M-a     moves point to first appearance of symbol at point
;;   C-c M-u     jumps to upper hierarchy level
;;
;; Cursor will jump to the target if there is one, searching packages
;; too. The ring mark is push after jumping, so to get back, press \C-c\C-p
;; or \M-, (default binds under `ggtags') if corresponding definition has been
;; found. Works better for files with correct syntax: think
;; `vhdl-beautify-buffer' before using `vhdl-tools'.
;;
;; Also have a look at customization possibilities with \M-x customize-group `vhdl-tools'.
;;
;; See README for details

;;; Todo:

;;; Code:

(require 'vhdl-mode)
(require 'ggtags)
(require 'imenu)
(require 'outshine)
(require 'helm-grep)

;;; Groups

(defgroup vhdl-tools nil "Some customizations of vhdl-tools package"
  :group 'local)

(defgroup vhdl-tools-vorg nil "Some customizations of vhdl-tools vorg package"
  :group 'local)

;;; Variables

;;;; User Variables

;;;;; vOrg

(defcustom vhdl-tools-vorg-tangle-comment-format-beg "@@@"
  "Variable to assign to `org-babel-tangle-comment-format-beg' during `vorg' tangling."
  :type 'string :group 'vhdl-tools-vorg)

(defcustom vhdl-tools-vorg-tangle-comment-format-end "@@@"
  "Variable to assign to `org-babel-tangle-comment-format-end' during `vorg' tangling."
  :type 'string :group 'vhdl-tools-vorg)

(defcustom vhdl-tools-vorg-tangle-comments-link nil
  "Flag to force set the comments:link header in vhdl src blocks."
  :type 'boolean :group 'vhdl-tools-vorg)

(defcustom vhdl-tools-vorg-tangle-header-argument-var nil
  "Variable used to filter code blocks to be tangled."
  :type 'boolean :group 'vhdl-tools-vorg)

;;;;; tools

(defcustom vhdl-tools-verbose nil
  "Make `vhdl-tools' verbose."
  :type 'boolean :group 'vhdl-tools)

(defcustom vhdl-tools-allowed-chars-in-signal "a-z0-9A-Z_"
  "Regexp with allowed characters in signal, constant or function.
Needed to determine end of name."
  :type 'string :group 'vhdl-tools)

(defcustom vhdl-tools-outline-regexp "^\\s-*-- [*]\\{1,8\\} "
  "Regexp to be used as `outline-regexp' when `vhdl-tools' minor mode is active."
  :type 'string :group 'vhdl-tools)

(defcustom vhdl-tools-imenu-regexp "^\\s-*--\\s-\\([*]\\{1,8\\}\\s-.+\\)"
  "Regexp ..."
  :type 'string :group 'vhdl-tools)

(defcustom vhdl-tools-use-outshine nil
  "Flag to activate `outshine' when `vhdl-tools' minor mode in active."
  :type 'boolean :group 'vhdl-tools)

(defcustom vhdl-tools-remap-smartscan nil
  "Flag to allow remapping `smartscan' when `vhdl-tools' minor mode in active."
  :type 'boolean :group 'vhdl-tools)

(defcustom vhdl-tools-manage-folding nil
  "Flag to allow remapping auto folding when jumping around."
  :type 'boolean :group 'vhdl-tools)

(defcustom vhdl-tools-recenter-nb-lines 10
  "Number of lines from top of scren to recenter point after jumping to new location."
  :type 'boolean :group 'vhdl-tools)

;;;; Internal Variables

(defvar vhdl-tools--jump-into-module-name nil)

(defvar vhdl-tools--store-link-link nil)

(defvar vhdl-tools--outline-active nil
  "Stores state of variable `outline-minor-mode' prior to activating the minor mode.")

(defvar vhdl-tools--ggtags-active nil
  "Stores state of `ggtags-mode' prior to activating the minor mode.")

(defvar vhdl-tools--outline-regexp-old nil
  "Stores state of `outline-regexp' prior to activating the minor mode.")

(defvar vhdl-tools--follow-links-tag nil)

(defvar vhdl-tools--follow-links-tosearch nil)


;;; Helper

;; Ancillary functions

(defun vhdl-tools--cleanup-tangled ()
  "Make invisible reference comments after tangling."
  (interactive)
  (when vhdl-tools-vorg-tangle-comments-link
    (save-excursion
      (when vhdl-tools-use-outshine
	(outline-show-all)
	(goto-char (point-min)))
      (while (re-search-forward (format "^-- %s.*$" vhdl-tools-vorg-tangle-comment-format-beg) nil t nil)
	(let ((endp (point))
	      (begp (progn (beginning-of-line) (point))))
	  (overlay-put (make-overlay begp endp)
		       'invisible
		       (intern "vhdl-tangled")))
	(forward-line))
      (add-to-invisibility-spec 'vhdl-tangled)
      (vhdl-tools--fold))))

(defun vhdl-tools--fold ()
  "Fold to current heading level."
  (when (and vhdl-tools-use-outshine
	     vhdl-tools-manage-folding
	     ;; only when heading exists
	     (save-excursion
	       (beginning-of-line)
	       (or (outline-on-heading-p)
		   (save-excursion
		     (re-search-backward (concat "^\\(?:" outline-regexp "\\)")
					 nil t)))))
    (save-excursion
      (outline-hide-sublevels 5)
      (org-back-to-heading nil)
      (outline-show-entry))))

(defun vhdl-tools--push-marker ()
  "Push tag (stolen from elisp-slime-nav.el)."
  (if (fboundp 'xref-push-marker-stack)
      (xref-push-marker-stack)
    (with-no-warnings
      (ring-insert find-tag-marker-ring (point-marker))))
  (setq ggtags-tag-ring-index nil))

(defun vhdl-tools--get-name ()
  "Extract word at current position DONT-DOWNCASE.
To determine end of word, vhdl-tools-allowed-chars-in-signal is used."
  (thing-at-point 'symbol t))

(defun vhdl-tools--get-entity-or-package-name ()
  "Return name of entity / package or empty string if nothing found."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^ *\\(entity\\|package\\) +" nil t nil)
        (vhdl-tools--get-name)
      "")))

(defun vhdl-tools--imenu-with-initial-minibuffer (str)
  "Text `STR'."
  (funcall `(lambda ()
	      (interactive)
	      (minibuffer-with-setup-hook
		  (lambda () (insert (format "%s " ,str)))
		(call-interactively 'helm-semantic-or-imenu)))))

(defun vhdl-tools--post-jump-function ()
  "To be called after jumping to recenter, indent, etc."
  (recenter-top-bottom vhdl-tools-recenter-nb-lines)
  (back-to-indentation))

(defun vhdl-tools-vorg--post-jump-function ()
  "To be called after jumping to recenter, indent, etc."
  (recenter-top-bottom vhdl-tools-recenter-nb-lines)
  (back-to-indentation))


;;; Misc

;;;###autoload
(defun vhdl-tools-beautify-region (arg)
  "Call beautify-region but auto activate region first.
With a prefix ARG, fall back to previous behaviour."
  (interactive "P")
  (if (equal arg '(4))
      (call-interactively 'vhdl-beautify-region)
    (save-excursion
      (when (not (region-active-p))
	(mark-paragraph))
      (call-interactively 'vhdl-beautify-region))))


;;; Jumping

;;;; Get definition

;;;###autoload
(defun vhdl-tools-get-buffer (entity-or-package-name)
  "Return buffer where ENTITY-OR-PACKAGE-NAME is found."
  (save-excursion
    (let ((thisfile (format "%s.vhd" entity-or-package-name)))
      ;; if open buffer exists, return it
      (if (get-buffer thisfile)
	  (get-buffer thisfile)
	;; if file exist, open it and return buffer
	(if (file-exists-p thisfile)
	    (progn
	      (find-file-noselect thisfile)
	      (get-buffer thisfile))
	  ;; search over all existing buffers
	  (let ((current-buffer-list (buffer-list))
		(counter 0)
		found)
	    ;; loop over all buffers
	    (while (and (nth counter current-buffer-list)
			(not found))
	      (set-buffer (nth counter current-buffer-list))
	      (if (equal entity-or-package-name (vhdl-tools--get-entity-or-package-name))
		  (setq found t)
		(setq counter (1+ counter))))
	    (if found
		(nth counter current-buffer-list)
	      nil)))))))

(defun vhdl-tools-package-names ()
  "Return a list of strings of all used packages or nil if nothing found.
Only use the form work.NAME.something."
  (save-excursion
    (let ((packages))
      ;; search for packages in current buffer
      (goto-char (point-min))
      (while (re-search-forward "^ *use  *work\." nil t nil)
        (forward-char)
	(when (not (member (vhdl-tools--get-name) packages))
	  (push (vhdl-tools--get-name) packages)))
      ;; search in all open buffers
      (dolist (var (buffer-list))
	(set-buffer var)
	(goto-char (point-min))
	(while (re-search-forward "^ *use  *work\." nil t nil)
	  (forward-char)
	  (when (not (member (vhdl-tools--get-name) packages))
	    (push (vhdl-tools--get-name) packages))))
      ;; search in all files in current dir
      (dolist (var (file-expand-wildcards "*.vhd"))
	(when (not (get-buffer var))
	  (find-file-noselect var))
	(set-buffer var)
	(goto-char (point-min))
	(while (re-search-forward "^ *use  *work\." nil t nil)
	  (forward-char)
	  (when (not (member (vhdl-tools--get-name) packages))
	    (push (vhdl-tools--get-name) packages))))
      packages)))

(defun vhdl-tools-process-file (name)
  "Search within a package or a vhdl file for NAME.
Test if it is a type definition or not."
  (let ((found nil)
	should-be-in-entity
	beginning-of-entity-port
	end-of-entity
	end-of-entity-port
	apoint)
    (save-excursion
      (goto-char (point-min))
      ;; search for entity ... is line
      (setq beginning-of-entity-port
	    (re-search-forward
	     (concat "^[ \t]*entity[ \n\t]+[" vhdl-tools-allowed-chars-in-signal "]+[ \n\t]+is") nil t nil))
      (if beginning-of-entity-port
          (progn
            (setq end-of-entity (save-excursion (re-search-forward "^[ \t]*end")))
            (re-search-forward "port[ \n\t]*(" nil t nil)
            (setq end-of-entity-port (progn (up-list) (point)))
            (goto-char (point-min))
            (setq should-be-in-entity (re-search-forward (concat " +" name "[ \n\t]+") nil t nil))
            (if (and should-be-in-entity
		     (< beginning-of-entity-port should-be-in-entity)
		     (> end-of-entity-port should-be-in-entity)
                     (< (save-excursion (re-search-forward ":" nil t nil))
			(save-excursion (re-search-forward "\n" nil t nil)))
                     (< (point)
			(save-excursion (re-search-forward ":" nil t nil)))
                     (< end-of-entity-port
			end-of-entity))
                (setq found (point)))))
      (goto-char (point-min))
      (while (and (not found)
		  (re-search-forward "^ *\\(component\\|function\\|procedure\\|constant\\|file\\|type\\|subtype\\)[ \n\t]+" nil t nil))
        (if (equal name (vhdl-tools--get-name))
            (setq found (point))))
      (goto-char (point-min))
      (while (and (not found)
		  (re-search-forward "^[ \t]*signal[ \n\t]+" nil t nil))
        (if (equal name (vhdl-tools--get-name))
            (setq found (point))
          (while (> (save-excursion (search-forward ":" nil t nil))
		    (if (setq apoint (save-excursion (search-forward "," nil t nil))) apoint 0))
            (search-forward "," nil t nil)
            (if (equal name (vhdl-tools--get-name))
                (setq found (point)))))))
    (if found found nil)))

(defun vhdl-tools-goto-type-def ()
  "Read word at point and try to find corresponding signal or type definition.
This function first tries to find a signal or type definition in the buffer from
where the function have been called.  It can only jump to signal, constant,
type and subtype definitions.  Works also for signals in an entity (in and out
ports, function will then jump to the entity).  To go back to the point where
the function has been called press.  If there was nothing found, it reads the
packages used, and works through all opened buffers to find packages used in
the vhdl file.  If a definition has been found in a package, package will be
displayed.  To go back to original vhdl file press."
  (interactive)
  ;; when no symbol at point, move forward to next symbol
  (vhdl-tools--push-marker)
  (when (not (vhdl-tools--get-name))
    (back-to-indentation))
  ;; check if found definition in calling file
  (if (not (setq found (vhdl-tools-process-file (vhdl-tools--get-name))))
      ;; no definition found in calling file found
      (let ((to-search-for (vhdl-tools--get-name))
	    (package-list (vhdl-tools-package-names))
	    (counter 0)
	    found
	    package-buffer)
	;; loop over all packages _____________________________________
	(while (and (not found)
		    (nth counter package-list))
	  (setq package-buffer
		(vhdl-tools-get-buffer (nth counter package-list)))
	  (with-current-buffer package-buffer
	    (setq found (vhdl-tools-process-file to-search-for)))
	  (setq counter (1+ counter)))
	;; loop over ____________________________________________________
	(if found
	    (progn
	      (switch-to-buffer package-buffer)
	      (goto-char found)
	      (vhdl-tools--post-jump-function))
	  (message "sorry, no corresponding definition found")))
    ;; found in current file
    (progn
      (goto-char found)
      (vhdl-tools--post-jump-function))))

;;;; Jump into module

;;;###autoload
(defun vhdl-tools-jump-into-module()
  "When point is at an instance, jump into the module.
Additionally, move point to signal at point.
Declare a key-bind to get back to the original point."
  (interactive)
  ;; when no symbol at point, move forward to next symbol
  (when (not (vhdl-tools--get-name))
    (back-to-indentation))
  ;; when nil, do nothing
  (when (vhdl-tools--get-name)
    ;; necessary during hook (see later)
    (setq vhdl-tools--jump-into-module-name (vhdl-tools--get-name))
    (vhdl-tools--push-marker)
    (save-excursion
      ;; case of component instantiation
      ;; locate component name to jump into
      (if (search-backward-regexp "port map" nil t)
	  (progn
	    (search-backward-regexp "\\s-*:\\s-"  nil t)
	    ;; in case there is a comment at the end of the entity line
	    (back-to-indentation)
	    (search-forward-regexp "  " nil t)
 	    (backward-char 3))
	;; case of component declaration
	(progn
	  (search-backward-regexp " component ")
	  ;; in case there is a comment at the end of the entity line
	  (back-to-indentation)
	  (search-forward-regexp "  " nil t)
	  (backward-char 3)))
      ;; empty old content in hook
      (setq ggtags-find-tag-hook nil)
      ;; update hook to execute an action
      ;; once jumped to new buffer
      (add-hook 'ggtags-find-tag-hook
		'(lambda()
		   (when (progn
			   (vhdl-tools--fold)
			   (search-forward (format "%s " vhdl-tools--jump-into-module-name)  nil t))
		     (vhdl-tools--post-jump-function)
		     ;; erase modified hook
		     (setq vhdl-tools--jump-into-module-name nil)
		     ;; erase hook
		     (setq ggtags-find-tag-hook nil))
		   ;; remove last jump so that `pop-tag-mark' will get to
		   ;; original position before jumping
		   (ring-remove find-tag-marker-ring 0)))
      ;; jump !
      (call-interactively 'ggtags-find-definition))))

;;;; Jump to first

;; Utility to jump to first time a symbol appears on file

;;;###autoload
(defun vhdl-tools-jump-first ()
  "Jump to first occurrence of symbol at point.
When no symbol at point, move point to indentation."
  (interactive)
  ;; when no symbol at point, move forward to next symbol
  (when (not (vhdl-tools--get-name))
    (back-to-indentation))
  ;; when nil, do nothing
  (when (vhdl-tools--get-name)
    (vhdl-tools--push-marker)
    (let ((vhdl-tools-jump-first-name (vhdl-tools--get-name)))
      (goto-char (point-min))
      (search-forward-regexp vhdl-tools-jump-first-name nil t)
      (vhdl-tools--fold)
      (backward-word))))

;;;; Jump Upper

;; Utility to jump to upper level

;;;###autoload
(defun vhdl-tools-jump-upper ()
  "Get to upper level module and move point to signal at point.
When no symbol at point, move point to indentation."
  (interactive)
  (require 'vc)
  ;; when no symbol at point, move forward to next symbol
  (when (not (vhdl-tools--get-name))
    (back-to-indentation))
  (let ((vhdl-tools-thing (vhdl-tools--get-name)))
    (vhdl-tools--push-marker)
    (save-excursion
      ;; get back to entity
      (search-backward-regexp "^entity")
      (forward-word)
      (forward-char 2)
      ;; Jump by searching with prefilling minubuffer
      (funcall `(lambda ()
		  (minibuffer-with-setup-hook
		      (lambda ()
			;; (insert (format "^.* : \\(entity work.\\)*%s$" ,(vhdl-tools--get-name)))
			(insert (format "^.*: %s$ vhd" ,(vhdl-tools--get-name))))
		    (call-interactively 'helm-grep-do-git-grep (vc-find-root (buffer-file-name) ".git") nil))))
      ;; search, when nil, do nothing
      (when vhdl-tools-thing
	(search-forward-regexp (format "%s " vhdl-tools-thing) nil t)
	(vhdl-tools--fold)
	(vhdl-tools--post-jump-function)))))

;;; SmartScan

;; Custom version of `smartscan' jumping functions.  Here, I manage
;; folding/unfolding of code headings, so that upon jumping only the
;; relevant section is shown

;;;; Go Forward

;;;###autoload
(defun vhdl-tools-smcn-next()
  (interactive)
  (smartscan-symbol-go-forward)
  (vhdl-tools--fold)
  (recenter-top-bottom vhdl-tools-recenter-nb-lines))

;;;###autoload
(defun vhdl-tools-vorg-smcn-next()
  (interactive)
  (smartscan-symbol-go-forward)
  (vhdl-tools--fold)
  (recenter-top-bottom vhdl-tools-recenter-nb-lines))

;;;; Go Backwards

;;;###autoload
(defun vhdl-tools-smcn-prev()
  (interactive)
  (smartscan-symbol-go-backward)
  (vhdl-tools--fold)
  (recenter-top-bottom vhdl-tools-recenter-nb-lines))

;;;###autoload
(defun vhdl-tools-vorg-smcn-prev()
  (interactive)
  (smartscan-symbol-go-backward)
  (vhdl-tools--fold)
  (recenter-top-bottom vhdl-tools-recenter-nb-lines))

;;; Org / VHDL

;; Following the literate programming paradigm, here we intend to provide some
;; infrastructure to deal with jumping between a "filename.vhd" and its
;; corresponding "filename.org", the former being tangled from the latter.

;;;; VHDL to VOrg

;;;###autoload
(defun vhdl-tools-vorg-jump-to-vorg()
  "From `vhdl' file, jump to same line in `vorg' file."
  (interactive)
  (let ((myfile (format "%s.org" (file-name-base)))
	(myline (vhdl-tools-vorg-get-current-line)))
    (if (file-exists-p myfile)
	(progn
	  (if vhdl-tools-vorg-tangle-comments-link
	      ;; use org feature
	      (let (;; I avoid scanning too much files: I already know where the
		    ;; related org file is.
		    ;; TODO: still too many files to scan ...
		    (org-id-search-archives nil)
		    (org-id-locations nil)
		    (org-agenda-files nil)
		    (org-id-extra-files nil))
		(org-babel-tangle-jump-to-org))
	    ;; use custom search
	    (progn
	      (find-file myfile)
	      (beginning-of-buffer)
	      (search-forward myline nil t nil)))
	  (org-content 5)
	  (org-back-to-heading nil)
	  (org-show-subtree)
	  (search-forward myline nil t nil)
	  (recenter-top-bottom vhdl-tools-recenter-nb-lines)
	  (back-to-indentation))
      (message (format "no %s file exists" myfile)))))

;;;; VOrg to VHDL

;;;###autoload
(defun vhdl-tools-vorg-jump-from-vorg()
  "From `vorg' file, jump to same line in `vhdl' file, tangling the
code before if necessary."
  (interactive)
  (call-interactively 'vhdl-tools-vorg-tangle)
  (let ((vhdlfile (format "%s.vhd" (file-name-base)))
	;; store current line
	(myline (vhdl-tools-vorg-get-current-line)))
    (when (file-exists-p vhdlfile)
      (find-file vhdlfile)
      (goto-char (point-min))
      (when vhdl-tools-use-outshine
	(outline-next-heading))
      (when (search-forward myline nil t nil)
	(vhdl-tools--fold)
	(search-forward myline nil t nil)
	(recenter-top-bottom vhdl-tools-recenter-nb-lines)
	(back-to-indentation)))))

;;;; VOrg to module

;;;###autoload
(defun vhdl-tools-vorg-jump-from-vorg-into-module()
  "From `vorg' file, jump to same line in `vhdl' file, tangling the
code before if necessary, then jump into module."
  (interactive)
  (vhdl-tools-vorg-jump-from-vorg)
  (vhdl-tools-jump-into-module))

;;;; VOrg tangle

;;;###autoload
(defun vhdl-tools-vorg-tangle (orgfile)
  "Tangle a `vorg' `ORGFILE' file to its corresponding `vhdl' file."
  (interactive (list (format "%s.org" (file-name-base))))
  (let ((vhdlfile (format "%s.vhd" (file-name-base orgfile))))
    (if (or (file-newer-than-file-p orgfile vhdlfile)
	    (not (file-exists-p vhdlfile)))
	;; do tangle
	(let ((org-babel-tangle-uncomment-comments nil)
	      ;; sets the "comments:link" header arg
	      ;; possible as this is constant header arg, not dynamic with code block
	      (org-babel-default-header-args
	       (if vhdl-tools-vorg-tangle-comments-link
		   (cons '(:comments . "link")
			 (assq-delete-all :comments org-babel-default-header-args))
		 org-babel-default-header-args))
	      (org-babel-tangle-comment-format-beg
	       (format "%s %s" vhdl-tools-vorg-tangle-comment-format-beg
		       org-babel-tangle-comment-format-beg))
	      (org-babel-tangle-comment-format-end
	       (format "%s %s" vhdl-tools-vorg-tangle-comment-format-end
		       org-babel-tangle-comment-format-end)))
	  ;; tangle and beautify the tangled file only when there are tangled blocks
	  (when (org-babel-tangle-file orgfile vhdlfile "vhdl")
	    (when vhdl-tools-verbose
	      (message (format "File %s tangled to %s." orgfile vhdlfile)))
	    ;; When tangling the org file, this code helps to auto set proper
	    ;; indentation, whitespace fixup, alignment, and case fixing to entire
	    ;; exported buffer
	    (org-babel-with-temp-filebuffer vhdlfile
	      (vhdl-beautify-buffer)
	      (vhdl-beautify-buffer)
	      (save-buffer))))
      ;; don't tangle
      (when vhdl-tools-verbose
	(message (format "File %s NOT tangled to %s." orgfile vhdlfile))))))

;;;###autoload
(defun vhdl-tools-vorg-tangle-all ()
  "Tangle all `vorg' files in current dir to its corresponding `vhdl' file."
  (interactive)
  (let ((vc-follow-symlinks nil)
	(vhdl-tools-verbose t)
	(org-global-properties
	 '(("header-args:vhdl" . ":prologue (vhdl-tools-vorg-prologue-header-argument) :tangle (vhdl-tools-vorg-tangle-header-argument)"))))
    (loop for thisfile in (file-expand-wildcards "*.org") do
	  (unless (or (string-match "readme" thisfile)
		      (and (file-exists-p (format "%s.el" (file-name-base thisfile)))
			   (not (file-newer-than-file-p thisfile (format "%s.el" (file-name-base thisfile))))))
	    (vhdl-tools-vorg-tangle thisfile)))))

;;;; VOrg source editing beautify

(defun vhdl-tools-vorg-src-edit-beautify ()
  "To be added to `org-src-mode-hook' when `vorg' mode is active.
Beautifies source code blocks before editing."
  (when (string= major-mode "vhdl-mode")
    (require 'vhdl-mode)
    (vhdl-beautify-buffer)))

;;;; VOrg source block beautify

;;;###autoload
(defun vhdl-tools-vorg-src-block-beautify ()
  "Beautify of source code block at point."
  (interactive)
  (when (org-in-src-block-p t)
    (org-edit-src-code)
    (vhdl-beautify-buffer)
    (org-edit-src-save)
    (org-edit-src-exit)))

;;;; Vorg Helper

;; Ancillary functions


(defun vhdl-tools-vorg-get-current-line ()
  "Send current line avoiding any comments."
  (save-excursion
    (back-to-indentation)
    (let ((vhdl-tools-vorg-line-beginning (point)))
      ;; check there is a comment in current line
      (if (let ((maxposition (save-excursion
			       (end-of-line)
			       (point))))
	    (save-excursion
	      (re-search-forward "--" maxposition t)))
	  (progn
	    (re-search-forward "--")
	    (re-search-backward " ")
	    ;; previous non whitespace character
	    (re-search-backward "\\S-"))
	(end-of-line))
      (buffer-substring-no-properties
       vhdl-tools-vorg-line-beginning
       (point)))))

(defun vhdl-tools-vorg-tangle-header-argument ()
  "To be used as def argument to `tangle' in source block header."
  (if (let ((mytags (org-get-tags-at (point) t)))
	(or (member vhdl-tools-vorg-tangle-header-argument-var mytags)
	    (null mytags)))
      (format "%s.vhd" (file-name-base
			(buffer-file-name)))
    "no"))

(defun vhdl-tools-vorg-prologue-header-argument ()
  "To be used as def argument to `prologue' in source block header."
  (save-excursion
    (org-back-to-heading nil)
    (let ((heading (car (cdr (org-element-headline-parser (point))))))
      (format "\n-- %s %s\n\n"
	      (if (> (plist-get heading ':level) 1)
		  (make-string (- (plist-get heading ':level) 1)
			       ?*)
		(make-string 1 ?*))
	      (plist-get heading ':raw-value)))))

;;; Links
;;
;; The goal here is, using the ggtags infrastructure, to implement a mechanism to
;; follow links in comments.
;;
;; For example, in the form of =tag@tosearch=
;;
;; "TM_IO_Sequencer@Pixel"
;;
;; will get to the definition of ~TM_IO_Sequencer~, and then forward search for
;; ~Pixel~. To achieve this, I update a hook before switching buffers with
;; ~find-tag~.

;;;; Link Store

;;;###autoload
(defun vhdl-tools-store-link ()
  "Store current line as a link."
  (interactive)
  (let* ((myline (vhdl-tools-vorg-get-current-line))
	 (myentity (save-excursion
		     (search-backward-regexp "entity")
		     (forward-word)
		     (forward-char 2)
		     (vhdl-tools--get-name)))
	 (mylink (format "%s\@%s" myentity myline)))
    (message mylink)
    (setq vhdl-tools--store-link-link mylink)))

;;;; Link Paste

;;;###autoload
(defun vhdl-tools-paste-link()
  "Paste previous stored link."
  (interactive)
  (insert (format "`%s`" vhdl-tools--store-link-link)))

;;;; Link Follow

;;;###autoload
(defun vhdl-tools-follow-links(arg)
  "Follow links in the form of Tag:ToSearch'."
  (interactive "P")
  ;; get item in the form of tag@tosearch
  (save-excursion
    (let* ((tmp-point-min (progn  ;; beginning of item
			    (search-backward-regexp "\`" )
			    (+ 1 (point))))
	   (tmp-point-max (progn ;; end of item
			    (forward-char 1)
			    (search-forward-regexp "\`" )
			    (- (point) 1)))
	   (vhdl-tools-follow-links-item ;; item
	    (buffer-substring-no-properties
	     tmp-point-min tmp-point-max)))
      ;; tag
      (setq vhdl-tools--follow-links-tag
	    (substring vhdl-tools-follow-links-item 0
		       (string-match "@" vhdl-tools-follow-links-item)))
      ;; tosearch
      (setq vhdl-tools--follow-links-tosearch
	    ;; with a prefix argument, ignore tosearch
	    (when (not (equal arg '(4)))
	      nil
	      (if (string-match "@" vhdl-tools-follow-links-item)
		  (substring
		   vhdl-tools-follow-links-item
		   (+ 1 (string-match "@" vhdl-tools-follow-links-item)) nil)
		nil)))))
  ;; when tosearch non nil, update hook to execute an action
  (when vhdl-tools--follow-links-tosearch
    ;; empty old content in hook
    (setq ggtags-find-tag-hook nil)
    (vhdl-tools--push-marker)
    ;; declare action after jumping to new buffer
    (add-hook 'ggtags-find-tag-hook
	      '(lambda()
		 ;; action: forward search
		 ;; if no tosearch is found, do nothing
		 (when (search-forward vhdl-tools--follow-links-tosearch nil t)
		   ;; otherwise, do this
		   (vhdl-tools--post-jump-function))
		 ;; erase modified hook
		 (setq vhdl-tools--follow-links-tosearch nil)
		 (setq ggtags-find-tag-hook nil)))
    ;; jump !
    (ggtags-find-definition vhdl-tools--follow-links-tag)))


;;; Headings

;;;; Get to next

;;;###autoload
(defun vhdl-tools-headings-next()
  "Get to next heading."
  (interactive)
  ;; hack: when already in outline-regexp, point will stay
  ;; I move forward one char to force getting to next
  (forward-char)
  (re-search-forward outline-regexp)
  ;; (beginning-of-line)
  (vhdl-tools--fold)
  (vhdl-tools--post-jump-function))

;;;###autoload
(defun vhdl-tools-vorg-headings-next()
  "Get to next heading in vorg buffer."
  (interactive)
  (org-next-visible-heading 1)
  (when vhdl-tools-manage-folding
    (outline-hide-sublevels 5)
    (org-show-entry)
    (vhdl-tools-vorg--post-jump-function)))

;;;; Get to previous

;;;###autoload
(defun vhdl-tools-headings-prev()
  "Get to previous heading."
  (interactive)
  ;; hack: no necessary in this case
  ;; (see vhdl-tools-headings-next)
  (re-search-backward outline-regexp)
  ;; (beginning-of-line)
  (vhdl-tools--fold)
  (vhdl-tools--post-jump-function))

;;;###autoload
(defun vhdl-tools-vorg-headings-prev()
  "Get to next heading in vorg buffer."
  (interactive)
  (org-previous-visible-heading 1)
  (when vhdl-tools-manage-folding
    (outline-hide-sublevels 5)
    (org-show-entry)
    (vhdl-tools-vorg--post-jump-function)))


;;; Helm-imenu navigation

;;;; Standard Imenu

;;;###autoload
(defun vhdl-tools-imenu()
  (interactive)
  (let ((imenu-generic-expression vhdl-imenu-generic-expression))
    (set-buffer-modified-p t)
    (save-buffer)
    (call-interactively 'imenu)
    (vhdl-tools--fold)
    (vhdl-tools--post-jump-function)))

;;;; Instances

;;;###autoload
(defun vhdl-tools-imenu-instance()
  (interactive)
  (let ((imenu-generic-expression vhdl-imenu-generic-expression)
	(helm-autoresize-max-height 100)
	(helm-candidate-number-limit 50))
    (set-buffer-modified-p t)
    (save-buffer)
    (vhdl-tools--imenu-with-initial-minibuffer "^Instance")
    (vhdl-tools--fold)
    (vhdl-tools--post-jump-function)))

;;;; Processes

;;;###autoload
(defun vhdl-tools-imenu-processes()
  (interactive)
  (let ((imenu-generic-expression vhdl-imenu-generic-expression)
	(helm-autoresize-max-height 100)
	(helm-candidate-number-limit 50))
    (set-buffer-modified-p t)
    (save-buffer)
    (vhdl-tools--imenu-with-initial-minibuffer "^Process")
    (vhdl-tools--fold)
    (vhdl-tools--post-jump-function)))

;;;; Components

;;;###autoload
(defun vhdl-tools-imenu-component()
  (interactive)
  (let ((imenu-generic-expression vhdl-imenu-generic-expression)
	(helm-autoresize-max-height 100)
	(helm-candidate-number-limit 50))
    (set-buffer-modified-p t)
    (save-buffer)
    (vhdl-tools--imenu-with-initial-minibuffer "^Component")
    (vhdl-tools--fold)
    (vhdl-tools--post-jump-function)))

;;;; Headings

;;;###autoload
(defun vhdl-tools-imenu-headers()
  (interactive)
  (let ((imenu-generic-expression `(("" ,vhdl-tools-imenu-regexp 1)))
	(helm-autoresize-max-height 100)
	(helm-candidate-number-limit 50))
    (set-buffer-modified-p t)
    (save-buffer)
    (call-interactively 'helm-semantic-or-imenu)
    (vhdl-tools--fold)
    (vhdl-tools--post-jump-function)))

;;;; Outshine - imenu

;;;###autoload
(defun vhdl-tools-outshine-imenu-headers(arg)
  (interactive "P")
  (if (equal arg '(4))
      (outshine-imenu nil)
    (vhdl-tools-imenu-headers))
  (vhdl-tools--fold)
  (vhdl-tools--post-jump-function))

;;;; All

;;;###autoload
(defun vhdl-tools-imenu-all()
  "In a vhdl buffer, call `helm-semantic-or-imenu', show all items.
  Processes, instances and doc headers are shown in order of appearance."
  (interactive)
  (let ((imenu-generic-expression
	 `(;; process
	   ("" "^\\s-*\\(\\(\\w\\|\\s_\\)+\\)\\s-*:\\(\\s-\\|\n\\)*\\(\\(postponed\\s-+\\|\\)process\\)" 1)
	   ;; instance
	   ("" "^\\s-*\\(\\(\\w\\|\\s_\\)+\\s-*:\\(\\s-\\|\n\\)*\\(entity\\s-+\\(\\w\\|\\s_\\)+\\.\\)?\\(\\w\\|\\s_\\)+\\)\\(\\s-\\|\n\\)+\\(generic\\|port\\)\\s-+map\\>" 1)
	   ("" ,vhdl-tools-imenu-regexp 1)
	   ("Subprogram" "^\\s-*\\(\\(\\(impure\\|pure\\)\\s-+\\|\\)function\\|procedure\\)\\s-+\\(\"?\\(\\w\\|\\s_\\)+\"?\\)" 4)
	   ;; ("Instance" "^\\s-*\\(\\(\\w\\|\\s_\\)+\\s-*:\\(\\s-\\|\n\\)*\\(entity\\s-+\\(\\w\\|\\s_\\)+\\.\\)?\\(\\w\\|\\s_\\)+\\)\\(\\s-\\|\n\\)+\\(generic\\|port\\)\\s-+map\\>" 1)
	   ("Component" "^\\s-*\\(component\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\)" 2)
	   ("Procedural" "^\\s-*\\(\\(\\w\\|\\s_\\)+\\)\\s-*:\\(\\s-\\|\n\\)*\\(procedural\\)" 1)
	   ;; ("Process" "^\\s-*\\(\\(\\w\\|\\s_\\)+\\)\\s-*:\\(\\s-\\|\n\\)*\\(\\(postponed\\s-+\\|\\)process\\)" 1)
	   ("Block" "^\\s-*\\(\\(\\w\\|\\s_\\)+\\)\\s-*:\\(\\s-\\|\n\\)*\\(block\\)" 1)
	   ("Package" "^\\s-*\\(package\\( body\\|\\)\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\)" 3)
	   ("Configuration" "^\\s-*\\(configuration\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\s-+of\\s-+\\(\\w\\|\\s_\\)+\\)" 2)
	   ("" "^\\s-*\\(architecture\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\s-+of\\s-+\\(\\w\\|\\s_\\)+\\)" 2)
	   ("Entity" "^\\s-*\\(entity\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\)" 2)
	   ("Context" "^\\s-*\\(context\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\)" 2)))
	(helm-autoresize-max-height 100)
	(helm-candidate-number-limit 50))
    (set-buffer-modified-p t)
    (save-buffer)
    (call-interactively 'helm-semantic-or-imenu)
    (vhdl-tools--fold)))


;;; Minor Mode - Tools

;;;; Keybindings

(defvar vhdl-tools-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c M-D") #'vhdl-tools-goto-type-def)
    (define-key m (kbd "C-c M-j") #'vhdl-tools-follow-links)
    (define-key m (kbd "C-c M-w") #'vhdl-tools-store-link)
    (define-key m (kbd "C-c M-y") #'vhdl-tools-paste-link)
    (define-key m (kbd "C-c M-.") #'vhdl-tools-jump-into-module)
    (define-key m (kbd "C-c M-a") #'vhdl-tools-jump-first)
    (define-key m (kbd "C-c M-u") #'vhdl-tools-jump-upper)
    (define-key m (kbd "C-c M-^") #'vhdl-tools-vorg-jump-to-vorg)
    (define-key m (kbd "C-c C-n") #'vhdl-tools-headings-next)
    (define-key m (kbd "C-c C-p") #'vhdl-tools-headings-prev)
    (define-key m (kbd "C-c M-b") #'vhdl-tools-beautify-region)
    ;;
    ;; imenu related
    (define-prefix-command 'vhdl-tools-imenu-map)
    (define-key m (kbd "C-x c i") 'vhdl-tools-imenu-map)
    (define-key vhdl-tools-imenu-map (kbd "m") #'vhdl-tools-imenu)
    (define-key vhdl-tools-imenu-map (kbd "i") #'vhdl-tools-imenu-instance)
    (define-key vhdl-tools-imenu-map (kbd "p") #'vhdl-tools-imenu-processes)
    (define-key vhdl-tools-imenu-map (kbd "c") #'vhdl-tools-imenu-component)
    (define-key vhdl-tools-imenu-map (kbd "SPC") #'vhdl-tools-imenu-headers)
    (define-key vhdl-tools-imenu-map (kbd "a") #'vhdl-tools-imenu-all)
    m)
  "Keymap for `vhdl-tools'.")

;;;; Mode

;;;###autoload
(define-minor-mode vhdl-tools-mode
  "Utilities for navigating vhdl sources.

Key bindings:
\\{vhdl-tools-map}"
  :lighter " #vtool"
  :keymap vhdl-tools-map
  :group 'vhdl-tools
  :global nil
  (require 'vc)
  (if vhdl-tools-mode
      ;; activate when gtags files are available
      (if (file-exists-p (format "%sGTAGS" (vc-find-root (buffer-file-name) ".git")))
	  (progn
	    ;; optional smartscan remapping
	    (when (and vhdl-tools-remap-smartscan
		       vhdl-tools-use-outshine
		       (boundp 'smartscan-mode)
		       (smartscan-mode 1))
	      (define-key vhdl-mode-map [remap smartscan-symbol-go-forward]
		#'vhdl-tools-smcn-next)
	      (define-key vhdl-mode-map [remap smartscan-symbol-go-backward]
		#'vhdl-tools-smcn-prev))
	    ;;
	    (when vhdl-tools-use-outshine
	      ;; try to keep things as they were
	      (setq vhdl-tools--outline-active outline-minor-mode)
	      (outline-minor-mode 1)
	      ;; (outshine-hook-function)
	      ;; custom outline regexp
	      (setq-local vhdl-tools--outline-regexp-old outline-regexp)
	      (setq-local outline-regexp vhdl-tools-outline-regexp)
	      ;;
	      (define-key vhdl-tools-imenu-map (kbd "SPC") #'vhdl-tools-outshine-imenu-headers))
	    (setq vhdl-tools--ggtags-active ggtags-mode)
	    (ggtags-mode 1)
	    ;; notify
	    (when vhdl-tools-verbose
	      (message "VHDL Tools enabled.")))
	;; not gtags files available
	(when vhdl-tools-verbose
	  (message "VHDL Tools NOT enabled.")))
    ;; disable
    (progn
      (when vhdl-tools-remap-smartscan
	(define-key vhdl-tools-map [remap smartscan-symbol-go-forward] nil)
	(define-key vhdl-tools-map [remap smartscan-symbol-go-backward] nil))
      (when (not vhdl-tools--ggtags-active)
	(ggtags-mode -1))
      (when vhdl-tools-use-outshine
	;; try to keep things as they were
	(when (not vhdl-tools--outline-active)
	  (outline-minor-mode -1))
	;; custom outline regexp
	(setq-local outline-regexp vhdl-tools--outline-regexp-old))
      ;; notify
      (when vhdl-tools-verbose
	(message "VHDL Tools disabled.")))))


;;; Minor Mode - vOrg

;;;; Keybindings

(defvar vhdl-tools-vorg-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c M-,") #'vhdl-tools-vorg-jump-from-vorg)
    (define-key m (kbd "C-c M-.") #'vhdl-tools-vorg-jump-from-vorg-into-module)
    (define-key m [remap org-babel-tangle] #'vhdl-tools-vorg-tangle)
    (define-key m (kbd "C-c C-v _") #'vhdl-tools-vorg-tangle-all)
    (define-key m (kbd "C-c C-n") #'vhdl-tools-vorg-headings-next)
    (define-key m (kbd "C-c C-p") #'vhdl-tools-vorg-headings-prev)
    (define-key m (kbd "C-c M-b") #'vhdl-tools-vorg-src-block-beautify)
    m)
  "Keymap for `vhdl-tools-vorg'.")

;;;; Mode

;;;###autoload
(define-minor-mode vhdl-tools-vorg-mode
  "Utilities for navigating vhdl sources in vorg files.

Key bindings:
\\{vhdl-tools-map}"
  :lighter " vOrg"
  :keymap vhdl-tools-vorg-map
  :group 'vhdl-tools-vorg
  :global nil
  (require 'vc)
  (require 'vhdl-tools)
  (if vhdl-tools-vorg-mode
      (progn
	(when (and vhdl-tools-remap-smartscan
		   (boundp 'smartscan-mode)
		   (smartscan-mode 1))
	  (define-key vhdl-tools-vorg-map [remap smartscan-symbol-go-forward]
	    #'vhdl-tools-vorg-smcn-next)
	  (define-key vhdl-tools-vorg-map [remap smartscan-symbol-go-backward]
	    #'vhdl-tools-vorg-smcn-prev))
	(when vhdl-tools-verbose
	  (message "VHDL Tools Vorg enabled."))
	(add-hook 'org-src-mode-hook 'vhdl-tools-vorg-src-edit-beautify))
    ;; disable
    (progn
      (when vhdl-tools-verbose
	(message "VHDL Tools Vorg disabled."))
      (remove-hook 'org-src-mode-hook 'vhdl-tools-vorg-src-edit-beautify))))

(provide 'vhdl-tools)

;;; vhdl-tools.el ends here
