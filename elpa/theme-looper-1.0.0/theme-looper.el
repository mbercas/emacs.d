;;; theme-looper.el --- Loop thru the available color-themes

;; This file is not part of Emacs

;; Author: Mohammed Ismail Ansari <team.terminal@gmail.com>
;; Version: 1.0
;; Package-Version: 1.0.0
;; Keywords: convenience, color-themes
;; Maintainer: Mohammed Ismail Ansari <team.terminal@gmail.com>
;; Created: 2014/03/22
;; Package-Requires: ((cl-lib "0.5"))
;; Description: Loop thru the available color-themes with a key-binding
;; URL: http://ismail.teamfluxion.com
;; Compatibility: Emacs24


;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
;; for more details.
;;

;;; Install:

;; Put this file on your Emacs-Lisp load path, add following into your
;; ~/.emacs startup file
;;
;;     (require 'theme-looper)
;;
;; And set a key-binding for cycling thru themes
;;
;;     (global-set-key (kbd "C-|") 'theme-looper-enable-next-theme)
;;
;; Or you can switch to a random theme
;;
;;     (global-set-key (kbd "C-\\") 'theme-looper-enable-random-theme)
;;
;; You can also set your list of favorite themes
;;
;;     (theme-looper-set-theme-set '(wombat tango-dark wheatgrass))
;;
;; You can alternatively set the ignored themes
;;
;;     (theme-looper-set-ignored-themes '(cobalt))
;;
;; Or you can set both, in which case only the favorite themes that are not
;; within the ones to be ignored are used.
;;
;; You can set customization to be applied after every theme switch
;;
;;     (theme-looper-set-customizations my-func)
;;

;;; Commentary:

;;     You can use this package to cycle thru the available themes in Emacs 24
;;     with a key-binding of your choice. Just a key combination to switch to
;;     the next theme.
;;
;;  Overview of features:
;;
;;     o   Automatically cycle thru the available color themes
;;     o   Provides an option to select a list of your favorite color themes
;;

;;; Code:

(require 'cl-lib)

(defvar theme-looper--favorite-themes)

(defvar theme-looper--ignored-themes
  nil)

(defun theme-looper--further-customize
    nil)

;;;###autoload
(defun theme-looper-set-theme-set (themes)
  "Sets the list of color-themes to cycle thru"
  (setq theme-looper--favorite-themes
	themes))

;;;###autoload
(defun theme-looper-set-ignored-themes (themes)
  "Sets the list of color-themes to ignore"
  (setq theme-looper--ignored-themes
	themes))

;;;###autoload
(defun theme-looper-set-customizations (func)
  "Sets customization to be applied after every theme switch"
  (setq theme-looper--further-customize
        func))

(defun theme-looper--get-current-theme ()
  "Determines the currently enabled theme"
  (car custom-enabled-themes))

(defun theme-looper--get-current-theme-index ()
  "Finds the currently enabled color-theme in the list of color-themes"
  (cl-position (theme-looper--get-current-theme)
               (theme-looper--get-looped-themes) :test #'equal))

(defun theme-looper--get-looped-themes ()
  (cl-remove-if (lambda (theme)
                  (member theme
                          theme-looper--ignored-themes))
                theme-looper--favorite-themes))

(defun theme-looper--get-next-theme-index ()
  "Find the index of the next color-theme in the list, to be moved to"
  (let ((theme-looper-current-theme-index (theme-looper--get-current-theme-index)))
    (cond
     ((equal theme-looper-current-theme-index
	     'nil)
      0)
     ((equal theme-looper-current-theme-index
	     (- (length (theme-looper--get-looped-themes))
		1))
      0)
     ((+ 1
	 theme-looper-current-theme-index)))))

(defun theme-looper--get-next-theme ()
  "Determines the next color-theme to be enabled"
  (nth (theme-looper--get-next-theme-index)
       (theme-looper--get-looped-themes)))

(defun theme-looper--disable-all-themes ()
  "Disables all the enabled color-themes"
  (mapcar 'disable-theme
	  custom-enabled-themes))

;;;###autoload
(defun theme-looper-enable-theme (theme)
  "Enables the specified color-theme"
  (theme-looper--disable-all-themes)
  (load-theme theme
              t)
  (theme-looper--further-customize)
  (message "Switched to theme: %s"
           theme))

;;;###autoload
(defun theme-looper-enable-next-theme ()
  "Enables the next color-theme in the list"
  (interactive)
  (let ((theme-looper-next-theme (theme-looper--get-next-theme)))
    (theme-looper-enable-theme theme-looper-next-theme)))

;;;###autoload
(defun theme-looper-enable-random-theme ()
  "Enables a random theme from the list"
  (interactive)
  (let ((theme-looper-next-theme (nth (random (length (theme-looper--get-looped-themes)))
                                      (theme-looper--get-looped-themes))))
    (theme-looper-enable-theme theme-looper-next-theme)))

(theme-looper-set-theme-set (custom-available-themes))

(provide 'theme-looper)

;;; theme-looper.el ends here
