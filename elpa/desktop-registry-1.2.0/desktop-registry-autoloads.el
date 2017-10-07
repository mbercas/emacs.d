;;; desktop-registry-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "desktop-registry" "desktop-registry.el" (22398
;;;;;;  23970 743749 324000))
;;; Generated autoloads from desktop-registry.el

(autoload 'desktop-registry-current-desktop "desktop-registry" "\
Get the name of the currently loaded desktop.

Returns DEFAULT when the variable `desktop-dirname' is nil, which
means there is no desktop currently loaded.

\(fn &optional DEFAULT)" nil nil)

(autoload 'desktop-registry-add-directory "desktop-registry" "\
Add DIR to the desktop registry, possibly using NAME.

If this command is called interactively, the location for DIR is
requested of the user, and if the universal argument (`C-u') was
used before calling this command a name will also be requested
for this directory.  This is useful when the directory name is
not the project name or when it would result in duplicate entries
in `desktop-registry-registry'.

\(fn DIR &optional NAME)" t nil)

(autoload 'desktop-registry-add-current-desktop "desktop-registry" "\
Add the currently opened desktop file to `desktop-registry-registry'.

If NAME is specified use that as the name for the registry entry.

If this command is called interactively and the universal
argument (`C-u') was used before calling this command the name
will be requested of the user.  This is useful when the directory
name is not the project name or when it would result in duplicate
entries in `desktop-registry-registry'.

\(fn &optional NAME)" t nil)

(autoload 'desktop-registry-remove-desktop "desktop-registry" "\
Remove DESKTOP from the desktop registry.

If this command is called interactively DESKTOP will be inferred
from the location of the cursor when viewing the desktop list, or
will be asked of the user (with completion) when the desktop list
is not currently shown.

\(fn DESKTOP)" t nil)

(autoload 'desktop-registry-rename-desktop "desktop-registry" "\
Rename desktop OLD to NEW.

If this command is called interactively OLD will be inferred from
the location of the cursor when viewing the desktop list, or will
be asked of the user (with completion) when the desktop list is
not currently shown.  NEW is always asked of the user.

\(fn OLD NEW)" t nil)

(autoload 'desktop-registry-change-desktop "desktop-registry" "\
Change to the desktop named NAME.

If this command is called interactively NAME will be inferred
from the location of the cursor when viewing the desktop list, or
will be asked of the user (with completion) when the desktop list
is not currently shown.

This function just calls `desktop-change-dir' with the directory
attached to NAME.

\(fn NAME)" t nil)

(defvar desktop-registry-auto-register nil "\
Non-nil if Desktop-Registry-Auto-Register mode is enabled.
See the command `desktop-registry-auto-register' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `desktop-registry-auto-register'.")

(custom-autoload 'desktop-registry-auto-register "desktop-registry" nil)

(autoload 'desktop-registry-auto-register "desktop-registry" "\
Automatically add saved desktops to the registry.

Enabling this global minor mode will add
`desktop-registry-add-current-desktop' as a hook to
`desktop-save-hook'.

\(fn &optional ARG)" t nil)

(autoload 'desktop-registry-list-desktops "desktop-registry" "\
Display a list of registered desktops.

Most functions that are available as interactive commands
elsewhere are also specialized to work here.  For example:
`desktop-registry-change-desktop' will open the desktop under the
user's cursor when called from this list.

The way the buffer is shown can be customized by specifying a
function to use in
`desktop-registry-list-switch-buffer-function'.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; desktop-registry-autoloads.el ends here
