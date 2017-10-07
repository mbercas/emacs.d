;;; vhdl-tools-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "vhdl-tools" "vhdl-tools.el" (22986 6270 545153
;;;;;;  904000))
;;; Generated autoloads from vhdl-tools.el

(autoload 'vhdl-tools-beautify-region "vhdl-tools" "\
Call beautify-region but auto activate region first.
With a prefix ARG, fall back to previous behaviour.

\(fn ARG)" t nil)

(autoload 'vhdl-tools-get-buffer "vhdl-tools" "\
Return buffer where ENTITY-OR-PACKAGE-NAME is found.

\(fn ENTITY-OR-PACKAGE-NAME)" nil nil)

(autoload 'vhdl-tools-jump-into-module "vhdl-tools" "\
When point is at an instance, jump into the module.
Additionally, move point to signal at point.
Declare a key-bind to get back to the original point.

\(fn)" t nil)

(autoload 'vhdl-tools-jump-first "vhdl-tools" "\
Jump to first occurrence of symbol at point.
When no symbol at point, move point to indentation.

\(fn)" t nil)

(autoload 'vhdl-tools-jump-upper "vhdl-tools" "\
Get to upper level module and move point to signal at point.
When no symbol at point, move point to indentation.

\(fn)" t nil)

(autoload 'vhdl-tools-smcn-next "vhdl-tools" "\


\(fn)" t nil)

(autoload 'vhdl-tools-vorg-smcn-next "vhdl-tools" "\


\(fn)" t nil)

(autoload 'vhdl-tools-smcn-prev "vhdl-tools" "\


\(fn)" t nil)

(autoload 'vhdl-tools-vorg-smcn-prev "vhdl-tools" "\


\(fn)" t nil)

(autoload 'vhdl-tools-vorg-jump-to-vorg "vhdl-tools" "\
From `vhdl' file, jump to same line in `vorg' file.

\(fn)" t nil)

(autoload 'vhdl-tools-vorg-jump-from-vorg "vhdl-tools" "\
From `vorg' file, jump to same line in `vhdl' file, tangling the
code before if necessary.

\(fn)" t nil)

(autoload 'vhdl-tools-vorg-jump-from-vorg-into-module "vhdl-tools" "\
From `vorg' file, jump to same line in `vhdl' file, tangling the
code before if necessary, then jump into module.

\(fn)" t nil)

(autoload 'vhdl-tools-vorg-tangle "vhdl-tools" "\
Tangle a `vorg' `ORGFILE' file to its corresponding `vhdl' file.

\(fn ORGFILE)" t nil)

(autoload 'vhdl-tools-vorg-tangle-all "vhdl-tools" "\
Tangle all `vorg' files in current dir to its corresponding `vhdl' file.

\(fn)" t nil)

(autoload 'vhdl-tools-vorg-src-block-beautify "vhdl-tools" "\
Beautify of source code block at point.

\(fn)" t nil)

(autoload 'vhdl-tools-store-link "vhdl-tools" "\
Store current line as a link.

\(fn)" t nil)

(autoload 'vhdl-tools-paste-link "vhdl-tools" "\
Paste previous stored link.

\(fn)" t nil)

(autoload 'vhdl-tools-follow-links "vhdl-tools" "\
Follow links in the form of Tag:ToSearch'.

\(fn ARG)" t nil)

(autoload 'vhdl-tools-headings-next "vhdl-tools" "\
Get to next heading.

\(fn)" t nil)

(autoload 'vhdl-tools-vorg-headings-next "vhdl-tools" "\
Get to next heading in vorg buffer.

\(fn)" t nil)

(autoload 'vhdl-tools-headings-prev "vhdl-tools" "\
Get to previous heading.

\(fn)" t nil)

(autoload 'vhdl-tools-vorg-headings-prev "vhdl-tools" "\
Get to next heading in vorg buffer.

\(fn)" t nil)

(autoload 'vhdl-tools-imenu "vhdl-tools" "\


\(fn)" t nil)

(autoload 'vhdl-tools-imenu-instance "vhdl-tools" "\


\(fn)" t nil)

(autoload 'vhdl-tools-imenu-processes "vhdl-tools" "\


\(fn)" t nil)

(autoload 'vhdl-tools-imenu-component "vhdl-tools" "\


\(fn)" t nil)

(autoload 'vhdl-tools-imenu-headers "vhdl-tools" "\


\(fn)" t nil)

(autoload 'vhdl-tools-outshine-imenu-headers "vhdl-tools" "\


\(fn ARG)" t nil)

(autoload 'vhdl-tools-imenu-all "vhdl-tools" "\
In a vhdl buffer, call `helm-semantic-or-imenu', show all items.
  Processes, instances and doc headers are shown in order of appearance.

\(fn)" t nil)

(autoload 'vhdl-tools-mode "vhdl-tools" "\
Utilities for navigating vhdl sources.

Key bindings:
\\{vhdl-tools-map}

\(fn &optional ARG)" t nil)

(autoload 'vhdl-tools-vorg-mode "vhdl-tools" "\
Utilities for navigating vhdl sources in vorg files.

Key bindings:
\\{vhdl-tools-map}

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; vhdl-tools-autoloads.el ends here
