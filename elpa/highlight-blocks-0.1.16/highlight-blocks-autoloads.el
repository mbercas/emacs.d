;;; highlight-blocks-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "highlight-blocks" "highlight-blocks.el" (22334
;;;;;;  47669 690386 611000))
;;; Generated autoloads from highlight-blocks.el

(autoload 'highlight-blocks-mode "highlight-blocks" "\
Highlight the nested blocks the point is currently in.

Toggle Highlight Blocks on or off.

With a prefix argument ARG, enable Highlight Blocks mode if ARG is
positive, and disable it otherwise. If called from Lisp, enable the
mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.

\(fn &optional ARG)" t nil)

(autoload 'highlight-blocks-now "highlight-blocks" "\
Highlight the nested blocks the point is in for `highlight-blocks-now-time'
seconds, or until input is available.
When called with an prefix argument, its value determines how many of the
innermost blocks will be highlighted; when called with no argument, the value
`highlight-blocks-max-innermost-block-count' is used, which see.

\(fn &optional HOW-MANY)" t nil)

;;;***

;;;### (autoloads nil nil ("highlight-blocks-pkg.el") (22334 47669
;;;;;;  705581 535000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; highlight-blocks-autoloads.el ends here
