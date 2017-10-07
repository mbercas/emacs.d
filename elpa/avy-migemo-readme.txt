This package is a minor mode of avy for using migemo.

Migemo which is a library for incremental search has only Japanese dictionaries.
It could be used as abbreviation matching for other languages
by preparing user's migemo dictionaries or customizing `avy-migemo-get-function'.

For example, if url is defined in a migemo dictionary as ftp, http, and so on,
these words also can be added to avy's candidates.


The following functions are provided:

  + avy-migemo-goto-char
  + avy-migemo-goto-char-2
  + avy-migemo-goto-char-in-line
  + avy-migemo-goto-char-timer
  + avy-migemo-goto-subword-1
  + avy-migemo-goto-word-1
  + avy-migemo-isearch
  + avy-migemo--overlay-at
  + avy-migemo--overlay-at-full
  + avy-migemo--read-string-timer

 These are the same as avy's predefined functions
 except for adding candidates via migemo (simply using migemo instead of `regexp-quote').

The following extensions are available:
  + avy-migemo-e.g.zzz-to-char.el
  + avy-migemo-e.g.swiper.el

Further information is available from:
https://github.com/momomo5717/avy-migemo  (README.org or README.jp.org)

Setup:
(add-to-list 'load-path "/path/to/avy-migemo")
(require 'avy-migemo)
If you override avy's predefined functions using `advice-add',
(avy-migemo-mode 1)

You can toggle `avy-migemo-mode' by M-x avy-migemo-mode.
