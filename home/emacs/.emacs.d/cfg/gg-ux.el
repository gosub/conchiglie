;; type y instead of yes
(defalias 'yes-or-no-p 'y-or-n-p)

;; set fill-column (alt-q) to 80 char per lines
(setq-default fill-column 80)

;; show matching parens
(show-paren-mode 1)

;; middle mouse click yank at text cursor
;; not al mouse cursor position
(setq mouse-yank-at-point t)

;; re-enable 'a' in dired
(put 'dired-find-alternate-file 'disabled nil)

;; use loopback mode in gpg (epa) pin-entry
;; so it's emacs that asks the password
(setq epa-pinentry-mode 'loopback)

;; launch http url in chromium incognito
(setq browse-url-browser-function
      (quote browse-url-generic))
(setq browse-url-generic-args
      (quote ("--incognito")))
(setq browse-url-generic-program
      "chromium")

(provide 'gg-ux)
