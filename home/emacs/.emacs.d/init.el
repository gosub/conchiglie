;;; additional package repositories

(add-to-list 'package-archives
  '("melpa" . "https://melpa.org/packages/"))


;;; personal local packages (gg-lisp subfolder)

(add-to-list 'load-path
  (expand-file-name "gg-lisp" user-emacs-directory))


;;; packages

(use-package ledger-mode
  :ensure t
  :defer t)

(use-package magit
  :ensure t)

(use-package evil
  :ensure t
  :bind ("C-c v" . evil-local-mode))

(use-package move-text
  :ensure t
  :config (move-text-default-bindings))

(use-package org
  :defer t
  :custom
  (org-ellipsis "⤵")
  (org-startup-folded t)
  (org-tags-column 0)
  (org-clock-sound "~/dl/audio/alarm.wav")
  (org-todo-keywords
	'((sequence "TODO" "ASIDE" "DONE")))
  (org-todo-keyword-faces
	'(("ASIDE" . "dark blue")))
  :custom-face
  (org-ellipsis ((t (:underline nil))))
  :hook
  (org-mode . (lambda ()
		"disable line numbers in org-mode, too distracting"
		(display-line-numbers-mode 0)
		(electric-indent-mode -1))))

(use-package gg-sclang-utils)

(use-package sclang
  :defer t
  :after (gg-sclang-utils)
  :commands (sclang-start)
  :custom
  (sclang-show-workspace-on-startup nil)
  (sclang-eval-line-forward nil)
  :mode ("\\.scd\\'" . sclang-mode)
  :bind (:map sclang-mode-map
	      ("C-<return>" . gg/sclang-eval-dwim)
	      ("C-." . sclang-main-stop)
	      ("C-c t" . gg/sclang-transient)))

(use-package ox-hugo
  :ensure t
  :pin melpa
  :after ox)


;;; Dirs and files

;; put autosave and backup files inside .emacs.d/ subdirs

(make-directory "~/.emacs.d/autosaves" :parents)
(make-directory "~/.emacs.d/backups" :parents)
(setq auto-save-file-name-transforms
      '((".*" "~/.emacs.d/autosaves/\\1" t)))
(setq backup-directory-alist
      '((".*". "~/.emacs.d/backups/")))

;; put emacs-customized values in a separate file
;; instead of appending it to init.el
;; necessary since package-list and package-install
;; use it as record of explicitly installed packages

(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))

;; I do not want to use customized values for configuration
;; init.el is the only source of truth

; (when (file-exists-p custom-file)
;   (load custom-file))


; most used files directory
(setq gg-txt-directory "~/box/txt")

; notes file
(setq gg-notes-file
      (expand-file-name "ziba.org" gg-txt-directory))
; todo file
(setq gg-todo-file
      (expand-file-name "todo.org" gg-txt-directory))
; done file
(setq gg-done-file
      (expand-file-name "done.org" gg-txt-directory))

; tidalcycles samples folder
(setq gg-tidal-sample-folder
      (expand-file-name "~/.local/share/SuperCollider/downloaded-quarks/Dirt-Samples"))


;;; Functions

(defun gg/insert-current-date (arg)
  "Insert current date in ISO8601 format (YYY-MM-DD)
with single prefix insert full date-time ISO8601 string"
  (interactive "p")
  (let* ((fmts '("%F" "%FT%T%:z"))
	(fmt (cl-case arg
	       (4 (cadr fmts))
	       (t (car fmts)))))
    (insert (format-time-string fmt))))


(defun gg/search-forward-and-copy-line ()
  "search forwards and insert whole line found at point"
  (interactive)
  (let ((p (point)))
    (when (not (null (isearch-forward)))
      (move-beginning-of-line nil)
      (kill-line)
      (yank)
      (goto-char p)
      (yank))))


(defun gg/search-backward-and-copy-line ()
  "search backward and insert whole line found at point"
  (interactive)
  (let ((p (point)))
    (when (not (null (isearch-backward)))
      (move-beginning-of-line nil)
      (kill-line)
      (yank)
      (goto-char p)
      (yank))))


(defun gg/increment-number-at-point ()
  "increment number at point, partially simulating C-a in vim"
  (interactive)
  (save-excursion
    (skip-chars-backward "0123456789")
    (or (looking-at "[0123456789]+")
	(error "No number at point"))
    (replace-match (number-to-string 
                      (1+ (string-to-number (match-string 0)))))))


(defun gg/apparecchia ()
  "setup initials buffers and windows as I like them"
  (interactive)
  (eshell)
  (split-window-right)
  (other-window 1)
  (find-file gg-notes-file)
  (find-file gg-todo-file)
  (find-file gg-done-file))


(defun gg/search-on-youtube ()
  "search region text on youtube"
  (interactive)
  (browse-url (concat "https://www.youtube.com/results?search_query="
		      (buffer-substring (mark) (point)))))


(defun gg/search-on-google ()
  "search region text on google"
  (interactive)
  (browse-url (concat "https://www.google.com/search?q="
		      (buffer-substring (mark) (point)))))


(defun gg/search-on-hackernews ()
  "search region text on hacker news"
  (interactive)
  (browse-url (concat "https://hn.algolia.com/?q="
		      (buffer-substring (mark) (point)))))


(defun gg/yank-line-at-beginning-of-buffer ()
  "yank the content of the clipboard at the beginning of the buffer"
  (interactive)
  (beginning-of-buffer)
  (yank)
  (newline))


(defun gg/yt-playlist-to-org (playlist-url)
  "turn a youtube playlist link into an org section, where each video is a subsection"
  (interactive "sPlaylist url or id: ")
  (let*
      ((header-cmd (concat "youtube-dl"
			   " --ignore-errors --get-filename "
			   " --output '* [/] [[https://www.youtube.com/playlist?list=%(playlist_id)s][%(uploader)s - %(playlist_title)s]]' "
			   " --playlist-end 1 "))
       (playlist-header (shell-command-to-string (concat header-cmd playlist-url)))
       (entries-cmd (concat "youtube-dl"
			    " --ignore-errors --get-filename "
			    " --output '- [ ] [[https://www.youtube.com/watch?v=%(id)s][%(title)s]]' "))
       (playlist-entries (shell-command-to-string (concat entries-cmd playlist-url))))
    (insert (concat "\n" playlist-header playlist-entries "\n"))))


(defun gg/switch-csv-and-org-link (string)
  (cond ((string-match "^....-..-..,\"?\\([^\"]*\\)\"?,\\(.*\\)$" string)
	 (concat "[[" (match-string 2 string)
		 "][" (match-string 1 string) "]]"))
	((string-match "\\[\\[\\(.*\\)\\]\\[\\(.*\\)\\]\\]" string)
	 (concat (format-time-string "%Y-%m-%d") ",\""
	    (match-string 2 string) "\","
	    (match-string 1 string)))))


(defun gg/copy-line-at-point-with-switched-link-format ()
  (interactive)
  (let ((switched-line-at-point
	 (gg/switch-csv-and-org-link (thing-at-point 'line t))))
   (when switched-line-at-point (kill-new switched-line-at-point))))


(defun gg/random-emacs-info-node ()
  "Open a random emacs manual Info node in a side window."
  (interactive)
  (let* ((all-nodes (mapcar #'car (Info-toc-nodes "emacs")))
	 (nodes-count (length all-nodes))
	 (random-node (nth (random nodes-count) all-nodes)))
    (info-other-window (concat "(emacs)" random-node))))


(defun gg/describe-random-interactive-function ()
  "Display the documentation of a random interactive function."
  (interactive)
  (let (all-the-funs)
    (mapatoms (lambda (sym)
		(when (and (symbol-function sym)
			   (commandp (symbol-function sym)))
		  (push sym all-the-funs))))
    (describe-function
     (nth (random (length all-the-funs))
	  all-the-funs))))


(defun gg/calculate-xkcd-geohashing-coords (latitude longitude)
  "Calculate the coordinates of today's XKCD geohashing game.
LATITUDE and LONGITUDE must be integers.
The Dow Jones opening value is downloaded from URL `http://geo.crox.net/djia/'.
The algorithm was invented for URL `https://xkcd.com/426/'.
See URL `https://geohashing.site/' for additional info."
  (let*
      ((djia-url (concat "http://geo.crox.net/djia/"
			 (format-time-string "%Y/%m/%d")))
       (djia (with-temp-buffer
	       (url-insert-file-contents djia-url)
	       (buffer-string)))
       (daily-pre-hash (concat
			(format-time-string "%Y-%m-%d")
			"-" djia))
       (daily-hash (md5 daily-pre-hash))
       (lat-frac-hex (substring daily-hash 0 16))
       (lon-frac-hex (substring daily-hash 16))
       (lat-frac (calc-eval (concat "16#0." lat-frac-hex)))
       (lon-frac (calc-eval (concat "16#0." lon-frac-hex)))
       (lat (+ latitude (string-to-number lat-frac)))
       (lon (+ longitude (string-to-number lon-frac))))
    (list lat lon)))


(defun gg/xkcd-geohashing-coords ()
  "Print the coordinates of today's XKCD geohashing game."
  (interactive)
  (let* ((lat (truncate (if (boundp 'calendar-latitude)
			    calendar-latitude
			  (read-number "Latitude: " 41))))
	 (lon (truncate (if (boundp 'calendar-longitude)
			    calendar-longitude
			  (read-number "Latitude: " 12))))
	 (coords (gg/calculate-xkcd-geohashing-coords lat lon)))
    (message
     (format "LAT %.6f, LON %.6f" (car coords) (cadr coords)))))


(defun gg/browse-xkcd-geohashing-map ()
  "Open a web map URL of today's XKCD geohashing game coordinates."
  (interactive)
  (let* ((lat (truncate (if (boundp 'calendar-latitude)
			    calendar-latitude
			  (read-number "Latitude: " 41))))
	 (lon (truncate (if (boundp 'calendar-longitude)
			    calendar-longitude
			  (read-number "Latitude: " 12))))
	 (coords (gg/calculate-xkcd-geohashing-coords lat lon))
	 (url-fmt "https://www.openstreetmap.org/?mlat=%.6f&mlon=%.6f&zoom=10")
	 (url (format url-fmt (car coords) (cadr coords))))
    (browse-url url)))


(defun gg/get-string-from-file (filePath)
  "Return file content as string."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))


(defun gg/get-computer-model ()
  (interactive)
  (let ((dmi-product-family-file "/sys/devices/virtual/dmi/id/product_family"))
    (if (file-readable-p dmi-product-family-file)
	(string-trim (gg/get-string-from-file dmi-product-family-file))
      "unknown")))


(defun gg/is-computer-model? (model)
  (interactive)
  (string= model (gg/get-computer-model)))



;;; UX

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
(setq epg-pinentry-mode 'loopback)

;; launch http url in chromium incognito
(setq browse-url-browser-function
      (quote browse-url-generic))
(setq browse-url-generic-args
      (quote ("--private-window")))
(setq browse-url-generic-program
      "firefox")



;;; Visuals

;; set default font and size
(set-face-attribute
 'default nil :family "Source Code Pro" :height 130)

;; do not show splash screen
(setq inhibit-startup-screen 1)

;; remove toolbar
(tool-bar-mode -1)

;; remove menu bar
(menu-bar-mode -1)

;; scrollbar on the right
(set-scroll-bar-mode 'right)

;; visible bell
(setq visible-bell t)

;; show line number on the side
(global-display-line-numbers-mode)
(setq linum-format "%3d")

;; column number in info bar
(column-number-mode 1)

;; *scratch* default text

(setq initial-scratch-message "\
;;                    | |     | |    \n\
;;  ___  ___ _ __ __ _| |_ ___| |__  \n\
;; / __|/ __| '__/ _` | __/ __| '_ \\ \n\
;; \\__ \\ (__| | | (_| | || (__| | | |\n\
;; |___/\\___|_|  \\__,_|\\__\\___|_| |_|\n\n")




;;; Unicode

; additional superscript and subscript
; unicode characters with C-x 8

(defun more-ctl-x-8-superscript ()
  "Add more unicode superscript characters to C-x 8."
  (define-key 'iso-transl-ctl-x-8-map "^0" [#x2070])
  ;; the next three keys are already defined in emacs
  ;; (define-key 'iso-transl-ctl-x-8-map "^1" [#x00B9])
  ;; (define-key 'iso-transl-ctl-x-8-map "^2" [#x00B2])
  ;; (define-key 'iso-transl-ctl-x-8-map "^3" [#x00B3])
  (define-key 'iso-transl-ctl-x-8-map "^4" [#x2074])
  (define-key 'iso-transl-ctl-x-8-map "^5" [#x2075])
  (define-key 'iso-transl-ctl-x-8-map "^6" [#x2076])
  (define-key 'iso-transl-ctl-x-8-map "^7" [#x2077])
  (define-key 'iso-transl-ctl-x-8-map "^8" [#x2078])
  (define-key 'iso-transl-ctl-x-8-map "^9" [#x2079])

  (define-key 'iso-transl-ctl-x-8-map "^+" [#x207A])
  (define-key 'iso-transl-ctl-x-8-map "^-" [#x207B])
  (define-key 'iso-transl-ctl-x-8-map "^=" [#x207C])
  (define-key 'iso-transl-ctl-x-8-map "^(" [#x207D])
  (define-key 'iso-transl-ctl-x-8-map "^)" [#x207E])
  (define-key 'iso-transl-ctl-x-8-map "^n" [#x207F])
  (define-key 'iso-transl-ctl-x-8-map "^i" [#x2071]))

(defun more-ctl-x-8-subscript ()
  "Add more unicode subscript characters to C-x 8."
  (define-key 'iso-transl-ctl-x-8-map "_0" [#x2080])
  (define-key 'iso-transl-ctl-x-8-map "_1" [#x2081])
  (define-key 'iso-transl-ctl-x-8-map "_2" [#x2082])
  (define-key 'iso-transl-ctl-x-8-map "_3" [#x2083])
  (define-key 'iso-transl-ctl-x-8-map "_4" [#x2084])
  (define-key 'iso-transl-ctl-x-8-map "_5" [#x2085])
  (define-key 'iso-transl-ctl-x-8-map "_6" [#x2086])
  (define-key 'iso-transl-ctl-x-8-map "_7" [#x2087])
  (define-key 'iso-transl-ctl-x-8-map "_8" [#x2088])
  (define-key 'iso-transl-ctl-x-8-map "_9" [#x2089])

  (define-key 'iso-transl-ctl-x-8-map "_+" [#x208A])
  (define-key 'iso-transl-ctl-x-8-map "_-" [#x208B])
  (define-key 'iso-transl-ctl-x-8-map "_=" [#x208C])
  (define-key 'iso-transl-ctl-x-8-map "_(" [#x208D])
  (define-key 'iso-transl-ctl-x-8-map "_)" [#x208E])
  (define-key 'iso-transl-ctl-x-8-map "_n" [#x2099]))

(more-ctl-x-8-superscript)
(more-ctl-x-8-subscript)



;;; Erlang

(if (file-directory-p "/usr/lib/erlang")
  (let* ((lib-dir "/usr/lib/erlang/lib/")
         (tools-dir (file-name-completion "tools" lib-dir))
         (erlang-mode-dir (concat lib-dir tools-dir "emacs")))
    (add-to-list 'load-path erlang-mode-dir)
    (add-to-list 'exec-path "/usr/lib/erlang/bin")
    (setq erlang-root-dir "/usr/lib/erlang")
    (require 'erlang-start)))

; printable char set is unicode
(setq inferior-erlang-machine-options
      '("+pc" "unicode"))



;;; Keybindings

;; save bufer with F2, like in GWBASIC
(global-set-key (kbd "<f2>")
		'save-buffer)

;; keybinding to most used file
(global-set-key (kbd "<f5>")
		'gg/apparecchia)

;; insert current date in iso format
(global-set-key (kbd "C-c d")
		'gg/insert-current-date)

;; search and duplicate whole line
(global-set-key (kbd "C-c s")
		'gg/search-forward-and-copy-line)
(global-set-key (kbd "C-c r")
		'gg/search-backward-and-copy-line)

;; missing ctrl-A from vim
(global-set-key (kbd "C-c +")
		'gg/increment-number-at-point)

;; kill buffer without confirmation
(global-set-key (kbd "C-x k")
		'kill-this-buffer)

;; toggle line truncation on/off
(global-set-key (kbd "C-c w")
		'visual-line-mode)

;; yank at beginning of buffer
(global-set-key (kbd "C-c y")
		'gg/yank-line-at-beginning-of-buffer)

;; copy line at point in kill ring
;; with format switched between csv and org link
(global-set-key (kbd "C-c u")
		'gg/copy-line-at-point-with-switched-link-format)

;; on my new Lenovo E14 gen5
;; Fn key is interpreded as <WakeUp>
;; so we ignore it
(when (gg/is-computer-model? "ThinkPad E14 Gen 5")
  (global-set-key (kbd "<WakeUp>") 'ignore))



;;; Tidalcycles

(defun gg/tidal-setup ()
  "setup my tidal environment, starting processes and opening files, prompting with ido-complete"
  (interactive)
  (let*
      ((tidal-setup-steps-alist '((jack   . (lambda () (async-shell-command "jackd -d alsa" "*jack-for-tidal*")))
				  (sclang . (lambda () (async-shell-command "cd ~/Dropbox/prj/2022/tidal && make && sclang autogen_superdirt_startup.scd" "*sclang-for-tidal*")))
				  (file   . (lambda () (find-file "~/Dropbox/prj/2022/tidal/scratch.tidal")))
				  (hask   . (lambda () (tidal-start-haskell)))))
       (steps-strings           (mapcar 'symbol-name (mapcar 'car tidal-setup-steps-alist)))
       (numbered-steps          (cl-mapcar (lambda (step num) (concat (number-to-string num) "-" step))
					   steps-strings (number-sequence 1 (length steps-strings))))
       (numbered-alist          (cl-pairlis numbered-steps (mapcar 'cdr tidal-setup-steps-alist)))
       (selected-step-string    (ido-completing-read "tidal setup step: " numbered-steps))
       (selected-step-function  (alist-get selected-step-string numbered-alist)))
    (funcall selected-step-function)))


(defun gg/tidal-random-sample ()
  "print the name of random tidal sample from the SuperDirt sample folder"
  (interactive)
  (let* ((dir (directory-files gg-tidal-sample-folder))
	 (sample (nth (random (length dir)) dir)))
    (message "random tidal sample: %s" sample)))


(defun gg/tidal-hush ()
  "send hush as a single line, stopping all streams"
  (interactive)
  (tidal-send-string ":{")
  (tidal-send-string "hush")
  (tidal-send-string ":}"))


(defun gg/tidal-mode-map ()
  "set personal keybidings for tidal-mode
C-.        stop all streams, like in SuperCollider IDE
C-c C-a    print a random sample from the SuperDirt folder"
  (define-key tidal-mode-map (kbd "C-.") 'gg/tidal-hush)
  (define-key tidal-mode-map (kbd "C-c C-a") 'gg/tidal-random-sample))

(add-hook 'tidal-mode-hook 'gg/tidal-mode-map)

;; redefine path for BootTidal.el, not installed via cabal
(require 'subr-x)
(setq tidal-boot-script-path
  (let ((filepath
         (cond
          ((string-equal system-type "windows-nt")
           '(("path" . "echo off && for /f %a in ('ghc-pkg latest tidal') do (for /f \"tokens=2\" %i in ('ghc-pkg describe %a ^| findstr data-dir') do (echo %i))")
             ("separator" . "\\")
             ))
          ((or (string-equal system-type "darwin") (string-equal system-type "gnu/linux"))
           '(("path" . "ghc-pkg field tidal data-dir")
             ("separator" . "/")
             ))
          )
         ))
    (concat
     (string-trim (cadr (split-string
                         (shell-command-to-string (cdr (assoc "path" filepath))) ":")))
     (cdr (assoc "separator" filepath))
     "BootTidal.hs")))


;;; CPP

(defun gg/c++-mode-hook ()
  "in c++-mode use tabs for indentation"
  (setq
   tab-width 4
   c-basic-offset 4
   backward-delete-char-untabify-method nil))


(add-hook 'c++-mode-hook 'gg/c++-mode-hook)
