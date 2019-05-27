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

;; necessary if you want to load customized values

; (when (file-exists-p custom-file)
;   (load custom-file))

(setq gg-txt-directory
      (if (file-directory-p "~/personal/Dropbox")
	  "~/personal/Dropbox/txt/"
	  "~/Dropbox/txt/"))


(provide 'gg-dirs-and-files)
