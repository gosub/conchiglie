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
(setq gg-txt-directory
      (if (file-directory-p "~/personal/Dropbox")
	  "~/personal/Dropbox/txt/"
	  "~/Dropbox/txt/"))
; notes file
(setq gg-notes-file
      (expand-file-name "notes.org" gg-txt-directory))
; todo file
(setq gg-todo-file
      (expand-file-name "todo.org" gg-txt-directory))
; done file
(setq gg-done-file
      (expand-file-name "done.org" gg-txt-directory))


(provide 'gg-dirs-and-files)
