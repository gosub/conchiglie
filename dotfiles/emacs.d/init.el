;; package system/marmelade init

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; personal customizations (cfg folder, gg- prefix)

(add-to-list 'load-path
	     (expand-file-name "cfg" user-emacs-directory))

(require 'gg-dirs-and-files)
(require 'gg-visuals)
(require 'gg-unicode)

;; editing tweaks

;; save bufer with F2, like in GWBASIC
(global-set-key (kbd "<f2>") 'save-buffer)

;; type y instead of yes
(defalias 'yes-or-no-p 'y-or-n-p)

;; set fill-column (alt-q) to 80 char per lines
(setq-default fill-column 80)



;; org-mode tweaks

;; make sure org is installed
(unless (package-installed-p 'org)
  (package-refresh-contents)
  (package-install 'org))
  
;; keybinding to most used file

(global-set-key (kbd "<f5>")
		(lambda () 
		  (interactive)
		  (find-file (concat gg-txt-directory "todo.org"))
		  (find-file (concat gg-txt-directory "done.org"))))

;; insert current date in iso format - function
(defun gg-insert-current-date-iso ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d" (current-time))))

;; insert current date in iso format - binding
(global-set-key "\C-cd" 'gg-insert-current-date-iso)

;; search and duplicate whole line - function
(defun gg-search-and-copy-line ()
  (interactive)
  (let ((p (point)))
    (when (not (null (isearch-forward)))
      (move-beginning-of-line nil)
      (kill-line)
      (yank)
      (goto-char p)
      (yank))))

;; search and duplicate whole line - binding
(global-set-key (kbd "C-c s") 'gg-search-and-copy-line)

;; disable linum-mode in org-mode, too slow
(defun gg-nolinum ()
  (linum-mode 0))
(add-hook 'org-mode-hook 'gg-nolinum)



;; evil mode

;; make sure evil is installed
(unless (package-installed-p 'evil)
  (package-refresh-contents)
  (package-install 'evil))

;; toggle vim in local buffer with F7
(require 'evil)
(global-set-key (kbd "<f7>") 'evil-local-mode)


;; partially simulates C-a (increment number at point) from VIM

(defun increment-number-at-point ()
  (interactive)
  (save-excursion
    (skip-chars-backward "0123456789")
    (or (looking-at "[0123456789]+")
	(error "No number at point"))
    (replace-match (number-to-string 
                      (1+ (string-to-number (match-string 0)))))))

(global-set-key (kbd "C-c +") 'increment-number-at-point)

;; added by emacs

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (tidal evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "ADBO" :slant normal :weight normal :height 113 :width normal)))))
