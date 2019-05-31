;; package stuff

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; personal customizations (cfg folder, gg- prefix)

(add-to-list 'load-path
	     (expand-file-name "cfg" user-emacs-directory))

(require 'gg-dirs-and-files)
(require 'gg-functions)
(require 'gg-ux)
(require 'gg-visuals)
(require 'gg-unicode)

;; editing tweaks

;; save bufer with F2, like in GWBASIC
(global-set-key (kbd "<f2>") 'save-buffer)


;; org-mode tweaks

;; make sure org is installed
(unless (package-installed-p 'org)
  (package-refresh-contents)
  (package-install 'org))
  
;; keybinding to most used file



;; insert current date in iso format - binding
(global-set-key "\C-cd" 'gg-insert-current-date-iso)


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



(global-set-key (kbd "C-c +") 'increment-number-at-point)
