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
		'gg/search-and-copy-line)

;; toggle vim in local buffer
(global-set-key (kbd "<f7>")
		'evil-local-mode)

;; magit-status enabled globally
(global-set-key (kbd "C-x g")
		'magit-status)

;; missing ctrl-A from vim
(global-set-key (kbd "C-c +")
		'gg/increment-number-at-point)


(provide 'gg-keybindings)