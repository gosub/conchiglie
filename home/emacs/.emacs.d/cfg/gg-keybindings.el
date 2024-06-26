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

(provide 'gg-keybindings)
