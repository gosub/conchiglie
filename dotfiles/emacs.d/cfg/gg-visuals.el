;; set default font and size
(set-face-attribute
 'default nil :family "DejaVu Sans Mono" :height 90)

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


(provide 'gg-visuals)
