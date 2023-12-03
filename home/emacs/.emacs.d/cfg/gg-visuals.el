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


(provide 'gg-visuals)
