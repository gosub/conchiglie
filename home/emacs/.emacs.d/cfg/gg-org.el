(defun gg/org-mode-hook ()
  "disable line numbers in org-mode, too distracting"
  (display-line-numbers-mode 0)
  (electric-indent-mode -1))

(add-hook 'org-mode-hook 'gg/org-mode-hook)

(setq org-todo-keywords
      '((sequence "TODO" "ASIDE" "DONE")))

(setq org-todo-keyword-faces
      '(("ASIDE" . "dark blue")))

(setq org-startup-folded t)

(setq org-clock-sound "~/dl/audio/alarm.wav")

(provide 'gg-org)
