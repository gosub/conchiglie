;; disable line numbers in org-mode, too distracting
(defun gg/org-mode-hook ()
  (display-line-numbers-mode 0))

(add-hook 'org-mode-hook 'gg/org-mode-hook)

(setq org-todo-keywords
      '((sequence "TODO" "ASIDE" "DONE")))

(setq org-todo-keyword-faces
      '(("ASIDE" . "dark blue")))

(provide 'gg-org)
