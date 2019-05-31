;; disable line numbers in org-mode, too distracting
(defun gg/org-mode-hook ()
  (display-line-numbers-mode 0))

(add-hook 'org-mode-hook 'gg/org-mode-hook)


(provide 'gg-org)
