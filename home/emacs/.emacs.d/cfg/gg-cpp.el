(defun gg/c++-mode-hook ()
  "in c++-mode use tabs for indentation"
  (setq
   tab-width 4
   c-basic-offset 4
   backward-delete-char-untabify-method nil))


(add-hook 'c++-mode-hook 'gg/c++-mode-hook)


(provide 'gg-cpp)
