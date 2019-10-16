;; insert current date in iso format - function
(defun gg/insert-current-date (arg)
  (interactive "P")
  (if (equal arg '(4)) ;; universal argument
      (insert
       (format-time-string "%d/%m/%Y" (current-time)))
    (insert
     (format-time-string "%Y-%m-%d" (current-time)))))

;; search and duplicate whole line - function
(defun gg/search-and-copy-line ()
  (interactive)
  (let ((p (point)))
    (when (not (null (isearch-forward)))
      (move-beginning-of-line nil)
      (kill-line)
      (yank)
      (goto-char p)
      (yank))))

;; partially simulates C-a (increment number at point) from VIM
(defun gg/increment-number-at-point ()
  (interactive)
  (save-excursion
    (skip-chars-backward "0123456789")
    (or (looking-at "[0123456789]+")
	(error "No number at point"))
    (replace-match (number-to-string 
                      (1+ (string-to-number (match-string 0)))))))

;; prepare my homepage
(defun gg/apparecchia ()
  (interactive)
  (eshell)
  (split-window-right)
  (other-window 1)
  (find-file gg-notes-file)
  (find-file gg-todo-file)
  (find-file gg-done-file))


(provide 'gg-functions)
