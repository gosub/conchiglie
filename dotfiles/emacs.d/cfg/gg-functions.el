(defun gg/insert-current-date (arg)
  "Insert current date in ISO8601 format (YYY-MM-DD)
with single prefix insert with DD/MM/YYYY format
with double prefix insert full date-time ISO8601 string"
  (interactive "P")
  (let ((format-string (cond ((equal arg '(4)) "%d/%m/%Y")
			     ((equal arg '(16)) "%FT%T%:z")
			     (t "%F"))))
    (insert (format-time-string format-string))))


(defun gg/search-forward-and-copy-line ()
  "search forwards and insert whole line found at point"
  (interactive)
  (let ((p (point)))
    (when (not (null (isearch-forward)))
      (move-beginning-of-line nil)
      (kill-line)
      (yank)
      (goto-char p)
      (yank))))


(defun gg/search-backward-and-copy-line ()
  "search backward and insert whole line found at point"
  (interactive)
  (let ((p (point)))
    (when (not (null (isearch-backward)))
      (move-beginning-of-line nil)
      (kill-line)
      (yank)
      (goto-char p)
      (yank))))


(defun gg/increment-number-at-point ()
  "increment number at point, partially simulating C-a in vim"
  (interactive)
  (save-excursion
    (skip-chars-backward "0123456789")
    (or (looking-at "[0123456789]+")
	(error "No number at point"))
    (replace-match (number-to-string 
                      (1+ (string-to-number (match-string 0)))))))


(defun gg/apparecchia ()
  "setup initials buffers and windows as I like them"
  (interactive)
  (eshell)
  (split-window-right)
  (other-window 1)
  (find-file gg-notes-file)
  (find-file gg-todo-file)
  (find-file gg-done-file))


(defun gg/search-on-youtube ()
  "search region text on youtube"
  (interactive)
  (browse-url (concat "https://www.youtube.com/results?search_query="
		      (buffer-substring (mark) (point)))))


(defun gg/search-on-google ()
  "search region text on google"
  (interactive)
  (browse-url (concat "https://www.google.com/search?q="
		      (buffer-substring (mark) (point)))))


(defun gg/search-on-hackernews ()
  "search region text on hacker news"
  (interactive)
  (browse-url (concat "https://hn.algolia.com/?q="
		      (buffer-substring (mark) (point)))))


(defun gg/yank-line-at-beginning-of-buffer ()
  "yank the content of the clipboard at the beginning of the buffer"
  (interactive)
  (beginning-of-buffer)
  (yank)
  (newline))


(provide 'gg-functions)
