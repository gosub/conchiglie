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


(defun gg/yt-playlist-to-org (playlist-url)
  "turn a youtube playlist link into an org section, where each video is a subsection"
  (interactive "sPlaylist url or id: ")
  (let*
      ((header-cmd (concat "youtube-dl"
			   " --ignore-errors --get-filename "
			   " --output '* [/] [[https://www.youtube.com/playlist?list=%(playlist_id)s][%(uploader)s - %(playlist_title)s]]' "
			   " --playlist-end 1 "))
       (playlist-header (shell-command-to-string (concat header-cmd playlist-url)))
       (entries-cmd (concat "youtube-dl"
			    " --ignore-errors --get-filename "
			    " --output '- [ ] [[https://www.youtube.com/watch?v=%(id)s][%(title)s]]' "))
       (playlist-entries (shell-command-to-string (concat entries-cmd playlist-url))))
    (insert (concat "\n" playlist-header playlist-entries "\n"))))


(defun gg/switch-csv-and-org-link (string)
  (cond ((string-match "^....-..-..,\"?\\([^\"]*\\)\"?,\\(.*\\)$" string)
	 (concat "[[" (match-string 2 string)
		 "][" (match-string 1 string) "]]"))
	((string-match "\\[\\[\\(.*\\)\\]\\[\\(.*\\)\\]\\]" string)
	 (concat (format-time-string "%Y-%m-%d") ",\""
	    (match-string 2 string) "\","
	    (match-string 1 string)))))


(provide 'gg-functions)
