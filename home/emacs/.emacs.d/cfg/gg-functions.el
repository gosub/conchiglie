(defun gg/insert-current-date (arg)
  "Insert current date in ISO8601 format (YYY-MM-DD)
with single prefix insert full date-time ISO8601 string"
  (interactive "p")
  (let* ((fmts '("%F" "%FT%T%:z"))
	(fmt (cl-case arg
	       (4 (cadr fmts))
	       (t (car fmts)))))
    (insert (format-time-string fmt))))


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


(defun gg/copy-line-at-point-with-switched-link-format ()
  (interactive)
  (let ((switched-line-at-point
	 (gg/switch-csv-and-org-link (thing-at-point 'line t))))
   (when switched-line-at-point (kill-new switched-line-at-point))))


(defun gg/random-emacs-info-node ()
  "Open a random emacs manual Info node in a side window."
  (interactive)
  (let* ((all-nodes (mapcar #'car (Info-toc-nodes "emacs")))
	 (nodes-count (length all-nodes))
	 (random-node (nth (random nodes-count) all-nodes)))
    (info-other-window (concat "(emacs)" random-node))))


(defun gg/describe-random-interactive-function ()
  "Display the documentation of a random interactive function."
  (interactive)
  (let (all-the-funs)
    (mapatoms (lambda (sym)
		(when (and (symbol-function sym)
			   (commandp (symbol-function sym)))
		  (push sym all-the-funs))))
    (describe-function
     (nth (random (length all-the-funs))
	  all-the-funs))))


(defun gg/calculate-xkcd-geohashing-coords (latitude longitude)
  "Calculate the coordinates of today's XKCD geohashing game.
LATITUDE and LONGITUDE must be integers.
The Dow Jones opening value is downloaded from URL `http://geo.crox.net/djia/'.
The algorithm was invented for URL `https://xkcd.com/426/'.
See URL `https://geohashing.site/' for additional info."
  (let*
      ((djia-url (concat "http://geo.crox.net/djia/"
			 (format-time-string "%Y/%m/%d")))
       (djia (with-temp-buffer
	       (url-insert-file-contents djia-url)
	       (buffer-string)))
       (daily-pre-hash (concat
			(format-time-string "%Y-%m-%d")
			"-" djia))
       (daily-hash (md5 daily-pre-hash))
       (lat-frac-hex (substring daily-hash 0 16))
       (lon-frac-hex (substring daily-hash 16))
       (lat-frac (calc-eval (concat "16#0." lat-frac-hex)))
       (lon-frac (calc-eval (concat "16#0." lon-frac-hex)))
       (lat (+ latitude (string-to-number lat-frac)))
       (lon (+ longitude (string-to-number lon-frac))))
    (list lat lon)))


(defun gg/xkcd-geohashing-coords ()
  "Print the coordinates of today's XKCD geohashing game."
  (interactive)
  (let* ((lat (truncate (if (boundp 'calendar-latitude)
			    calendar-latitude
			  (read-number "Latitude: " 41))))
	 (lon (truncate (if (boundp 'calendar-longitude)
			    calendar-longitude
			  (read-number "Latitude: " 12))))
	 (coords (gg/calculate-xkcd-geohashing-coords lat lon)))
    (message
     (format "LAT %.6f, LON %.6f" (car coords) (cadr coords)))))


(defun gg/browse-xkcd-geohashing-map ()
  "Open a web map URL of today's XKCD geohashing game coordinates."
  (interactive)
  (let* ((lat (truncate (if (boundp 'calendar-latitude)
			    calendar-latitude
			  (read-number "Latitude: " 41))))
	 (lon (truncate (if (boundp 'calendar-longitude)
			    calendar-longitude
			  (read-number "Latitude: " 12))))
	 (coords (gg/calculate-xkcd-geohashing-coords lat lon))
	 (url-fmt "https://www.openstreetmap.org/?mlat=%.6f&mlon=%.6f&zoom=10")
	 (url (format url-fmt (car coords) (cadr coords))))
    (browse-url url)))


(defun gg/get-string-from-file (filePath)
  "Return file content as string."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))


(defun gg/get-computer-model ()
  (interactive)
  (let ((dmi-product-family-file "/sys/devices/virtual/dmi/id/product_family"))
    (if (file-readable-p dmi-product-family-file)
	(string-trim (gg/get-string-from-file dmi-product-family-file))
      "unknown")))


(defun gg/is-computer-model? (model)
  (interactive)
  (string= model (gg/get-computer-model)))


(provide 'gg-functions)
