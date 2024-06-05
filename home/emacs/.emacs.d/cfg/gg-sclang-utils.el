(defun gg/sclang-eval-dwim ()
  "If a region is active, eval it. If the cursor is between ^($ and ^)$, mark the text and run sclang-eval-region. Otherwise, run sclang-eval-line."
  (interactive)
  (cond
   ;; if region is active, eval it
   ((use-region-p)
    (gg--sclang-eval-region))

   ;; if there is a parenthesized section that starts before the point
   ;; when the point is inside of it, eval it
   ;; when the point is outside of it, eval the current line
   ((gg--search-parenthesized-region-backward)
    (let* ((pos (point))
	   (reg (gg--search-parenthesized-region-backward))
	   (start (car reg))
	   (end (cadr reg)))
      (if (and (<= start pos) (<= pos end))
	  (save-excursion
            (goto-char start)
            (set-mark (point))
            (goto-char end)
	    (gg--sclang-eval-region))
	(gg--sclang-eval-line))))

   ;; base case: eval the current line
   (t
    (gg--sclang-eval-line))))


(defun gg--sclang-eval-region ()
  (pulse-momentary-highlight-region
   (region-beginning) (region-end))
  (sclang-eval-region)
  (deactivate-mark))


(defun gg--sclang-eval-line ()
  (pulse-momentary-highlight-one-line (point))
  (sclang-eval-line))


(defun gg--search-parenthesized-region-backward ()
  (save-excursion
    (let (start end)
      (if (or (looking-at "^($")
	      (re-search-backward "^($" nil t))
	  (setq start (point)))
      (if start
	  (progn
	    (goto-char start)
	    (if (re-search-forward "^);?$" nil t)
		(setq end (point)))))
      (if (and start end)
	  (list start end)
	nil))))


(transient-define-prefix gg/sclang-transient ()
  "Transient for sclang-mode common options."
  [["Control"
    ("s" "Start SClang" sclang-start :transient t)
    ("q" "Stop SCLang" sclang-stop)
    ("k" "Kill SCLang" sclang-kill)
    ("l" "Recompile classes"  sclang-recompile)]
   ["Server"
    ("b" "Boot server" sclang-server-boot)
    ("t" "Quit server" sclang-server-quit)]
   ["Navigation"
    ("w" "Switch to Workspace" sclang-switch-to-workspace)
    ("p" "Show Post" sclang-show-post-buffer)
    ("c" "Clear Post" sclang-clear-post-buffer)]
   ["Help"
    ("h" "Help GUI" sclang-open-help-gui)
    ("f" "Find help in GUI" sclang-find-help-in-gui)]
   ["Record"
    ("rn" "Prepare for record" sclang-server-prepare-for-record)
    ("rr" "Start recording" sclang-server-record)
    ("rs" "Stop recording" sclang-server-stop-recording)
    ("rp" "Pause recording" sclang-server-pause-recording)]])


(provide 'gg-sclang-utils)
