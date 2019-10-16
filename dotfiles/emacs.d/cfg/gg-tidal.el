;; print a random sample from SuperDirt sample folder
(defun gg/tidal-random-sample ()
  (interactive)
  (let* ((dir (directory-files gg-tidal-sample-folder))
	 (sample (nth (random (length dir)) dir)))
    (message  "random tidal sample: %s" sample)))

;; stop all streams
(defun gg/tidal-hush ()
  "send hush as a single line"
  (interactive)
  (tidal-send-string ":{")
  (tidal-send-string "hush")
  (tidal-send-string ":}"))

;; personal tidal-mode keybindings
(defun gg/tidal-mode-map ()
  (define-key tidal-mode-map (kbd "C-.") 'gg/tidal-hush)
  (define-key tidal-mode-map (kbd "C-c C-a") 'gg/tidal-random-sample))

(add-hook 'tidal-mode-hook 'gg/tidal-mode-map)

(provide 'gg-tidal)
