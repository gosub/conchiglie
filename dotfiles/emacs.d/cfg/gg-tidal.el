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

;; redefine path for BootTidal.el, not installed via cabal
(require 'subr-x)
(setq tidal-boot-script-path
  (let ((filepath
         (cond
          ((string-equal system-type "windows-nt")
           '(("path" . "echo off && for /f %a in ('ghc-pkg latest tidal') do (for /f \"tokens=2\" %i in ('ghc-pkg describe %a ^| findstr data-dir') do (echo %i))")
             ("separator" . "\\")
             ))
          ((or (string-equal system-type "darwin") (string-equal system-type "gnu/linux"))
           '(("path" . "ghc-pkg field tidal data-dir")
             ("separator" . "/")
             ))
          )
         ))
    (concat
     (string-trim (cadr (split-string
                         (shell-command-to-string (cdr (assoc "path" filepath))) ":")))
     (cdr (assoc "separator" filepath))
     "BootTidal.hs")))


(provide 'gg-tidal)
