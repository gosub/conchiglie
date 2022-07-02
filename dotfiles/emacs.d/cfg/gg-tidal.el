(defun gg/tidal-setup ()
  "setup my tidal environment, starting processes and opening files, prompting with ido-complete"
  (interactive)
  (let*
      ((tidal-setup-steps-alist '((jack   . (lambda () (async-shell-command "jackd -d alsa" "*jack-for-tidal*")))
				  (sclang . (lambda () (async-shell-command "cd ~/Dropbox/prj/2022/tidal && make && sclang autogen_superdirt_startup.scd" "*sclang-for-tidal*")))
				  (file   . (lambda () (find-file "~/Dropbox/prj/2022/tidal/scratch.tidal")))
				  (hask   . (lambda () (tidal-start-haskell)))))
       (steps-strings           (mapcar 'symbol-name (mapcar 'car tidal-setup-steps-alist)))
       (numbered-steps          (cl-mapcar (lambda (step num) (concat (number-to-string num) "-" step))
					   steps-strings (number-sequence 1 (length steps-strings))))
       (numbered-alist          (cl-pairlis numbered-steps (mapcar 'cdr tidal-setup-steps-alist)))
       (selected-step-string    (ido-completing-read "tidal setup step: " numbered-steps))
       (selected-step-function  (alist-get selected-step-string numbered-alist)))
    (funcall selected-step-function)))


(defun gg/tidal-random-sample ()
  "print the name of random tidal sample from the SuperDirt sample folder"
  (interactive)
  (let* ((dir (directory-files gg-tidal-sample-folder))
	 (sample (nth (random (length dir)) dir)))
    (message "random tidal sample: %s" sample)))


(defun gg/tidal-hush ()
  "send hush as a single line, stopping all streams"
  (interactive)
  (tidal-send-string ":{")
  (tidal-send-string "hush")
  (tidal-send-string ":}"))


(defun gg/tidal-mode-map ()
  "set personal keybidings for tidal-mode
C-.        stop all streams, like in SuperCollider IDE
C-c C-a    print a random sample from the SuperDirt folder"
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
