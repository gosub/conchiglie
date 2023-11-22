(if (file-directory-p "/usr/lib/erlang")
  (let* ((lib-dir "/usr/lib/erlang/lib/")
         (tools-dir (file-name-completion "tools" lib-dir))
         (erlang-mode-dir (concat lib-dir tools-dir "emacs")))
    (add-to-list 'load-path erlang-mode-dir)
    (add-to-list 'exec-path "/usr/lib/erlang/bin")
    (setq erlang-root-dir "/usr/lib/erlang")
    (require 'erlang-start)))

; printable char set is unicode
(setq inferior-erlang-machine-options
      '("+pc" "unicode"))

(provide 'gg-erlang)
