(let* ((lib-dir "/usr/lib/erlang/lib/")
       (tools-dir (file-name-completion "tools" lib-dir))
       (erlang-mode-dir (concat lib-dir tools-dir "emacs")))
  (add-to-list 'load-path erlang-mode-dir))

(setq erlang-root-dir
      "/usr/lib/erlang")

(add-to-list 'exec-path
	     "/usr/lib/erlang/bin")

(require 'erlang-start)

(provide 'gg-erlang)
