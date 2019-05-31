(add-to-list 'load-path
	     "/usr/lib/erlang/lib/tools-3.2/emacs")

(setq erlang-root-dir
      "/usr/lib/erlang")

(add-to-list 'exec-path
	     "/usr/lib/erlang/bin")

(require 'erlang-start)

(provide 'gg-erlang)
