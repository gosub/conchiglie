;; package stuff

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; package configuration

(move-text-default-bindings)

;; personal customizations (cfg folder, gg- prefix)

(add-to-list 'load-path
	     (expand-file-name "cfg" user-emacs-directory))

(require 'gg-dirs-and-files)
(require 'gg-functions)
(require 'gg-ux)
(require 'gg-visuals)
(require 'gg-unicode)
(require 'gg-evil)
(require 'gg-org)
(require 'gg-erlang)
(require 'gg-keybindings)
(require 'gg-tidal)
(require 'gg-cpp)
