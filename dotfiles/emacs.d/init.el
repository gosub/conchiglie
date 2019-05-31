;; package stuff

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; personal customizations (cfg folder, gg- prefix)

(add-to-list 'load-path
	     (expand-file-name "cfg" user-emacs-directory))

(require 'gg-dirs-and-files)
(require 'gg-functions)
(require 'gg-ux)
(require 'gg-visuals)
(require 'gg-unicode)
;; evil mode

;; make sure evil is installed
(unless (package-installed-p 'evil)
  (package-refresh-contents)
  (package-install 'evil))

;; toggle vim in local buffer with F7
(require 'evil)



(require 'gg-org)
(require 'gg-keybindings)
