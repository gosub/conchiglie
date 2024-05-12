;; package stuff

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; package configuration

(if (boundp 'move-text-default-bindings)
  (move-text-default-bindings))

;; personal customizations (cfg folder, gg- prefix)

(add-to-list 'load-path
	     (expand-file-name "cfg" user-emacs-directory))

;; packages

(use-package ledger-mode
  :ensure t)

(use-package magit
  :ensure t)

(use-package evil
  :ensure t
  :bind ("C-c v" . evil-local-mode))



;; personal configurations

(require 'gg-dirs-and-files)
(require 'gg-functions)
(require 'gg-ux)
(require 'gg-visuals)
(require 'gg-unicode)
(require 'gg-org)
(require 'gg-erlang)
(require 'gg-keybindings)
(require 'gg-tidal)
(require 'gg-cpp)
