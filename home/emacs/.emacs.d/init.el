;; package stuff

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

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

(use-package move-text
  :ensure t
  :config (move-text-default-bindings))


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
