(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; M-x list-packages

(package-install 'solarized-theme)
(package-install 'zenburn-theme)
(package-install 'neotree)
(package-install 'expand-region)
(package-install 'avy)
(package-install 'ace-window)
(package-install 'web-mode)
(package-install 'emmet-mode)
(package-install 'smex)
(package-install 'multiple-cursors)
(package-install 'erlang)
(package-install 'alchemist)
(package-install 'osx-clipboard)
(package-install 'column-marker)

;; M-x eval-buffer
