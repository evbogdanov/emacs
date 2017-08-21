;; PACKAGE CONFIGURATION MUST COME FIRST
;; -----------------------------------------------------------------------------

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; LET'S GET THE PARTY STARTED
;; -----------------------------------------------------------------------------

(setq my-dir "~/github/emacs/my")

(setq my-files '("naked-emacs"
                 "tabs-and-spaces"
                 "backup"
                 "little-helpers"
                 "russian-computer"
                 "defuns"
                 "keys"
                 "theme"
                 "dired"
                 "ido"
                 "html"
                 "erlang"
                 "neotree"
                 "expand-region"
                 "web"
                 "avy-and-ace-window"
                 "emmet"
                 "smex"
                 "multiple-cursors"
                 "c"
                 "macos"
                 "column-marker"
                 "server"))

(dolist (my-file my-files)
  (load (concat my-dir "/" my-file)))
