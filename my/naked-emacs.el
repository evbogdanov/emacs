;; NAKED EMACS
;; -----------------------------------------------------------------------------

;; Hide top menu
(menu-bar-mode 0)

;; Hide welcome screen
(setq inhibit-startup-screen t)

;; No default messages in *scratch* buffer
(setq initial-scratch-message nil)

;; Don't hurt my ears (and my eyes)
(setq ring-bell-function 'ignore)

;; Fat-free yes/no
(fset 'yes-or-no-p 'y-or-n-p)
