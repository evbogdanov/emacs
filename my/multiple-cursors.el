;; MULTIPLE CURSORS
;; -----------------------------------------------------------------------------

(require 'multiple-cursors)

;; Cursors for each line in active region
;; --------------------------------------
(global-set-key (kbd "<M-up>")   'mc/edit-lines)   ; gui 
(global-set-key (kbd "ESC <up>") 'mc/edit-lines)   ; terminal

;; Cursors based on selected string
;; --------------------------------

;; gui
(global-set-key (kbd "<M-down>")  'mc/mark-all-like-this)
(global-set-key (kbd "<M-left>")  'mc/mark-previous-like-this)
(global-set-key (kbd "<M-right>") 'mc/mark-next-like-this)

;; terminal
(global-set-key (kbd "ESC <down>")  'mc/mark-all-like-this)
(global-set-key (kbd "ESC <left>")  'mc/mark-previous-like-this)
(global-set-key (kbd "ESC <right>") 'mc/mark-next-like-this)
