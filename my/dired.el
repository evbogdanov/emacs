;; DIRED
;; -----------------------------------------------------------------------------

(require 'dired )

;; Don't ask me about enabling this command
(put 'dired-find-alternate-file 'disabled nil)

;; Go dir/file forward
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
(define-key dired-mode-map (kbd "C-j") 'dired-find-alternate-file)

;; Go dir backward
(define-key dired-mode-map (kbd "M-j") (lambda ()
                                         (interactive)
                                         (find-alternate-file "..")))

;; Always delete and copy recursively
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies  'always)
