;; HIGHLIGHT LINES THAT GO OVER 80 CHARS
;; -----------------------------------------------------------------------------

(require 'column-marker)

(define-key my-f13 (kbd "8") '(lambda () (interactive) (column-marker-1 80)))

(define-key my-f13 (kbd "0") '(lambda () (interactive) (column-marker-1 -1)))
