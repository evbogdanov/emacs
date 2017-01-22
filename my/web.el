;; WEB MODE
;; -----------------------------------------------------------------------------

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'"     . web-mode)) ;; plain html
(add-to-list 'auto-mode-alist '("\\.html\\.ep\\'" . web-mode)) ;; mojo templates
