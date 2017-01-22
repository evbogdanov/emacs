;; AVY
;; -----------------------------------------------------------------------------

;; Avy is better 'ace jump'
;; http://emacsredux.com/blog/2015/07/19/ace-jump-mode-is-dead-long-live-avy/

(require 'avy)
(define-key my-f13 (kbd "l") 'avy-goto-line)
(define-key my-f13 (kbd "w") 'avy-goto-word-1)

;; Use only the selected window
(setq avy-all-windows nil)

;; Customize decision chars for avy and ace-window modes:
(setq my-avy-and-ace-keys '(
;;           ?r ?t ?y ?u
    ?a ?s ?d ?f ?g ?h ?j ?k ?l
;;           ?v ?b ?n ?m
))

(setq avy-keys my-avy-and-ace-keys)

;; ACE WINDOW
;; -----------------------------------------------------------------------------

(require 'ace-window)
(define-key my-f13 (kbd "<f13>") 'ace-window)
(setq aw-keys my-avy-and-ace-keys)
