;; ERLANG MODE
;; -----------------------------------------------------------------------------

(require 'erlang-start)

;; Better indentation
(add-hook 'erlang-mode-hook
          (lambda ()
            (local-set-key (kbd "RET") 'newline-and-indent)))
