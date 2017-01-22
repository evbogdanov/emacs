;; NEOTREE MODE
;; -----------------------------------------------------------------------------

(require 'neotree)

(setq neo-show-hidden-files t)

(setq neo-theme 'ascii)

(define-key my-f13 (kbd "n") 'neotree-toggle)

(add-hook 'neotree-mode-hook
          (lambda ()
            (define-key neotree-mode-map (kbd "C-j") 'neotree-change-root)
            (define-key neotree-mode-map (kbd "M-j") 'neotree-select-up-node)))
