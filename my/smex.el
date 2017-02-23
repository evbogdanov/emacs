;; SMEX
;; -----------------------------------------------------------------------------

(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map (kbd "C-h")    'delete-backward-char)
            (define-key ido-completion-map (kbd "§ f") 'smex-describe-function)
            (define-key ido-completion-map (kbd "§ w") 'smex-where-is)))
