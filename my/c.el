;; C MODE
;; -----------------------------------------------------------------------------

(setq c-default-style "bsd"
      c-basic-offset 4)

;; C++ MODE
;; -----------------------------------------------------------------------------

(add-hook 'c++-mode-hook
          (lambda ()
            "Do not increase the indentation level when entering namespace scope."
            (c-set-offset 'innamespace [0])))
