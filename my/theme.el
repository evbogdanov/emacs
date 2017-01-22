;; THEME: GUI
;; -----------------------------------------------------------------------------

(when window-system
  ;; Cursor, please don't blink
  (blink-cursor-mode 0)

  ;; Scroll bar? no, thanks
  (scroll-bar-mode 0)

  ;; No annoying tooltips
  (tooltip-mode 0)

  ;; Hide very top menu
  (tool-bar-mode 0)

  ;; Hide vertical scroll bar
  (toggle-scroll-bar 0)

  ;; Custom font
  (set-face-attribute 'default nil
                      :family "Menlo"
                      :height 120
                      :weight 'normal)

  ;; Initial size
  (setq initial-frame-alist
        '((width . 100) (height . 50)))

  ;; Solarized light theme
  (setq solarized-use-less-bold t)
  (custom-set-variables
   '(custom-enabled-themes (quote (solarized-light)))
   '(custom-safe-themes
     (quote ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))))

;; THEME: TERMINAL
;; -----------------------------------------------------------------------------

(unless window-system
  (custom-set-faces '(default ((t (:background "nil")))))

  (custom-set-variables
   '(custom-enabled-themes (quote (zenburn)))
   '(custom-safe-themes
     (quote ("9d91458c4ad7c74cf946bd97ad085c0f6a40c370ac0a1cbeb2e3879f15b40553" default)))))
