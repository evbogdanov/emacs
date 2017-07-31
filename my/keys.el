;; PREFIX KEY: C-O
;; -----------------------------------------------------------------------------

;; It's too cumbersome to type combos like this: `C-x 4 C-f`. Use shortcuts!

(define-prefix-command 'my-C-o)

(global-set-key (kbd "C-o") 'my-C-o)

(define-key my-C-o (kbd "b") 'switch-to-buffer-other-window)
(define-key my-C-o (kbd "d") 'dired-other-window)
(define-key my-C-o (kbd "f") 'find-file-other-window)
(define-key my-C-o (kbd "C-f") 'find-file-other-window)

;; KEY: C-X F
;; -----------------------------------------------------------------------------

(global-set-key (kbd "C-x f") 'find-file)

;; PREFIX KEY: F13 (EX CAPS LOCK)
;; -----------------------------------------------------------------------------

(define-prefix-command 'my-f13)

(global-set-key (kbd "<f13>") 'my-f13)

;; KEYS: F13 S, F13 F, F13 T, F13 V
;; -----------------------------------------------------------------------------

(define-key my-f13 (kbd "s") 'shell)
(define-key my-f13 (kbd "f") 'fundamental-mode)
(define-key my-f13 (kbd "t") 'toggle-truncate-lines)
(define-key my-f13 (kbd "v") 'visual-line-mode)
(define-key my-f13 (kbd "i") 'lisp-interaction-mode)

;; KEYS: F13 O
;; -----------------------------------------------------------------------------

(define-key my-f13 (kbd "o") 'my-open-string-as-url)

;; KEYS: C-X C-U AND C-X C-L
;; -----------------------------------------------------------------------------

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; KEYS: C-A
;; -----------------------------------------------------------------------------

(global-set-key (kbd "C-a") 'my-move-beginning-of-line)

;; Restore "C-a" in visual-line-mode
(define-key visual-line-mode-map (kbd "C-a") 'beginning-of-visual-line)

;; KEYS: HEADING
;; -----------------------------------------------------------------------------

;; I chose "C-x C-h" because it acts very similar to Emacs's built-in combos
;; "C-x C-u" and "C-x C-l"
(global-set-key (kbd "C-x C-h") 'my-heading)

;; KEYS: C-SPC
;; -----------------------------------------------------------------------------

;; Let's emacs be symmetrical to my macOS language switcher
(global-set-key (kbd "C-SPC") 'toggle-input-method)
(global-set-key (kbd "C-@")   'toggle-input-method) ;; special for -nw mode

;; KEYS: M-SPC
;; -----------------------------------------------------------------------------

(global-set-key (kbd "M-SPC") 'my-no-space)

;; KEYS: C-H / M-H
;; -----------------------------------------------------------------------------

(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)

;; Isearch tweaks:
(define-key isearch-mode-map (kbd "C-h") 'isearch-del-char)
(define-key isearch-mode-map (kbd "M-h") 'my-isearch-del-word)

;; KEYS: SCROLLING
;; -----------------------------------------------------------------------------

;; C-z, C-v scroll this window up/down
(global-set-key (kbd "C-z") 'scroll-down-command)

;; M-z, M-v scroll other window up/down
(global-set-key (kbd "M-v") 'scroll-other-window)
(global-set-key (kbd "M-z") 'my-scroll-other-window-down)
(global-unset-key (kbd "C-M-v"))

;; KEYS: SWITCH BETWEEN BUFFERS
;; -----------------------------------------------------------------------------

(global-set-key (kbd "<C-up>")    'windmove-up)
(global-set-key (kbd "<C-down>")  'windmove-down)
(global-set-key (kbd "<C-right>") 'windmove-right)
(global-set-key (kbd "<C-left>")  'windmove-left)

;; Fix for shell
(with-eval-after-load 'shell
  (define-key shell-mode-map (kbd "<C-up>")   'windmove-up)
  (define-key shell-mode-map (kbd "<C-down>") 'windmove-down))

;; KEYS: LIST ALL BUFFERS IN THE CURRENT WINDOW
;; -----------------------------------------------------------------------------

(global-set-key (kbd "C-x C-b") 'buffer-menu)

;; KEYS: M-K
;; -----------------------------------------------------------------------------

(global-set-key (kbd "M-k") 'my-backward-kill-line)

;; KEYS: C-M-K
;; -----------------------------------------------------------------------------

;; Emacs reply to Vim-ish `d d`
(global-set-key (kbd "C-M-k") 'kill-whole-line)

;; KEYS: C-M-Y
;; -----------------------------------------------------------------------------

(global-set-key (kbd "C-M-y") 'my-yank-line)

;; KEYS: PIPING
;; -----------------------------------------------------------------------------

(global-set-key (kbd "M-\\") 'my-pipe-region)

(global-unset-key (kbd "M-|"))

(global-set-key (kbd "C-x M-\\") 'shell-command-on-region)

(global-set-key (kbd "C-\\") 'my-pipe-buffer)

(global-set-key (kbd "C-x C-\\") 'my-pipe-buffer-to-echo-area)

;; KEYS: MARKING
;; -----------------------------------------------------------------------------

(global-unset-key (kbd "C-x h"))

(global-set-key (kbd "C-x a") 'mark-whole-buffer)

(global-set-key (kbd "C-x p") 'mark-paragraph)

(global-set-key (kbd "C-x l") 'my-mark-line)

(global-set-key (kbd "C-x w") 'my-mark-word)

(global-set-key (kbd "C-x SPC") 'set-mark-command)

;; KEYS: C-X C-P
;; -----------------------------------------------------------------------------

(global-set-key (kbd "C-x C-p") 'my-perl-eval-buffer)

;; KEYS: EM DASH
;; -----------------------------------------------------------------------------

(global-set-key (kbd "M-_") '(lambda () (interactive) (insert "—")))

;; KEYS: F1 -> §
;; -----------------------------------------------------------------------------

(global-set-key "§" help-map)
