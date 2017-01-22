(load "~/emacs/my/naked-emacs")
(load "~/emacs/my/tabs-and-spaces")
(load "~/emacs/my/backup")
(load "~/emacs/my/little-helpers")
(load "~/emacs/my/russian-mac-keyboard")

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

(defun my-open-string-as-url ()
  "Open string under cursor as URL"
  (interactive)
  (let ((str (thing-at-point 'url))
        (cmd "open ")) ; warning: macOS only
    (if (null str)
        (error "no url to open")
      (shell-command (concat cmd str)))))

(define-key my-f13 (kbd "o") 'my-open-string-as-url)

;; KEYS: C-X C-U AND C-X C-L
;; -----------------------------------------------------------------------------

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; KEYS: C-A
;; -----------------------------------------------------------------------------

(defun my-current-position-in-line ()
  "Function `(current-column)` has a fatal flaw: it doesn't treat tab as a
single character. In order to use functions which work with char indexes, I need
something else."
  (save-excursion
    (let ((p1 (point))
          (p2))
      (move-beginning-of-line nil)
      (setq p2 (point))
      (- p1 p2))))

(defun my-c-a ()
  "Call `(move-beginning-of-line)` or `(back-to-indentation)` depending on
the point position."
  (interactive)
  (let* ((pos (my-current-position-in-line))
         (ss (substring (thing-at-point 'line t) 0 pos))
         (ch (char-after)))
    (if (= pos 0)
        (back-to-indentation)
      (if (and (string-match "^[ |\t]+$" ss) (/= ch ? ) (/= ch ?\t))
          (move-beginning-of-line nil)
        (back-to-indentation)))))

(global-set-key (kbd "C-a") 'my-c-a)

;; KEYS: C-X C-H
;; -----------------------------------------------------------------------------

(defun my-h+ ()
  "Create heading via `h+` script."
  (interactive)
  (shell-command-on-region
   (line-beginning-position) (line-end-position) "h+" t t))

(global-set-key (kbd "C-x C-h") 'my-h+)

;; KEYS: C-SPC
;; -----------------------------------------------------------------------------

;; Let's emacs be symmetrical to my macOS language switcher
(global-set-key (kbd "C-SPC") 'toggle-input-method)
(global-set-key (kbd "C-@")   'toggle-input-method) ;; special for -nw mode

;; KEYS: M-SPC
;; -----------------------------------------------------------------------------

;; Delete all spaces and tabs around point
(global-set-key (kbd "M-SPC") '(lambda () (interactive) (just-one-space 0)))

;; KEYS: C-H / M-H
;; -----------------------------------------------------------------------------

(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)

;; ISEARCH: C-H
;; -----------------------------------------------------------------------------

(define-key isearch-mode-map (kbd "C-h") 'isearch-del-char)

;; ISEARCH: M-H
;; -----------------------------------------------------------------------------

(defun my-str-join (words)
  "Good old words join like in many other programming languages."
  (reduce
   (lambda (w1 w2) (concat w1 " " w2))
   words))

(defun my-str-kill-last-word (str)
  "Return string without last word."
  (let ((words (split-string str)))
    (if (>= 1 (length words))
        ""
      (my-str-join (butlast words)))))

(defun my-isearch-del-word ()
  "Delete word from end of search string and search again.

Note: this function is heavily inspired by `isearch-del-char`,
see its code to understand what's going on here"
  (interactive)
  (if (= 0 (length isearch-string))
      (ding)
    ;; the only difference is how i set isearch-string
    (setq isearch-string  (my-str-kill-last-word isearch-string)
          isearch-message (mapconcat 'isearch-text-char-description
                                     isearch-string "")))
  ;; next lines are shamelessly copy/pasted from isearch-del-char
  (if isearch-other-end (goto-char isearch-other-end))
  (isearch-search)
  (isearch-push-state)
  (isearch-update))

(define-key isearch-mode-map (kbd "M-h") 'my-isearch-del-word)

;; KEYS: SCROLLING
;; -----------------------------------------------------------------------------

;; C-z, C-v scroll this window up/down
(global-set-key (kbd "C-z") 'scroll-down-command)

;; M-z, M-v scroll other window up/down
(global-set-key (kbd "M-v") 'scroll-other-window)
(global-set-key (kbd "M-z") '(lambda () (interactive) (scroll-other-window `-)))
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

;; KEYS: M-K
;; -----------------------------------------------------------------------------

(defun my-backward-kill-line ()
  "By default, M-k runs the command kill-sentence, i want it to behave
differently: kill the characters from the current point to the beginning of the
line. If the point is already at the beginning of the line, the current
line is joined with the previous one.
"
  (interactive)
  (if (= 0 (current-column))
      (delete-backward-char 1)
    (kill-line 0)))

(global-set-key (kbd "M-k") 'my-backward-kill-line)

;; KEYS: C-M-K
;; -----------------------------------------------------------------------------

;; Emacs reply to Vim-ish `d d`
(global-set-key (kbd "C-M-k") 'kill-whole-line)

;; KEYS: C-M-Y
;; -----------------------------------------------------------------------------

(defun my-yank-line ()
  "Copy and paste current line."
  (interactive)
  (setq kill-ring (cons "" kill-ring)) ;; Don't append previous kill
  (let ((beg)
        (end))
    (move-beginning-of-line nil)
    (setq beg (point))
    (move-end-of-line nil)
    (setq end (point))
    (kill-ring-save beg end)
    (open-line 1)
    (next-line)
    (yank)))

(global-set-key (kbd "C-M-y") 'my-yank-line)

;; KEYS: M-\
;; -----------------------------------------------------------------------------

(defun my-pipe-region (start end command)
  "Pipe region through a shell command."
  (interactive (let (cmd)
                 (unless (mark)
                   (error "No mark, no region"))
                 (setq cmd (read-shell-command "Pipe through command: "))
                 (list (region-beginning) (region-end) cmd)))
  (shell-command-on-region start end command t t))

(global-set-key (kbd "M-\\") 'my-pipe-region)

;; KEYS: C-X M-\
;; -----------------------------------------------------------------------------

(global-unset-key (kbd "M-|"))

(global-set-key (kbd "C-x M-\\") 'shell-command-on-region)

;; KEYS: C-\
;; -----------------------------------------------------------------------------

(defun my-pipe-buffer ()
  "Pipe a whole buffer through a shell command. A buffer content
will be replaced."
  (interactive)
  (let ((start (point-min))
        (end (point-max))
        (cmd (read-shell-command "pipe through command: ")))
    (shell-command-on-region start end cmd t t)))

(global-set-key (kbd "C-\\") 'my-pipe-buffer)

;; KEYS: C-X C-\
;; -----------------------------------------------------------------------------

(defun my-pipe-buffer-to-echo-area ()
  "Pipe a whole buffer through a shell command (don't replace buffer's content)"
  (interactive)
  (let ((start (point-min))
        (end (point-max))
        (cmd (read-shell-command "pipe through command: ")))
    (shell-command-on-region start end cmd)))

(global-set-key (kbd "C-x C-\\") 'my-pipe-buffer-to-echo-area)

;; KEYS: C-X A, C-X P, C-X L, C-X W, C-X SPC
;; -----------------------------------------------------------------------------

;; By default 'C-x h' bound to 'select all'
;; I don't like it...
(global-unset-key (kbd "C-x h"))

;; ... that's better:
;; Select [a]ll
(global-set-key (kbd "C-x a") 'mark-whole-buffer)

;; Select [p]aragraph
(global-set-key (kbd "C-x p") 'mark-paragraph)

;; Select [l]ine
(defun my-mark-line ()
  "Function name says it all."
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))

(global-set-key (kbd "C-x l") 'my-mark-line)

;; Select [w]ord
(defun my-mark-word ()
  "Better 'mark-word', imho."
  (interactive)
  (forward-word)
  (backward-word)
  (mark-word)
  (exchange-point-and-mark))

(global-set-key (kbd "C-x w") 'my-mark-word)

;; Select anything
(global-set-key (kbd "C-x SPC") 'set-mark-command)

;; KEYS: C-X C-P (A-LA C-X C-E)
;; -----------------------------------------------------------------------------

;; Acme equivalent: Edit , > perl
(global-set-key (kbd "C-x C-p")
                (lambda ()
                  (interactive)
                  (shell-command-on-region 1 (point-max) "perl")))

;; MELPA
;; -----------------------------------------------------------------------------

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; THEME
;; -----------------------------------------------------------------------------

(load "~/emacs/my/theme")

;; MODE: IDO (BUILT-IN)
;; -----------------------------------------------------------------------------

(ido-mode 1)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-separator "\n")
(setq ido-create-new-buffer 'always)

;; MODE: HTML (BUILT-IN)
;; -----------------------------------------------------------------------------

(add-hook 'html-mode-hook (lambda () (setq sgml-basic-offset 4)))

;; MODE: ERLANG
;; -----------------------------------------------------------------------------

(require 'erlang-start)

;; Better indentation
(add-hook 'erlang-mode-hook
          (lambda ()
            (local-set-key (kbd "RET") 'newline-and-indent)))

;; MODE: NEOTREE
;; -----------------------------------------------------------------------------

(require 'neotree)
(setq neo-show-hidden-files t)
(setq neo-theme 'ascii)
(define-key my-f13 (kbd "n") 'neotree-toggle)

(add-hook 'neotree-mode-hook
          (lambda ()
            (define-key neotree-mode-map (kbd "C-j") 'neotree-change-root)
            (define-key neotree-mode-map (kbd "M-j") 'neotree-select-up-node)))

;; MODE: EXPAND REGION
;; -----------------------------------------------------------------------------

(require 'expand-region)
(define-key my-f13 (kbd "=") 'er/expand-region)

;; MODE: WEB
;; -----------------------------------------------------------------------------

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'"     . web-mode)) ;; plain html
(add-to-list 'auto-mode-alist '("\\.html\\.ep\\'" . web-mode)) ;; mojo templates

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

;; MODE: EMMET
;; -----------------------------------------------------------------------------

(require 'emmet-mode)
(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)
(add-hook 'web-mode-hook  'emmet-mode)

;; MODE: SMEX
;; -----------------------------------------------------------------------------

(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; smex mode messes with ido-completion-map:
;;     - "C-h f" runs "smex-describe-function"
;;     - "C-h w" runs "smex-where-is"
;;
;; Have a look: https://github.com/nonsequitur/smex/blob/master/smex.el
;;
;; Adjust my key bindings
(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map (kbd "C-h")    'delete-backward-char)
            (define-key ido-completion-map (kbd "<f1> f") 'smex-describe-function)
            (define-key ido-completion-map (kbd "<f1> w") 'smex-where-is)))

;; MULTIPLE CURSORS
;; -----------------------------------------------------------------------------

(require 'multiple-cursors)

;; Cursors for each line in active region
;; --------------------------------------
(global-set-key (kbd "<M-up>")   'mc/edit-lines)   ; gui 
(global-set-key (kbd "ESC <up>") 'mc/edit-lines)   ; terminal

;; Cursors based on selected string
;; --------------------------------

;; gui
(global-set-key (kbd "<M-down>")  'mc/mark-all-like-this)
(global-set-key (kbd "<M-left>")  'mc/mark-previous-like-this)
(global-set-key (kbd "<M-right>") 'mc/mark-next-like-this)

;; terminal
(global-set-key (kbd "ESC <down>")  'mc/mark-all-like-this)
(global-set-key (kbd "ESC <left>")  'mc/mark-previous-like-this)
(global-set-key (kbd "ESC <right>") 'mc/mark-next-like-this)

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

;; PERL MODE
;; -----------------------------------------------------------------------------

;; Indentation bug:
;; http://superuser.com/questions/537366/in-emacs-how-do-i-align-closing-parentheses-with-the-start-of-the-opening-line
(setq perl-indent-parens-as-block t)

;; C MODE
;; -----------------------------------------------------------------------------

(setq c-default-style "bsd"
      c-basic-offset 4)

;; EM DASH
;; -----------------------------------------------------------------------------

(global-set-key (kbd "M-_") '(lambda () (interactive) (insert "—")))

;; MAC CLIPBOARD IN A TEXT TERMINAL
;; -----------------------------------------------------------------------------

(osx-clipboard-mode +1)

;; HIGHLIGHT LINES THAT GO OVER 80 CHARS
;; -----------------------------------------------------------------------------

(require 'column-marker)

(define-key my-f13 (kbd "8") '(lambda () (interactive) (column-marker-1 80)))

(define-key my-f13 (kbd "0") '(lambda () (interactive) (column-marker-1 -1)))
