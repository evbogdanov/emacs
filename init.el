;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package manager
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Naked Emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Hide top menu
(menu-bar-mode 0)

;; Hide welcome screen
(setq inhibit-startup-screen t)

;; Unfancy *scratch* buffer
(setq initial-scratch-message nil)
(setq initial-major-mode 'fundamental-mode)

;; Don't hurt my ears (and my eyes)
(setq ring-bell-function 'ignore)

;; Fat-free yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Turn off syntax highlighting
(global-font-lock-mode 0)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tabs and spaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default tab-width 2)

;; No tabs, use spaces
(setq-default indent-tabs-mode nil)

;; Use it to nicely format text via `M-q`
(setq-default fill-column 80)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Backup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Where to put backup files (file1~ file2~ etc)
(setq backup-directory-alist '(("." . "~/github/emacs/bak/")))

;; Auto-save files (#file1# #file2# etc)
(setq auto-save-file-name-transforms '((".*" "~/github/emacs/bak/" t)))

;; Options
(setq make-backup-files t         ; backup of a file the first time it is saved.
      backup-by-copying t         ; don't clobber symlinks
      version-control t           ; version numbers for backup files
      delete-old-versions t       ; delete excess backup files silently
      delete-by-moving-to-trash t ;
      kept-old-versions 2         ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 2         ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t         ; auto-save every buffer that visits a file
      auto-save-timeout 20        ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200)     ; number of keystrokes between auto-saves (default: 300)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Little helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Automatically indent new lines
(electric-indent-mode t)

;; Typed text replaces the selection
(delete-selection-mode t)

;; Show matched parentheses
(show-paren-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Russian Mac keyboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; "russian-computer" doesn't work flawlessly with mac keyboard, read this:
;; http://ru-emacs.livejournal.com/83575.html

(quail-define-package "my-russian-computer" "Russian" "RU" nil
                      "ЙЦУКЕН Russian Mac layout"
                      nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ;; row 1
 ("§" ?>)

 ;; row 2
 ("q" ?й) ("w" ?ц) ("e" ?у) ("r" ?к) ("t" ?е)
 ("y" ?н) ("u" ?г) ("i" ?ш) ("o" ?щ) ("p" ?з) ("[" ?х) ("]" ?ъ)

 ;; row 3
 ("a" ?ф) ("s" ?ы) ("d" ?в) ("f" ?а) ("g" ?п)
 ("h" ?р) ("j" ?о) ("k" ?л) ("l" ?д) (";" ?ж) ("'" ?э) ("\\" ?ё)

 ;; row 4
 ("`" ?\]) ("z" ?я) ("x" ?ч) ("c" ?с) ("v" ?м) ("b" ?и)
 ("n" ?т)  ("m" ?ь) ("," ?б) ("." ?ю)

 ;; shift row 1
 ("±" ?<) ("@" ?\") ("#" ?№) ("$" ?%) ("%" ?:) ("^" ?,) ("&" ?.) ("*" ?\;)

 ;; shift row 2
 ("Q" ?Й) ("W" ?Ц) ("E" ?У) ("R" ?К) ("T" ?Е)
 ("Y" ?Н) ("U" ?Г) ("I" ?Ш) ("O" ?Щ) ("P" ?З) ("{" ?Х) ("}" ?Ъ)

 ;; shift row 3
 ("A" ?Ф) ("S" ?Ы) ("D" ?В) ("F" ?А) ("G" ?П)
 ("H" ?Р) ("J" ?О) ("K" ?Л) ("L" ?Д) (":" ?Ж) ("\"" ?Э) ("|" ?Ё)

 ;; shift row 4
 ("~" ?\[) ("Z" ?Я) ("X" ?Ч) ("C" ?С) ("V" ?М) ("B" ?И)
 ("N" ?Т)  ("M" ?Ь) ("<" ?Б) (">" ?Ю))

(setq default-input-method "my-russian-computer")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; My variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The same keys for ace-window and avy
(setq my-ace-avy-keys (number-sequence ?a ?z))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; My functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-current-line ()
  "Read current line as a string.
Note that this also returns any newline at the end of the line."
  (thing-at-point 'line t))

(defun my-line-starts-with (str)
  "Determine whether a line begins with the specified string.
Return `t` or `nil` as appropriate."
  (let ((res (search str (my-current-line))))
    (and (numberp res) (zerop res))))

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

(defun my-move-beginning-of-line ()
  "Call `(move-beginning-of-line)` or `(back-to-indentation)` depending on
the point position."
  (interactive)
  (let* ((pos (my-current-position-in-line))
         (ss (substring (my-current-line) 0 pos))
         (ch (char-after)))
    (if (= pos 0)
        (back-to-indentation)
      (if (and (string-match "^[ |\t]+$" ss) (/= ch ? ) (/= ch ?\t))
          (move-beginning-of-line nil)
        (back-to-indentation)))))

(defun my-scroll-other-window-down ()
  "Scroll text of other window down."
  (interactive)
  (scroll-other-window `-))

(defun my-heading-c-style ()
  "Create a pretty C-style heading. This type of heading starts with `/*`"
  (move-beginning-of-line 1)
  (forward-char)
  (insert "*******************************************************************************")
  (newline)
  (insert " **")
  (end-of-line)
  (newline)
  (insert " ******************************************************************************/")
  (newline))

(defun my-heading-other ()
  "Create a non-C-style heading."
  (move-beginning-of-line 1)
  (let* ((comment-symbols '(?# ?% ?/ ?\;))
         (default-comment-symbol (car comment-symbols))
         (char-at-beginning-of-line (char-after))
         (comment-symbol (if (member char-at-beginning-of-line comment-symbols)
                             char-at-beginning-of-line
                           default-comment-symbol))
         (comment-symbol-str (char-to-string comment-symbol))
         (comment-str (concat comment-symbol-str comment-symbol-str
                              comment-symbol-str " "))
         (comment-str-sep (make-string 80 comment-symbol)))

    (cl-loop
     (let ((ch (char-after)))
       (if (or (eq ch ? ) (eq ch comment-symbol))
           (delete-char 1)
         (cl-return))))

    (move-beginning-of-line 1)
    (insert comment-str-sep)
    (newline)
    (insert comment-str)
    (end-of-line)
    (newline)
    (insert comment-str-sep)
    (newline)))

(defun my-heading ()
  "Create a pretty heading. C-style or not."
  (interactive)
  (if (my-line-starts-with "/*")
      (my-heading-c-style)
    (my-heading-other)))

(defun my-str-join (words)
  "Good old words join like in many other programming languages."
  (cl-reduce
   (lambda (w1 w2) (concat w1 " " w2))
   words))

(defun my-str-kill-last-word (str)
  "Return string without last word."
  (let ((words (split-string str)))
    (if (>= 1 (length words))
        ""
      (my-str-join (butlast words)))))

(defun my-isearch-del (what)
  "Delete word or line from isearch.
Note: this function is heavily inspired by `isearch-del-char`,
see its code to understand what's going on here."
  (if (= 0 (length isearch-string))
      (ding)
    ;; The only difference is how i set isearch-string
    (setq isearch-string (if (string= what "word")
                             (my-str-kill-last-word isearch-string)
                           "")
          isearch-message (mapconcat 'isearch-text-char-description
                                     isearch-string "")))
  ;; Next lines are shamelessly copy/pasted from isearch-del-char
  (if isearch-other-end (goto-char isearch-other-end))
  (isearch-search)
  (isearch-push-state)
  (isearch-update))

(defun my-isearch-del-word ()
  "Delete a word from the end of isearch."
  (interactive)
  (my-isearch-del "word"))

(defun my-isearch-del-line ()
  "Delete a whole isearch line."
  (interactive)
  (my-isearch-del "line"))

(defun my-backward-kill-line (&optional arg)
  "Kill line(s) backward.

TODO: Make it work nicely with multiple lines. Now I have to
press an extra C-u after passing a digit argument."
  (interactive "p")
  (if (> arg 1)
      (kill-line (- (- arg 1)))
    (if (= 0 (current-column))
        (delete-backward-char 1)
      (kill-line 0))))

(defun my-backward-kill-word-or-region (&optional arg)
  "Kill word backward or go with defaults and kill region."
  (interactive "p")
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))    
    (backward-kill-word arg)))

(defun my-mark-word ()
  "Better 'mark-word', IMO."
  (interactive)
  (forward-word)
  (backward-word)
  (mark-word)
  (exchange-point-and-mark))

(defun my-mark-line ()
  "Function name says it all."
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))

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

(defun my-mark-paragraph ()
  "My own 'mark-paragraph that does exactly what I want it to do."
  (interactive)
  (mark-paragraph)
  (let ((n (line-number-at-pos)))
    (when (/= n 1)
      (forward-char)))
  (exchange-point-and-mark))

(defun my-eval-buffer ()
  "Eval current buffer depending on the major mode."
  (interactive)
  (if (or (string= major-mode "lisp-mode")
          (string= major-mode "emacs-lisp-mode"))
      (eval-buffer)
    (my-eval-buffer-interpreted)))

(defun my-eval-buffer-interpreted ()
  "Feed current buffer to some interpreter."
  (let ((modes-and-interpreters '(("js-mode"     . "node")
                                  ("python-mode" . "python3")
                                  ("perl-mode"   . "perl")
                                  ("sh-mode"     . "bash")))
        (interpreter nil))
    (dolist (m-and-i modes-and-interpreters)
      (when (string= major-mode (car m-and-i))
        (setq interpreter (cdr m-and-i))))
    (if (null interpreter)
        (error "No interpreter for the major mode")
      (shell-command-on-region (point-min)
                               (point-max)
                               interpreter))))

(defun my-pipe (prompt output-buffer replace)
  "Pipe a whole buffer if no selection. Otherwise pipe just a selected region."
  (let ((start (point-min))
        (end (point-max))
        (cmd (read-shell-command prompt)))
    (when (use-region-p)
      (setq start (region-beginning)
            end (region-end)))
    (shell-command-on-region start end cmd output-buffer replace)))

(defun my-pipe-replace ()
  "Pipe and replace a whole buffer or selected region."
  (interactive)
  (my-pipe " | " t t))

(defun my-pipe-do-not-replace ()
  "Pipe and NOT replace a whole buffer or selected region."
  (interactive)
  (my-pipe " > " nil nil))

(defun my-move-after-tag ()
  "Find the next `>` and move point after."
  (interactive)
  (loop (forward-char)
        (if (= (point) (point-max))
            (return)
          (let ((ch (string (char-after))))
            (when (string= ch ">")
              (forward-char)
              (return))))))

(defun my-move-beginning-of-tag ()
  "Find `<` and move point to it."
  (interactive)
  (loop (backward-char)
        (if (= (point) (point-min))
            (return)
          (let ((ch (string (char-after))))
            (when (string= ch "<")
              (return))))))

(defun my-comment-line ()
  "Comment or uncomment current line and leave point where it was."
  (interactive)
  (save-excursion (comment-line 1)))

(defun my-ace-window-swap ()
  "Swap between the selected window and the current window."
  (interactive)
  (ace-window 4))

(defun my-move-to-neotree ()
  "Move to the window containing Neotree buffer."
  (interactive)
  (let ((i 1) (max-tries 5))
    (loop (windmove-left)
          (if (or (string= (buffer-name) " *NeoTree*")
                  (> i max-tries))
              (return)
            (setq i (+ i 1))))))

(defun my-transpose-lines (direction previous-line-n-times)
  "Do transposing."
  (when (eq direction 'down) (next-line))
  (transpose-lines 1)
  (previous-line previous-line-n-times)
  (end-of-line))

(defun my-transpose-lines-simple () (interactive) (my-transpose-lines 'up 1))
(defun my-transpose-lines-up     () (interactive) (my-transpose-lines 'up 2))
(defun my-transpose-lines-down   () (interactive) (my-transpose-lines 'down 1))

(defvar my-transpose-lines-map
  (let ((map (make-sparse-keymap)))
    (define-key map "p" 'my-transpose-lines-up)
    (define-key map "n" 'my-transpose-lines-down)
    map))

(defun my-transpose-lines-interactively ()
  "Activate interactive transposing."
  (interactive)
  (message "Move line up (p) or down (n)")
  (set-transient-map my-transpose-lines-map t))

(defun my-4-windows ()
  "Split Emacs into 4 windows."
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (split-window-below)
  (windmove-right)
  (split-window-below)
  (windmove-left))

(defun my-4-windmove-diagonal ()
  "Select the window that is diagonally across from the current window."
  (interactive)
  (let ((is-top (null (windmove-find-other-window 'up)))
        (is-bottom (window-minibuffer-p (windmove-find-other-window 'down)))
        (is-right (null (windmove-find-other-window 'right))))
    (cond ((and is-top is-right)
           (windmove-left)
           (windmove-down))
          (is-top
           (windmove-right)
           (windmove-down))
          ((and is-bottom is-right)
           (windmove-left)
           (windmove-up))
          (is-bottom
           (windmove-right)
           (windmove-up))
          (t
           (user-error "Something's wrong with the windows' layout")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package osx-clipboard
  :ensure t
  :config
  (osx-clipboard-mode +1))

(use-package ace-window
  :ensure t
  :config
  (setq aw-keys my-ace-avy-keys))

(use-package avy
  :ensure t
  :config
  (custom-set-faces
   '(avy-lead-face ((t (:foreground "red"))))
   '(avy-lead-face-0 ((t (:foreground "red"))))
   '(avy-lead-face-1 ((t (:foreground "red"))))
   '(avy-lead-face-2 ((t (:foreground "red")))))
  (setq avy-all-windows nil
        avy-background t
        avy-keys my-ace-avy-keys))

(use-package ido
  :ensure t
  :config
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-max-prospects 10
        ido-default-file-method 'selected-window
        ido-auto-merge-work-directories-length -1
        ido-separator "\n")
  (ido-mode 1)
  (ido-everywhere 1))

(use-package flx-ido
  :ensure t
  :config
  (flx-ido-mode 1)
  ;; Disable ido faces to see flx highlights
  (setq ido-use-faces nil))

(use-package smex
  :ensure t)

(use-package expand-region
  :ensure t
  :config
  (setq expand-region-contract-fast-key ","))

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue?\\'" . web-mode))
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-style-padding 2
        web-mode-script-padding 2)
  (define-key web-mode-map (kbd "M-a") 'my-move-beginning-of-tag)
  (define-key web-mode-map (kbd "M-e") 'my-move-after-tag))

(use-package emmet-mode
  :ensure t
  :config
  (setq emmet-self-closing-tag-style "")
  (add-hook 'html-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode)
  (add-hook 'web-mode-hook  'emmet-mode))

(use-package neotree
  :ensure t
  :config
  (setq-default neo-show-hidden-files t)
  (setq-default neo-theme 'ascii)
  (define-key neotree-mode-map (kbd "F") 'neotree-change-root)
  (define-key neotree-mode-map (kbd "f")
    (neotree-make-executor :file-fn 'neo-open-file
                           :dir-fn  'neo-open-dir))
  (define-key neotree-mode-map (kbd "b") 'neotree-select-up-node))

(use-package dired
  :config
  ;; Open files and directories right in the dired buffer
  (put 'dired-find-alternate-file 'disabled nil)

  ;; Don't mess with my keybindings
  (define-key dired-mode-map (kbd "C-o") nil)
  (define-key dired-mode-map (kbd "M-s") nil)

  ;; Go directory/file forward
  (define-key dired-mode-map (kbd "f") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)

  ;; Go directory backward
  (define-key dired-mode-map (kbd "b") (lambda ()
                                         (interactive)
                                         (find-alternate-file "..")))

  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  ;; Dired Extra (enables dired-jump C-x C-j)
  (require 'dired-x))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Quicker access to help
;; M-h was mapped to `mark-paragraph`
(global-set-key (kbd "M-h") help-map)
(global-set-key (kbd "M-h M-h") 'help-for-help)

;; Enhanced C-a
(global-set-key (kbd "C-a") 'my-move-beginning-of-line)
(define-key visual-line-mode-map (kbd "C-a") 'beginning-of-visual-line)

;; Easier scrolling
(global-set-key (kbd "C-z") 'scroll-down-command)
(global-set-key (kbd "C-M-z") 'my-scroll-other-window-down)

;; List all buffers in the current window
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(define-key Buffer-menu-mode-map (kbd "C-o") nil)

;; Alias to C-x C-f
(global-set-key (kbd "C-x f") 'find-file)

;; 4 windows
(global-set-key (kbd "C-x 4") 'my-4-windows)

;; Stuff in other window
(define-prefix-command 'my-c-o)
(global-set-key (kbd "C-o") 'my-c-o)
(define-key my-c-o (kbd "C-o") 'ace-window)
(define-key my-c-o (kbd "b") 'switch-to-buffer-other-window)
(define-key my-c-o (kbd "d") 'dired-other-window)
(define-key my-c-o (kbd "f") 'find-file-other-window)
(define-key my-c-o (kbd "r") 'find-file-read-only-other-window)
(define-key my-c-o (kbd "C-f") 'windmove-right)
(define-key my-c-o (kbd "C-b") 'windmove-left)
(define-key my-c-o (kbd "C-p") 'windmove-up)
(define-key my-c-o (kbd "C-n") 'windmove-down)
(define-key my-c-o (kbd "C-d") 'my-4-windmove-diagonal)
(define-key my-c-o (kbd "C-s") 'my-ace-window-swap)
(define-key my-c-o (kbd "t") 'neotree-toggle)
(define-key my-c-o (kbd "C-t") 'my-move-to-neotree)
(define-key my-c-o (kbd "C-j") 'dired-jump-other-window)

;; Better 'move-to-window-line-top-bottom
(global-set-key (kbd "M-r") 'avy-goto-line)

;; Better than tons of unused commands
(global-set-key (kbd "M-s") 'avy-goto-word-1)

;; Enable C-x C-u and C-x C-l.
;; Add similar combo for heading (C-x C-h is undefined by default)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(global-set-key (kbd "C-x C-h") 'my-heading)

;; Familiar shell-like behaviour for C-h, C-w and C-u
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-w") 'my-backward-kill-word-or-region)
(global-set-key (kbd "C-u") 'my-backward-kill-line)
(define-key isearch-mode-map (kbd "C-h") 'isearch-del-char)
(define-key isearch-mode-map (kbd "C-w") 'my-isearch-del-word)
(define-key isearch-mode-map (kbd "C-u") 'my-isearch-del-line)
(add-hook
 'ido-setup-hook
 (lambda ()
   (define-key ido-completion-map (kbd "C-h") 'ido-delete-backward-updir)
   (define-key ido-completion-map (kbd "C-w") 'ido-delete-backward-word-updir)
   (define-key ido-completion-map (kbd "M-h f") 'smex-describe-function)))

;; Replace 'execute-extended-command
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Marking
(define-prefix-command 'my-c-spc)
(global-set-key (kbd "C-@") 'my-c-spc)
(define-key my-c-spc (kbd "C-@") 'set-mark-command)
(define-key my-c-spc (kbd "w") 'my-mark-word)
(define-key my-c-spc (kbd "l") 'my-mark-line)
(define-key my-c-spc (kbd "p") 'my-mark-paragraph)
(define-key my-c-spc (kbd "b") 'mark-whole-buffer)
(define-key my-c-spc (kbd ".") 'er/expand-region)

;; C-M-y is undefined by default
(global-set-key (kbd "C-M-y") 'my-yank-line)

;; Eval buffer
(global-set-key (kbd "M-RET") 'my-eval-buffer)

;; Piping
(global-set-key (kbd "M-|") 'my-pipe-replace)
(global-set-key (kbd "M-\"") 'my-pipe-do-not-replace)

;; Comments
(global-set-key (kbd "M-;") 'my-comment-line)  ; used to be `comment-dwim`
(global-set-key (kbd "M-'") 'comment-dwim)     ; used to be `abbrev-prefix-mark`

;; Transposing lines
(global-set-key (kbd "C-x C-t") 'my-transpose-lines-simple)      ; was `transpose-lines`
(global-set-key (kbd "C-x t") 'my-transpose-lines-interactively) ; was undefined

;; macOS-like behaviour of Fn+Left/Right arrows
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Front end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq css-indent-offset 2)
(setq js-indent-level 2)
(add-to-list 'auto-mode-alist '("\\.ts$" . javascript-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'python-mode-hook
          (lambda ()
            (setq tab-width 4)
            (setq python-indent 4)
            (custom-set-variables
             '(python-guess-indent nil)
             '(python-indent-offset 4))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
