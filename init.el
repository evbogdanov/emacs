;; SETTING UP THE PACKAGE MANAGER
;; ----------------------------------------------------------------------------

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; NAKED EMACS
;; ----------------------------------------------------------------------------

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

;; TABS AND SPACES
;; ----------------------------------------------------------------------------

(setq-default tab-width 4)

;; No tabs, use spaces
(setq-default indent-tabs-mode nil)

;; Use it to nicely format text via `M-q`
(setq-default fill-column 79)

;; BACKUP
;; ----------------------------------------------------------------------------

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

;; LITTLE HELPERS
;; ----------------------------------------------------------------------------

;; Automatically indent new lines
(electric-indent-mode t)

;; Typed text replaces the selection
(delete-selection-mode t)

;; Show matched parentheses
(show-paren-mode t)

;; RUSSIAN MAC KEYBOARD
;; ----------------------------------------------------------------------------

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

;; MY VARIABLES
;; ----------------------------------------------------------------------------

;; The same keys for ace-window and avy
(setq my-ace-avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;; MY FUNCTIONS
;; ----------------------------------------------------------------------------

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
         (ss (substring (thing-at-point 'line t) 0 pos))
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

(defun my-heading ()
  "Create a pretty heading."
  (interactive)
  (move-beginning-of-line nil)
  (let* ((comment-symbols '(?# ?% ?/ ?\;))
         (default-comment-symbol (car comment-symbols))
         (char-at-beginning-of-line (char-after))
         (comment-symbol (if (member char-at-beginning-of-line comment-symbols)
                             char-at-beginning-of-line
                           default-comment-symbol))
         (comment-symbol-str (char-to-string comment-symbol))
         (comment-str (concat comment-symbol-str comment-symbol-str " "))
         (comment-str-sep (make-string 76 ?-)))

    (cl-loop
     (let ((ch (char-after)))
       (if (or (eq ch ? ) (eq ch comment-symbol))
           (delete-char 1)
         (cl-return))))

    (upcase-region (line-beginning-position) (line-end-position))
    (move-beginning-of-line 1)
    (insert comment-str)
    (end-of-line)
    (newline)
    (insert comment-str comment-str-sep)
    (newline)
    (newline)))

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

;; PACKAGES
;; ----------------------------------------------------------------------------

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
  (setq avy-all-windows nil
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

;; KEYS
;; ----------------------------------------------------------------------------

;; Enhanced C-a
(global-set-key (kbd "C-a") 'my-move-beginning-of-line)
(define-key visual-line-mode-map (kbd "C-a") 'beginning-of-visual-line)

;; Easier scrolling
(global-set-key (kbd "C-z") 'scroll-down-command)
(global-set-key (kbd "C-M-z") 'my-scroll-other-window-down)

;; List all buffers in the current window
(global-set-key (kbd "C-x C-b") 'buffer-menu)

;; Alias to C-x C-f
(global-set-key (kbd "C-x f") 'find-file)

;; Stuff in other window
(define-prefix-command 'my-c-o)
(global-set-key (kbd "C-o") 'my-c-o)
(define-key my-c-o (kbd "b") 'switch-to-buffer-other-window)
(define-key my-c-o (kbd "d") 'dired-other-window)
(define-key my-c-o (kbd "f") 'find-file-other-window)
(define-key my-c-o (kbd "C-f") 'find-file-other-window)
(define-key my-c-o (kbd "r") 'find-file-read-only-other-window)
(define-key my-c-o (kbd "C-r") 'find-file-read-only-other-window)
(define-key my-c-o (kbd "o") 'ace-window)
(define-key my-c-o (kbd "C-o") 'ace-window)

;; Better 'other-window. Alias to C-o C-o
(global-set-key (kbd "C-x o") 'ace-window)

;; Better 'move-to-window-line-top-bottom
(global-set-key (kbd "M-r") 'avy-goto-line)

;; Better than tons of unused commands
(global-set-key (kbd "M-s") 'avy-goto-word-1)

;; Replace 'mark-paragraph
(global-set-key (kbd "M-h") 'my-heading)

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
   (define-key ido-completion-map (kbd "<f1> f") 'smex-describe-function)))

;; Replace 'execute-extended-command
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; THE END
;; ----------------------------------------------------------------------------
