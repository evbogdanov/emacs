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
see its code to understand what's going on here."
  (interactive)
  (if (= 0 (length isearch-string))
      (ding)
    ;; The only difference is how i set isearch-string
    (setq isearch-string  (my-str-kill-last-word isearch-string)
          isearch-message (mapconcat 'isearch-text-char-description
                                     isearch-string "")))
  ;; Next lines are shamelessly copy/pasted from isearch-del-char
  (if isearch-other-end (goto-char isearch-other-end))
  (isearch-search)
  (isearch-push-state)
  (isearch-update))

;; PACKAGES
;; ----------------------------------------------------------------------------

(use-package bind-key
  :ensure t)

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

;; KEYS
;; ----------------------------------------------------------------------------

;; Enhanced C-a
(bind-key "C-a" 'my-move-beginning-of-line)
(bind-keys :map visual-line-mode-map
           ("C-a" . beginning-of-visual-line))

;; Easier scrolling
(bind-key "C-z" 'scroll-down-command)
(bind-key "C-M-z" 'my-scroll-other-window-down)

;; List all buffers in the current window
(bind-key "C-x C-b" 'buffer-menu)

;; Alias to C-x C-f
(bind-key "C-x f" 'find-file)

;; Stuff in other window
(bind-keys :prefix-map my-other-window-map
           :prefix "C-o"
           ("b"   . switch-to-buffer-other-window)
           ("d"   . dired-other-window)
           ("f"   . find-file-other-window)
           ("C-f" . find-file-other-window)
           ("r"   . find-file-read-only-other-window)
           ("C-r" . find-file-read-only-other-window)
           ("o"   . ace-window)
           ("C-o" . ace-window))

;; Better 'other-window. Alias to C-o C-o
(bind-key "C-x o" 'ace-window)

;; Better 'move-to-window-line-top-bottom
(bind-key "M-r" 'avy-goto-line)

;; Better than tons of unused commands
(bind-key "M-s" 'avy-goto-word-1)

;; Replace 'mark-paragraph
(bind-key "M-h" 'my-heading)

;; Make DEL and M-DEL act as they do everywhere else
(bind-keys :map isearch-mode-map
           ("DEL"   . isearch-del-char)
           ("M-DEL" . my-isearch-del-word))

;; THE END
;; ----------------------------------------------------------------------------
