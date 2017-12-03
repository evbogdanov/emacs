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
(setq-default fill-column 79)


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
(setq my-ace-avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; My functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun my-eval-buffer (interpreter)
  "Feed current buffer to some interpreter (Python, Bash, etc)."
  (shell-command-on-region 1 (point-max) interpreter))

(defun my-eval-buffer-python3 ()
  "Feed current buffer to Python 3 interpreter."
  (interactive)
  (my-eval-buffer "python3"))

(defun my-eval-buffer-node ()
  "Feed current buffer to Node.js interpreter."
  (interactive)
  (my-eval-buffer "node"))

(defun my-eval-buffer-bash ()
  "Feed current buffer to Bash interpreter."
  (interactive)
  (my-eval-buffer "bash"))

(defun my-eval-buffer-tidy ()
  "Validate HTML with a little help from http://www.html-tidy.org"
  (interactive)
  (my-eval-buffer "tidy"))

(defun my-open-url ()
  "Open string under cursor as URL."
  (interactive)
  (if (not (eq system-type 'darwin))
      (error "Sorry, macOS only")
    (let ((str (thing-at-point 'url))
          (cmd "open "))
      (if (null str)
          (error "URL not found")
        (shell-command (concat cmd str))))))

(defun my-open-url-search (base-url)
  "Search something on a specific website. Target word(s) are chosen
from either selection or user input."
  (let* ((q (if (use-region-p)
                (buffer-substring (region-beginning) (region-end))
              (read-string "Search: ")))
         (q-encoded (url-encode-url q))
         (buff-name "*eww*"))
    (when (not (string= (buffer-name) buff-name))
      (switch-to-buffer-other-window buff-name))
    (eww (concat base-url q-encoded))))

(defun my-open-url-search-macmillandictionary ()
  "Search something on macmillandictionary.com"
  (interactive)
  (my-open-url-search
   "http://www.macmillandictionary.com/search/british/direct/?q="))

(defun my-open-url-search-urbandictionary ()
  "Search something on urbandictionary.com"
  (interactive)
  (my-open-url-search "http://www.urbandictionary.com/define.php?term="))

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

(use-package expand-region
  :ensure t
  :config
  (setq expand-region-contract-fast-key ","))

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; Piping
(define-prefix-command 'my-m-spc)
(global-set-key (kbd "M-SPC") 'my-m-spc)
(define-key my-m-spc (kbd "M-SPC") 'my-pipe-replace)
(define-key my-m-spc (kbd ".") 'my-pipe-do-not-replace)
(define-key my-m-spc (kbd "e") 'eval-buffer)
(define-key my-m-spc (kbd "p") 'my-eval-buffer-python3)
(define-key my-m-spc (kbd "n") 'my-eval-buffer-node)
(define-key my-m-spc (kbd "b") 'my-eval-buffer-bash)
(define-key my-m-spc (kbd "t") 'my-eval-buffer-tidy)

;; URL openings
(define-prefix-command 'my-m-o)
(global-set-key (kbd "M-o") 'my-m-o)
(define-key my-m-o (kbd "M-o") 'my-open-url)
(define-key my-m-o (kbd "m") 'my-open-url-search-macmillandictionary)
(define-key my-m-o (kbd "u") 'my-open-url-search-urbandictionary)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dired
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'dired)

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq c-default-style "k&r"
      c-basic-offset 4)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Front end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq css-indent-offset 2)
(setq js-indent-level 2)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
