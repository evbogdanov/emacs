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
;;; My variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-var-ace-avy-keys (number-sequence ?a ?z)
  "The same keys for `ace-window' and `avy'.")

(defvar my-var-self-closing-tag-style " /"
  "How to close tags like <br /> and <input />")

(defvar my-working-directory default-directory
  "Remember the directory where Emacs was started.")

(defvar my-working-directory-abs
  (concat (getenv "HOME") (substring my-working-directory 1))
  "Absolute path to my working directory.")

(defvar my-spendings-categories nil
  "List of spending categories.")

(defvar my-spendings-categories-file "~/Dropbox/spendings/list-of-categories"
  "Where to load spending categories from.")

(defvar my-spendings-file "~/Dropbox/spendings/spendings.js"
  "Where my spendings are.")


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tabs and spaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Indent with 2 spaces!
;; (but make it easier to distinguish between tabs and spaces)
(setq-default tab-width 8)

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
;;; Performance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Garbage collection will happen happen on each 32MB (default was 0.8MB)
(setq gc-cons-threshold 32000000)

;; Warn when opening files bigger than 64MB (default was 10MB)
(setq large-file-warning-threshold 64000000)

;; Always load newest byte code
(setq load-prefer-newer t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Little helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Automatically refresh buffers when underlying files are changed externally
(global-auto-revert-mode t)

;; Automatically refresh dired (quietly!)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Automatically indent new lines
(electric-indent-mode t)

;; Automatically close pairs: ', ", (, {, [
(electric-pair-mode t)

;; Typed text replaces the selection
(delete-selection-mode t)

;; Show matched parentheses
(show-paren-mode t)

;; Easily navigate CamelCasedWords
(global-subword-mode t)


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

(defun my-scroll-other-window-down ()
  "Scroll text of other window down."
  (interactive)
  (scroll-other-window `-))

(defun my-css-heading ()
  "Create CSS heading."
  (move-beginning-of-line 1)
  (insert "/******************************************************************************")
  (newline)
  (insert " ** ")
  (end-of-line)
  (newline)
  (insert " ******************************************************************************/")
  (newline))

(defun my-heading-with-symbol (comment-symbol)
  "Create heading with a given comment symbol."
  (let* ((comment-symbol-str (char-to-string comment-symbol))
         (comment-str (concat comment-symbol-str comment-symbol-str
                              comment-symbol-str " "))
         (comment-str-sep (make-string 80 comment-symbol)))
    (move-beginning-of-line 1)
    (insert comment-str-sep)
    (newline)
    (insert comment-str)
    (end-of-line)
    (newline)
    (insert comment-str-sep)
    (newline)))

(defun my-heading ()
  "Create a pretty heading."
  (interactive)
  (cond ((string= major-mode "css-mode") (my-css-heading))
        ((null comment-start) (my-heading-with-symbol ?#))
        (t (my-heading-with-symbol (aref comment-start 0)))))

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

(defun my-mark-word ()
  "Better `mark-word', IMO."
  (interactive)
  (forward-word)
  (backward-word)
  (mark-word)
  (exchange-point-and-mark))

(defun my-mark-word-and-copy ()
  "I copy the marked word quite often. Create shortcut for it!"
  (interactive)
  (save-excursion
    (my-mark-word)
    (kill-ring-save (point) (mark))))

(defun my-mark-line ()
  "Function name says it all."
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))

(defun my-mark-line-and-copy ()
  "I copy lines quite often!"
  (interactive)
  (save-excursion
    (my-mark-line)
    (kill-ring-save (point) (mark))))

(defun my-mark-whole-buffer-and-copy ()
  "Copy it all!"
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (kill-ring-save (point) (mark))))

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
  (cond ((or (string= major-mode "lisp-mode")
             (string= major-mode "emacs-lisp-mode")) (eval-buffer))
        (t (my-eval-buffer-interpreted))))

(defun my-eval-buffer-interpreted ()
  "Feed current buffer to some interpreter."
  (let ((modes-and-interpreters '(("js-mode" . "node")
                                  ("ruby-mode" . "ruby")
                                  ("python-mode" . "python3")
                                  ("sh-mode" . "bash")))
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

(defun my-windmove-furthest (dir)
  "Select the furthest window to the 'left, 'right, 'up or 'down."
  (let (other-window)
    (cl-loop (setq other-window (windmove-find-other-window dir))
             (if (null other-window)
                 (cl-return)
               (windmove-do-window-select dir)))))

(defun my-windmove-leftmost ()
  "Select the leftmost window."
  (interactive)
  (my-windmove-furthest 'left))

(defun my-windmove-rightmost ()
  "Select the rightmost window."
  (interactive)
  (my-windmove-furthest 'right))

(defun my-move-to-neotree-and-refresh ()
  "Select the window containing Neotree buffer (the leftmost one)
and refresh it."
  (interactive)
  (my-windmove-leftmost)
  (if (string= (buffer-name) " *NeoTree*")
      (neotree-refresh)
    (user-error "No buffer called NeoTree")))

(defun my-move-line (direction previous-line-n-times)
  "Do line moving."

  ;; Content should fit in one line. Otherwise, I'll have a nasty bug on a
  ;; narrow screen.
  (toggle-truncate-lines)

  (when (eq direction 'down) (next-line))
  (transpose-lines 1)
  (previous-line previous-line-n-times)
  (end-of-line)

  ;; Bring my beautiful untruncated lines back.
  (toggle-truncate-lines))

(defun my-transpose-lines () (interactive) (my-move-line 'up 1))
(defun my-move-line-up    () (interactive) (my-move-line 'up 2))
(defun my-move-line-down  () (interactive) (my-move-line 'down 1))

(defun my-join-line ()
  "Join line forward in the manner of WebStorm."
  (interactive)
  (join-line 1))

(defun my-dired-at-point ()
  "Open the thing at point inside `dired'"
  (interactive)
  (let ((dir (thing-at-point 'filename)))
    (dired dir)))

(defun my-dired-open-working-directory ()
  "Quickly go to my working directory."
  (interactive)
  (dired my-working-directory))

(defun my-dired-open-working-directory-in-other-window ()
  "Quickly go to my working directory in another window."
  (interactive)
  (dired-other-window my-working-directory))

(defun my-dired-hide-file ()
  "In Dired, hide the file (or directory) on this line in another window.
Useful when I did `dired-display-file' and then want to hide it."
  (interactive)
  (dired-find-file-other-window)
  (kill-buffer)
  (delete-window))

(defun my-ibuffer-hide-buffer ()
  "In IBuffer, hide the buffer on this line in another window.
Useful when I did `ibuffer-visit-buffer-other-window-noselect' and then want to hide it."
  (interactive)
  (ibuffer-visit-buffer-other-window)
  (kill-buffer)
  (delete-window)
  (ibuffer-update nil))

(defun my-emmet-make-self-closing-tag ()
  "Convert <MyTag>`point'</MyTag> to <MyTag/>."
  (zap-to-char 1 ?>)
  (backward-char)
  (insert my-var-self-closing-tag-style)
  (forward-char))

(defun my-emmet-expand-line (arg)
  "Tweak `emmet-expand-line' for a better JSX support."
  (interactive "P")
  (if (and (eq (char-before) ?>)
           (eq (char-after) ?<))
      (my-emmet-make-self-closing-tag)
    ;; In JSX, I want 'className' instead of 'class'
    (when (string= major-mode "js-jsx-mode")
      (setq emmet-expand-jsx-className? t))
    (emmet-expand-line arg)
    ;; Switch back to 'class'
    (setq emmet-expand-jsx-className? nil)))

(defun my-prettify-buffer ()
  "Turn tabs into spaces, ditch trailing whitespace, and indent a whole buffer."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max)))

(defun my-delete-line ()
  "Delete the current line without copying it."
  (interactive)
  (delete-region (line-beginning-position)
                 (line-end-position))
  (save-excursion
    (move-end-of-line nil)
    (if (eobp) (delete-char -1)
      (delete-char 1))))

(defun my-find-recent-file ()
  "Find a recent file."
  (interactive)
  (if (find-file (ido-completing-read "Recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(defun my-find-recent-dir ()
  "Find recently visited dired directory."
  (interactive)
  (dired (ido-completing-read "Recent directory: " dired-recent-directories)))

(defun my-do-grep (&optional is-working-directory)
  "My own grepping."
  (when is-working-directory
    (cd my-working-directory-abs))
  (let* ((input (read-shell-command "Grep: "))
         (wd (if (null is-working-directory) "" my-working-directory))
         (ag "ag --hidden --nocolor --nogroup --path-to-ignore ~/github/dotfiles/.ignore_global")
         (maybe-sed (if (null is-working-directory) ""
                      (concat " | sed 's|^" my-working-directory-abs "||'")))
         (cmd (concat ag " " input " " wd maybe-sed))
         (grep-use-null-device nil)) ;; don't append /dev/null to `cmd`'
    (grep cmd)
    (switch-to-buffer "*grep*")
    (delete-other-windows)))

(defun my-grep-working-directory ()
  "Grep files in my working directory."
  (interactive)
  (my-do-grep t))

(defun my-grep ()
  "Grep files relative to `default-directory'."
  (interactive)
  (my-do-grep))

(defun my-buffer-contains-string (string)
  "Check if current buffer contains a string."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (search-forward string nil t))))

(defun my-js-mode-activate-jsx ()
  "Maybe activate JSX inside JS mode."
  (interactive)
  (cond ((string= major-mode "js-mode") (js-jsx-mode))
        ((string= major-mode "js-jsx-mode") (message "You're in JSX mode"))
        (t (message "Not in JS mode"))))

(defun my-spendings-load-categories ()
  "Load list of spending categories from `my-spendings-categories-file'."
  (interactive)
  (when (file-readable-p my-spendings-categories-file)
    (with-temp-buffer
      (insert-file-contents my-spendings-categories-file)
      (goto-char (point-min))
      (setq my-spendings-categories (read (current-buffer))))))

(defun my-spendings-select-category ()
  "Select spending category."
  (when (null my-spendings-categories)
    (my-spendings-load-categories))
  (ido-completing-read "Category: " my-spendings-categories))

(defun my-spendings-add ()
  "Add a new entry to the list of spendings."
  (interactive)
  (let ((date (read-from-minibuffer "Date: " (format-time-string "%Y-%m-%d")))
        (money (read-from-minibuffer "Money: "))
        (category (my-spendings-select-category))
        (notes (read-from-minibuffer "Notes: ")))
    (find-file my-spendings-file)
    (end-of-buffer)
    (search-backward "]")
    (move-beginning-of-line 1)
    (insert (concat "  {
    date: '" date "',
    money: " money ",
    category: " category ",
    notes: '" notes "',
  },
"))
    (save-buffer)))

(defun my-start-searching-symbol-at-point-backward ()
  "Start searching symbol at point backward."
  (interactive)
  (isearch-forward-symbol-at-point)
  (isearch-repeat-backward)
  (isearch-repeat-backward))

(defun my-start-searching-symbol-at-point-forward ()
  "Start searching symbol at point forward."
  (interactive)
  (isearch-forward-symbol-at-point)
  (isearch-repeat-forward))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package osx-clipboard
  :ensure t
  :config
  (osx-clipboard-mode +1))

;; `my-windmove-' functions won't work without enabling `windmove' first
(use-package windmove
  :ensure t)

(use-package ace-window
  :ensure t
  :config
  (setq aw-keys my-var-ace-avy-keys))

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
        avy-keys my-var-ace-avy-keys))

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
  :ensure t)

(use-package emmet-mode
  :ensure t
  :config
  (setq emmet-self-closing-tag-style my-var-self-closing-tag-style)
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode)
  (add-hook 'js-jsx-mode-hook 'emmet-mode)
  (define-key emmet-mode-keymap (kbd "C-j") 'my-emmet-expand-line))

(use-package neotree
  :ensure t
  :config
  (setq-default neo-show-hidden-files t)
  (setq-default neo-theme 'ascii)
  (define-key neotree-mode-map (kbd "F") 'neotree-change-root)
  (define-key neotree-mode-map (kbd "f")
    (neotree-make-executor :file-fn 'neo-open-file
                           :dir-fn  'neo-open-dir))
  (define-key neotree-mode-map (kbd "b") 'neotree-select-up-node)

  ;; Magit style:
  (define-key neotree-mode-map (kbd "M-p") 'neotree-select-previous-sibling-node)
  (define-key neotree-mode-map (kbd "M-n") 'neotree-select-next-sibling-node)

  ;; Dired style:
  (define-key neotree-mode-map (kbd "+") 'neotree-create-node)
  (define-key neotree-mode-map (kbd "C") 'neotree-copy-node)
  (define-key neotree-mode-map (kbd "R") 'neotree-rename-node)
  (define-key neotree-mode-map (kbd "D") 'neotree-delete-node))

(use-package dired
  :config
  (add-hook 'dired-mode-hook
            (lambda ()
              ;; I'm sick and tired of hiding details manually by pressing `('
              ;; all the time. Dired, please hide details for me.
              (dired-hide-details-mode)))

  ;; Open files and directories right in the dired buffer
  (put 'dired-find-alternate-file 'disabled nil)

  ;; Don't mess with my keybindings
  (define-key dired-mode-map (kbd "C-o") nil)

  ;; Display or hide file in another window
  (define-key dired-mode-map (kbd "SPC") 'dired-display-file)
  (define-key dired-mode-map (kbd "h") 'my-dired-hide-file)

  ;; Go directory/file forward
  (define-key dired-mode-map (kbd "f") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)

  ;; Go directory backward
  (define-key dired-mode-map (kbd "b") (lambda ()
                                         (interactive)
                                         (find-alternate-file "..")))

  ;; Edit dired buffer with wdired (use same keybinding as occur and wgrep)
  (define-key dired-mode-map (kbd "e") 'dired-toggle-read-only)

  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  ;; Use `ls` from GNU (get it: `brew install coreutils`)
  (setq insert-directory-program (executable-find "gls"))
  ;; By default it's just `-al`, but I really want to list directories first
  (setq dired-listing-switches "-al --group-directories-first")

  ;; Dired Extra (enables dired-jump C-x C-j)
  (require 'dired-x)

  ;; Move files between two dired buffers
  (setq dired-dwim-target t))

(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :config
  ;; Disable prompt when I insert ```
  (define-key gfm-mode-map (kbd "`") nil))

(use-package magit
  :ensure t
  :bind (("C-x m" . magit-status))
  :config
  ;; Magit wants `C-x g` to show me git status. I don't think so.
  (define-key magit-file-mode-map (kbd "C-x g") nil))

;; Tweak `grep' (which is built on top of `compile' mode)
(use-package compile
  :config
  (define-key compilation-minor-mode-map "\C-o" nil)
  (define-key compilation-minor-mode-map "o" 'compilation-display-error))

(use-package ibuffer
  :config
  (define-key ibuffer-mode-map (kbd "C-o") nil)
  (define-key ibuffer-mode-map (kbd "M-o") nil)
  (define-key ibuffer-mode-map (kbd "SPC") 'ibuffer-visit-buffer-other-window-noselect)
  (define-key ibuffer-mode-map (kbd "h") 'my-ibuffer-hide-buffer))

(use-package recentf
  :init (recentf-mode t)
  :config
  (setq recentf-max-saved-items 500))

(use-package dired-recent
  :ensure t
  :init (dired-recent-mode 1)
  :config
  (setq dired-recent-max-directories 500)
  (define-key dired-recent-mode-map (kbd "C-x C-d") 'my-find-recent-dir))

(use-package yaml-mode
  :ensure t)

(use-package wgrep
  :ensure t
  :config
  ;; Use same keybinding as occur
  (setq wgrep-enable-key "e"))

(use-package css-mode
  :config
  (setq css-indent-offset 2))

(use-package js
  :config
  (add-hook 'js-mode-hook
            (lambda ()
              (when (my-buffer-contains-string "import React")
                (my-js-mode-activate-jsx))))

  (setq js-indent-level 2)

  (define-key js-mode-map (kbd "C-c x") 'my-js-mode-activate-jsx)
  (define-key js-mode-map (kbd "M-.") nil)

  (define-key js-jsx-mode-map (kbd "C-M-a") 'my-move-beginning-of-tag)
  (define-key js-jsx-mode-map (kbd "C-M-e") 'my-move-after-tag))

(use-package typescript-mode
  :ensure t
  :config
  (setq typescript-indent-level 2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Easier scrolling
(global-set-key (kbd "C-z") 'scroll-down-command)
(global-set-key (kbd "C-M-z") 'my-scroll-other-window-down)

;; Replace `list-buffers' with `ibuffer'
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Stuff in other window
(define-prefix-command 'my-other-win-prefix)
(global-set-key (kbd "C-o") 'my-other-win-prefix)
(define-key my-other-win-prefix (kbd "C-o") 'ace-window)
(define-key my-other-win-prefix (kbd "b") 'switch-to-buffer-other-window)
(define-key my-other-win-prefix (kbd "d d") 'dired-other-window)
(define-key my-other-win-prefix (kbd "d /") 'my-dired-open-working-directory-in-other-window)
(define-key my-other-win-prefix (kbd "f") 'find-file-other-window)
(define-key my-other-win-prefix (kbd "C-f") 'windmove-right)
(define-key my-other-win-prefix (kbd "C-b") 'windmove-left)
(define-key my-other-win-prefix (kbd "C-p") 'windmove-up)
(define-key my-other-win-prefix (kbd "C-n") 'windmove-down)
(define-key my-other-win-prefix (kbd "C-a") 'my-windmove-leftmost)
(define-key my-other-win-prefix (kbd "C-e") 'my-windmove-rightmost)
(define-key my-other-win-prefix (kbd "C-s") 'my-ace-window-swap)
(define-key my-other-win-prefix (kbd "t") 'neotree-toggle)
(define-key my-other-win-prefix (kbd "C-t") 'my-move-to-neotree-and-refresh)
(define-key my-other-win-prefix (kbd "C-j") 'dired-jump-other-window)

;; Open different things in Dired
(define-prefix-command 'my-dired-prefix)
(global-set-key (kbd "C-x d") 'my-dired-prefix)
(define-key my-dired-prefix (kbd "d") 'ido-dired)
(define-key my-dired-prefix (kbd "/") 'my-dired-open-working-directory)
(define-key my-dired-prefix (kbd ".") 'my-dired-at-point)

;; Shortcut for C-o C-o
(global-set-key (kbd "M-o") 'ace-window)

;; Better 'move-to-window-line-top-bottom
(global-set-key (kbd "M-r") 'avy-goto-line)
;; It kinda like C-] in bash
(global-set-key (kbd "C-]") 'avy-goto-word-or-subword-1)

;; Keys in `occur' buffer
(define-key occur-mode-map "\C-o" nil)
(define-key occur-mode-map "p" 'occur-prev)
(define-key occur-mode-map "n" 'occur-next)

;; Enable C-x C-u and C-x C-l.
;; Add similar combo for heading (C-x C-h is undefined by default)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(global-set-key (kbd "C-x C-h") 'my-heading)

;; Prettify code
(global-set-key (kbd "C-x p") 'my-prettify-buffer)

;; Replace 'execute-extended-command
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Marking
;; Why M-h for marking? Firstly, "h" can stand for "highlight". Secondly, M-h
;; used to mark by default (but only paragraph).
(define-prefix-command 'my-mark-key)
(global-set-key (kbd "M-h") 'my-mark-key)
(define-key my-mark-key (kbd "M-w") 'my-mark-word-and-copy)
(define-key my-mark-key (kbd "w") 'my-mark-word)
(define-key my-mark-key (kbd "l") 'my-mark-line)
(define-key my-mark-key (kbd "M-l") 'my-mark-line-and-copy)
(define-key my-mark-key (kbd "p") 'my-mark-paragraph)
(define-key my-mark-key (kbd "a") 'mark-whole-buffer) ;; aka "Mark [A]ll"
(define-key my-mark-key (kbd "M-a") 'my-mark-whole-buffer-and-copy)
(define-key my-mark-key (kbd "r") 'rectangle-mark-mode)

;; In WebStorm, it's called "extend / shrink selection"
(global-set-key (kbd "M-.") 'er/expand-region)
(global-set-key (kbd "M-,") 'er/contract-region)

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

;; Moving lines
(global-set-key (kbd "C-x C-t") 'my-transpose-lines) ; was `transpose-lines`
(global-set-key (kbd "ESC <down>") 'my-move-line-down)
(global-set-key (kbd "ESC <up>") 'my-move-line-up)

;; Join line
(global-set-key (kbd "M-j") 'my-join-line)
(global-set-key (kbd "M-J") 'join-line)

;; macOS-like behaviour of Fn+Left/Right arrows
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)

;; `hippie-expand' instead of `dabbrev-expand'
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))
(global-set-key (kbd "M-/") 'hippie-expand)

;; Tweak isearch
(define-key isearch-mode-map (kbd "DEL") 'isearch-del-char)
(define-key isearch-mode-map (kbd "M-DEL") 'my-isearch-del-word)
(define-key isearch-mode-map (kbd "M-k") 'my-isearch-del-line)

;; M-k used to execute `kill-sentence' -- what a waste!
(global-set-key (kbd "M-k") 'my-delete-line)

;; Find recent files (similar to C-x C-d for directories)
(global-set-key (kbd "C-x C-r") 'my-find-recent-file)

;; Add more useful `M-s ...` commands
(define-key search-map (kbd "C-r") 'my-start-searching-symbol-at-point-backward)
(define-key search-map (kbd "C-s") 'my-start-searching-symbol-at-point-forward)
(define-key search-map (kbd "s") 'my-grep)
(define-key search-map (kbd "M-s") 'my-grep-working-directory)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
