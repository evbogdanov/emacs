;; DEFUNS
;; -----------------------------------------------------------------------------

(defun my-open-string-as-url ()
  "Open string under cursor as URL."
  (interactive)
  (if (not (eq system-type 'darwin))
      (error "Sorry, macOS only")
    (let ((str (thing-at-point 'url))
          (cmd "open "))
      (if (null str)
          (error "URL not found")
        (shell-command (concat cmd str))))))

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

(defun my-heading ()
  "Create heading via `h+` script."
  (interactive)
  (shell-command-on-region
   (line-beginning-position) (line-end-position) "h+" t t))

(defun my-no-space ()
  "Delete all spaces and tabs around point."
  (interactive)
  (just-one-space 0))

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
    ;; the only difference is how i set isearch-string
    (setq isearch-string  (my-str-kill-last-word isearch-string)
          isearch-message (mapconcat 'isearch-text-char-description
                                     isearch-string "")))
  ;; next lines are shamelessly copy/pasted from isearch-del-char
  (if isearch-other-end (goto-char isearch-other-end))
  (isearch-search)
  (isearch-push-state)
  (isearch-update))

(defun my-scroll-other-window-down ()
  "Scroll text of other window down."
  (interactive)
  (scroll-other-window `-))

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

(defun my-pipe-region (start end command)
  "Pipe region through a shell command."
  (interactive (let (cmd)
                 (unless (mark)
                   (error "No mark, no region"))
                 (setq cmd (read-shell-command "Pipe through command: "))
                 (list (region-beginning) (region-end) cmd)))
  (shell-command-on-region start end command t t))

(defun my-pipe-buffer ()
  "Pipe a whole buffer through a shell command. A buffer content
will be replaced."
  (interactive)
  (let ((start (point-min))
        (end (point-max))
        (cmd (read-shell-command "pipe through command: ")))
    (shell-command-on-region start end cmd t t)))

(defun my-pipe-buffer-to-echo-area ()
  "Pipe a whole buffer through a shell command (don't replace buffer's content)"
  (interactive)
  (let ((start (point-min))
        (end (point-max))
        (cmd (read-shell-command "pipe through command: ")))
    (shell-command-on-region start end cmd)))

(defun my-mark-line ()
  "Function name says it all."
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))

(defun my-mark-word ()
  "Better 'mark-word', IMO."
  (interactive)
  (forward-word)
  (backward-word)
  (mark-word)
  (exchange-point-and-mark))

(defun my-eval-buffer-as (interpreter)
  "Feed current buffer to some interpreter (Perl, Node, etc)"
  (shell-command-on-region 1 (point-max) interpreter))

(defun my-eval-buffer-as-perl () (interactive) (my-eval-buffer-as "perl"))
(defun my-eval-buffer-as-node () (interactive) (my-eval-buffer-as "node"))
