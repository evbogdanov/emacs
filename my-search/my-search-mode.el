;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst my-search-buffer-name "*my-search*"
  "Name of the buffer with `my-search' interface.")

(defconst my-search-what-label " What:"
  "Label for WHAT variable.")

(defconst my-search-where-label "Where:"
  "Label for WHERE variable.")

(defconst my-search-regex-label "Regex:"
  "Label for REGEX variable.")

(defconst my-search-regex-default-value "no"
  "Initial value for REGEX variable. I only expect 'no' or 'yes'")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-search-make-read-only (start end)
  "Make text between START and END read-only."
  (add-text-properties start end '(face bold))
  (add-text-properties start end '(read-only t)))

(defun my-search-get-what-label ()
  "Get START and END for WHAT label."
  (let* ((start (point-min))
         (end (+ start (length my-search-what-label))))
    (list start end)))

(defun my-search-get-where-label ()
  "Get START and END for WHERE label."
  (save-excursion
    (goto-char (point-min))
    (forward-line 1)
    (unless (search-forward my-search-where-label nil t)
      (user-error "Cannot find `my-search-where-label'"))
    (list (match-beginning 0) (match-end 0))))

(defun my-search-get-regex-label ()
  "Get START and END for REGEX label."
  (save-excursion
    (goto-char (point-min))
    (forward-line 2)
    (unless (search-forward my-search-regex-label nil t)
      (user-error "Cannot find `my-search-regex-label'"))
    (list (match-beginning 0) (match-end 0))))

(defun my-search-make-what-label-read-only ()
  "Make WHAT label read-only."
  (let* ((start-end (my-search-get-what-label))
         (start (car start-end))
         (end (cadr start-end)))
    (my-search-make-read-only start end)))

(defun my-search-make-where-label-read-only ()
  "Make WHERE label read-only."
  (let* ((start-end (my-search-get-where-label))
         (start (car start-end))
         (end (cadr start-end)))
    (my-search-make-read-only start end)))

(defun my-search-make-regex-label-read-only ()
  "Make REGEX label read-only."
  (let* ((start-end (my-search-get-regex-label))
         (start (car start-end))
         (end (cadr start-end)))
    (my-search-make-read-only start end)))

(defun my-search-erase ()
  "Erases *my-search* buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun my-search-prepare ()
  "Prepare *my-search* buffer for searching."
  (insert (concat my-search-what-label " \n"
                  my-search-where-label " " default-directory "\n"
                  my-search-regex-label " " my-search-regex-default-value))

  (my-search-make-what-label-read-only)
  (my-search-make-where-label-read-only)
  (my-search-make-regex-label-read-only)

  (goto-char (point-min))
  (end-of-line))

(defun my-search-get-what-value ()
  "Get the input: WHAT to search."
  (save-excursion
    (goto-char (point-min))
    (unless (search-forward my-search-what-label nil t)
      (user-error "Cannot find WHAT label"))
    (string-trim (buffer-substring (match-end 0) (line-end-position)))))

(defun my-search-get-where-value ()
  "Get the input: WHERE to search"
  (save-excursion
    (goto-char (point-min))
    (forward-line 1)
    (unless (search-forward my-search-where-label nil t)
      (user-error "Cannot find WHERE label"))
    (string-trim (buffer-substring (match-end 0) (line-end-position)))))

(defun my-search-get-regex-p ()
  "Get the input: should I use REGEX for search?"
  (save-excursion
    (goto-char (point-min))
    (forward-line 2)
    (unless (search-forward my-search-regex-label nil t)
      (user-error "Cannot find REGEX label"))
    (let ((yes-prefix "y") ;; allow Y or YES or YEP as true values
          (regex-value (string-trim (buffer-substring (match-end 0) (line-end-position))))
          (ignore-case t))
      (string-prefix-p yes-prefix
                       regex-value
                       ignore-case))))

(defun my-search-set-what-value (what-value)
  "Set the input: WHAT to search"
  (save-excursion
    (goto-char (point-min))
    (unless (search-forward my-search-what-label nil t)
      (user-error "Cannot find WHAT label"))
    (delete-region (1+ (match-end 0)) (line-end-position))
    (end-of-line)
    (insert what-value)))

(defun my-search-do-search ()
  "Do search!"
  (interactive)
  (let ((what-value (my-search-get-what-value))
        (where-value (my-search-get-where-value))
        (is-regex (my-search-get-regex-p)))
    (message "My search: \"%s\" in \"%s\" (regex? %s)" what-value where-value is-regex)
    (my-grep what-value where-value is-regex)))

;;;###autoload
(defun my-search ()
  "Open or switch to the *my-search* buffer and activate `my-search-mode'.
If there's selected text, use it as WHAT to search."
  (interactive)

  (let ((buf (get-buffer-create my-search-buffer-name))
        (selected-text nil))
    (when (use-region-p)
      (setq selected-text (buffer-substring-no-properties (region-beginning)
                                                          (region-end))))
    (with-current-buffer buf
      (unless (eq major-mode 'my-search-mode)
        (message "my-search: making buffer...")
        (my-search-erase)
        (my-search-prepare)
        (my-search-mode)))

    (switch-to-buffer-other-window buf)

    (when selected-text
      (message "my-search: using SELECTED-TEXT: %s" selected-text)
      (my-search-set-what-value selected-text))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Make keymap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'my-search-do-search)
    map)
  "Keymap for `my-search-mode'.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Define mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(define-derived-mode my-search-mode fundamental-mode "my-search"
  "Major mode for my searching."
  (use-local-map my-search-mode-map))

(provide 'my-search-mode)
