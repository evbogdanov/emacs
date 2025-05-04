;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst my-search-buffer-name "*my-search*"
  "Name of the buffer with `my-search' interface.")

(defconst my-search-what-str " What:"
  "Label for WHAT to search")

(defconst my-search-where-str "Where:"
  "Label for WHERE to search")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-search-make-read-only (start end)
  (add-text-properties start end '(face bold))
  (add-text-properties start end '(read-only t)))


(defun my-search-remove-read-only (start end)
  (let ((inhibit-read-only t))
    (remove-text-properties start end '(read-only t))
    (remove-text-properties start end '(face bold))))


(defun my-search-get-what ()
  (let* ((start (point-min))
         (end (+ start (length my-search-what-str))))
    (list start end)))


(defun my-search-get-where ()
  (save-excursion
    (goto-char (point-min))
    (forward-line 1)
    (unless (search-forward my-search-where-str nil t)
      (user-error "Cannot find `my-search-where-str'"))
    (list (match-beginning 0) (match-end 0))))


(defun my-search-make-what-read-only ()
  (let* ((start-end (my-search-get-what))
         (start (car start-end))
         (end (cadr start-end)))
    (my-search-make-read-only start end)))


(defun my-search-make-where-read-only ()
  (let* ((start-end (my-search-get-where))
         (start (car start-end))
         (end (cadr start-end)))
    (my-search-make-read-only start end)))


(defun my-search-remove-read-only-from-what ()
  (let* ((start-end (my-search-get-what))
         (start (car start-end))
         (end (cadr start-end)))
    (my-search-remove-read-only start end)))


(defun my-search-remove-read-only-from-where ()
  (let* ((start-end (my-search-get-where))
         (start (car start-end))
         (end (cadr start-end)))
    (my-search-remove-read-only start end)))


(defun my-search-erase ()
  "Erases *my-search* buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)))


(defun my-search-prepare ()
  "Prepare *my-search* buffer for searching."
  (insert (concat my-search-what-str " \n"
                  my-search-where-str " " default-directory))
  (my-search-make-what-read-only)
  (my-search-make-where-read-only))


(defun my-search-get-what-value ()
  "Get the input: WHAT to search."
  (save-excursion
    (beginning-of-buffer)
    (unless (search-forward my-search-what-str nil t)
      (user-error "Cannot find WHAT label"))
    (string-trim (buffer-substring (match-end 0) (line-end-position)))))


(defun my-search-get-where-value ()
  "Get the input: WHERE to search"
  (save-excursion
    (beginning-of-buffer)
    (unless (search-forward my-search-where-str nil t)
      (user-error "Cannot find WHERE label"))
    (string-trim (buffer-substring (match-end 0) (line-end-position)))))


(defun my-search-set-what-value (what-value)
  "Set the input: WHAT to search"
  (save-excursion
    (beginning-of-buffer)
    (unless (search-forward my-search-what-str nil t)
      (user-error "Cannot find WHAT label"))
    (delete-region (1+ (match-end 0)) (line-end-position))
    (end-of-line)
    (insert what-value)))


(defun my-search-do-search ()
  "Do search!"
  (interactive)
  (let ((what-value (my-search-get-what-value))
        (where-value (my-search-get-where-value)))
    (message "My search: \"%s\" in \"%s\"" what-value where-value)
    (my-grep what-value where-value)))


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
