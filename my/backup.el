;; BACKUP
;; -----------------------------------------------------------------------------

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
