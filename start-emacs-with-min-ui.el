;;; start-emacs-with-min-ui.el --- Emacs initialization

;;; Commentary:
;;
;;
;; No frills Emacs initialization.
;;
;;; Code:

(require 'cl)
(require 'ido)

(defun initialize-ido ()
  (progn
    (ido-mode t)
    (ido-everywhere t)
    (setq ido-save-directory-list-file
          (locate-user-emacs-file ".ido.last"))
    (setq ido-enable-flex-matching t)
    (setq ido-use-filename-at-point 'guess)
    (setq ido-default-buffer-method 'other-window)
    (setq ido-auto-merge-work-directories-length 0)
    (setq ido-use-virtual-buffers t)
    ;; disable ido faces to see flx highlights.
    (setq ido-use-faces nil)))

(defun start-emacs-with-min-ui ()
  (progn
    (dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
      (when (fboundp mode) (funcall mode -1)))
    (setq inhibit-startup-screen t)
    ;;
    ;; no junk
    ;;
    (setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t))
          backup-directory-alist `((".*" . ,temporary-file-directory)))
    ;; set utf-8 as default encoding
    (prefer-coding-system 'utf-8)
    (setq coding-system-for-read 'utf-8)
    (setq coding-system-for-write 'utf-8)

    ;;
    ;; some useful initialization
    ;;
    (setq column-number-mode t)      ;; enable column-number-mode
    (global-hl-line-mode)	         ;; highlight current line
    (setq scroll-step 1)             ;; keyboard scroll one line at a time
    (setq-default indent-tabs-mode nil)
    ;; redefine save-buffers-kill-emacs function
    (defadvice save-buffers-kill-emacs
        (around no-query-kill-emacs activate)
      (flet ((process-list ())) ad-do-it))))

;;
;; Some helper functions
;;
;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun initialize-emacs-with-min-ui ()
  (progn
    (start-emacs-with-min-ui)
    (initialize-ido)))

(provide 'start-emacs-with-min-ui)
;;; start-emacs-with-min-ui.el ends here