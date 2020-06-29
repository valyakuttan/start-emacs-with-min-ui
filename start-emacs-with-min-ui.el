;;; start-emacs-with-min-ui.el --- Emacs initialization

;;; Commentary:
;;
;;
;; No frills Emacs initialization.
;;
;;; Code:

;;; Initialize Emacs with minimum distractions.
;;;
;;;###autoload
(defun init-emacs ()
  (progn
    (dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
      (when (fboundp mode) (funcall mode -1)))
    (setq inhibit-startup-screen t)
    (setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t))
          backup-directory-alist
          `((".*" . ,temporary-file-directory)))

    ;; set utf-8 as default encoding
    (prefer-coding-system 'utf-8)
    (setq coding-system-for-read 'utf-8)
    (setq coding-system-for-write 'utf-8)

    ;;; Set font
    ;; (set-frame-font "Inconsolata 14")

    (setq column-number-mode t) ;; enable column-number-mode
    (global-hl-line-mode)	;; highlight current line
    (setq scroll-step 1)    ;; keyboard scroll one line at a time

    (global-set-key (kbd "RET") 'newline-and-indent)
    ;; activate whitespace-mode to view all whitespace characters
    (global-set-key (kbd "C-c w") 'whitespace-mode)
    ;; show unncessary whitespace that can mess up your diff
    (add-hook 'prog-mode-hook (lambda () (interactive)
                                (setq show-trailing-whitespace 1)))
    ;; use space to indent by default
    (setq-default indent-tabs-mode nil)
    ;; set appearance of a tab that is represented by 4 spaces
    (setq-default tab-width 4)))


;; Rename both file and current buffer.
;;;
;;;###autoload
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

(provide 'start-emacs-with-min-ui)
;;; start-emacs-with-min-ui.el ends here
