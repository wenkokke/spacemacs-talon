;;; -*- lexical-binding: t -*-

;;; emacs-monitor.el --- Write Emacs state to the file system

;; Copyright (C) 2021 Wen Kokke

;; Author: Wen Kokke <me@wen.works>
;; Version: 0.1
;; Package-Requires:
;; Keywords: voice control
;; URL:

;;; Commentary:

;;; Code:

;; TODO: extend this to support monitors which run functions

(defun emacs-monitor (syms)
  "Create a 'post-command-hook to monitor each symbol.\
   If the symbol has changed, write it to a file with the same name.\
   If the symbol is nil, delete the file."

  ;; Notify the user that 'emacs-monitor' is starting:
  (message "Starting emacs-monitor\n  with directory %s\n  with monitors %s"
           emacs-monitor-directory
           syms)
  ;; Support passing a single symbol:
  (if (symbolp syms) (setq syms '(syms)))

  ;; Run 'emacs-monitor--monitor-create' for each symbol:
  (if (and (sequencep syms) (seq-every-p 'symbolp syms))
      (seq-mapcat 'emacs-monitor--monitor-create syms)
    (error "Error emacs-monitor: expected one or more symbols, found %S" syms)))

(defun emacs-monitor--monitor-write (file value)
  "Writes a value to the file associated with the monitor.\
   If value is non-nil, write it to the file.\
   Otherwise, delete the file."
  (if value
      (write-region (format "%s\n" value) nil file nil 'quiet)
    (when (file-exists-p file) (delete-file file))))

(defun emacs-monitor--monitor-create (sym)
  "Create a 'post-command-hook to monitor the symbol.\
   (See 'emacs-monitor'.)"
  (let* ((name (symbol-name sym))
         (file (concat emacs-monitor-directory name))
         (prev (gensym)))

    ;; Initialize cache:
    (set prev nil)

    ;; Setup monitor hook:
    (add-hook 'post-command-hook
              (lambda ()
                (let ((next (when (boundp sym) (symbol-value sym))))
                  (unless (equal (symbol-value prev) next)
                    (set prev next)
                    (emacs-monitor--monitor-write file next))
                  )))

    ;; Setup cleanup hook:
    (add-hook 'kill-emacs-hook
              (lambda ()
                (emacs-monitor--monitor-write file nil)))
    ))

(provide 'emacs-monitor)

;;; emacs-monitor.el ends here
