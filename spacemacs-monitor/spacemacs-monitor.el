;;; -*- lexical-binding: t -*-

;;; spacemacs-monitor.el --- Write Spacemacs state to the file system

;; Copyright (C) 2021 Wen Kokke

;; Author: Wen Kokke <me@wen.works>
;; Version: 0.1
;; Package-Requires:
;; Keywords: voice control
;; URL:

;;; Commentary:

;;; Code:

;; (setq debug-on-error t)

;; Create a directory for communication with Talon
(setq-default spacemacs-monitor-directory
              (concat spacemacs-cache-directory "spacemacs-monitor/"))

(make-directory spacemacs-monitor-directory t)

;; Create a monitor for a given value

(defun spacemacs-monitor-create (sym)
  ""
  (let* ((name (symbol-name sym))
         (file (concat spacemacs-monitor-directory name))
         (prev (gensym)))

    ;; Initialize cache:
    (set prev nil)

    ;; Set up hook:
    (add-hook 'post-command-hook
              (lambda ()
                (let ((next (when (boundp sym) (symbol-value sym))))
                  (if next
                      ;; Write file if monitor has changed:
                      (unless (equal (symbol-value prev) next)
                        (set prev next)
                        (write-region (format "%s\n" next) nil file nil 'quiet))
                    ;; Delete file if monitor is nil:
                    (when (file-exists-p file) (delete-file file))
                    ))))
    ))

(provide 'spacemacs-monitor)

;;; spacemacs-monitor.el ends here
