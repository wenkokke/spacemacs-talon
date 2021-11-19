;;; -*- lexical-binding: t; -*-

;;; talon-integration.el --- Communicate with Talon Voice

;; Copyright (C) 2021 Wen Kokke

;; Author: Wen Kokke <me@wen.works>
;; Version: 0.1
;; Package-Requires:
;; Keywords: voice control
;; URL:

;;; Commentary:

;;; Code:


;; Added Spacemacs to the title
(setq-default
 dotspacemacs-frame-title-format
 (if dotspacemacs-frame-title-format
     (concat "Spacemacs - " dotspacemacs-frame-title-format)
   "Spacemacs"))

;; Create the customizable variable for Talon integration
(defgroup talon-integration nil
  "Settings for talon-integration."
  :group 'talon-integration)

(defcustom talon-integration-root
  "~/.talon-integration/spacemacs/"
  "The directory to which talon-integration writes the editor state."
  :type  'directory
  :group 'talon-integration
  :set #'(lambda (sym dirname)
           (if (stringp dirname)
               (let ((dir (file-name-as-directory dirname)))
                 (setq-default talon-integration-root dir)
                 (setq-default talon-integration--file-file-name    (concat dir "file-name"))
                 (setq-default talon-integration--file-major-mode   (concat dir "major-mode"))
                 (setq-default talon-integration--file-editor-style (concat dir "editor-style"))
                 (setq-default talon-integration--file-editor-state (concat dir "editor-state"))
                 (make-directory dir t)))))

;; Previous state to check if the state has changed:
(setq-default talon-integration--old-file-name    nil)
(setq-default talon-integration--old-major-mode   nil)
(setq-default talon-integration--old-editor-style nil)
(setq-default talon-integration--old-editor-state nil)

;;
(defmacro talon-integration--write-variable-if-changed (oldsym newval filesym)
  `(and ,newval
        (not (eq (symbol-value ,oldsym) ,newval))
        (set-default ,oldsym ,newval)
        (write-region (format "%s" ,newval) nil (symbol-value ,filesym) nil 'quiet)))

(defun talon-integration--post-command-hook ()
  "For each setting tracked by talon-integration, this function checks if its value has changed, and if so, writes updates the relevant file."
  (when talon-integration-root
    (progn

      ;; Save the file associated with or name of the active buffer
      (talon-integration--write-variable-if-changed
       'talon-integration--old-file-name
       (or buffer-file-name (buffer-name))
       'talon-integration--file-file-name)

      ;; Save the current major mode
      (talon-integration--write-variable-if-changed
       'talon-integration--old-major-mode
       major-mode
       'talon-integration--file-major-mode)

      ;; Save the current editor style (emacs, vim, or hybrid)
      (talon-integration--write-variable-if-changed
       'talon-integration--old-editor-style
       dotspacemacs-editing-style
       'talon-integration--file-editor-style)

      ;; If evil-mode is enabled, save the current editor state
      (when (featurep 'evil)
        (talon-integration--write-variable-if-changed
         'talon-integration--old-editor-state
         evil-state
         'talon-integration--file-editor-state))
    )))
(add-hook 'post-command-hook #'talon-integration--post-command-hook)

(provide 'talon-integration)

;;; talon-integration.el ends here
