;;; simple-scratch.el --- Preserve the scratch buffer across Emacs sessions -*- lexical-binding: t -*-

;; Author: Xavier Young <younger321@foxmail.com>
;; URL: https://github.com/younger-1/simple-scratch.el
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "28"))

(defcustom simple-scratch-buffer-name "*JoJo*"
  ""
  :type 'string)

(defcustom simple-scratch-file (expand-file-name "simple-scratch" user-emacs-directory)
  ""
  :type 'file)

(defun simple-scratch-load ()
  "Load the scratch buffer"
  (interactive)
  (with-current-buffer (get-buffer-create simple-scratch-buffer-name)
    (when (file-exists-p simple-scratch-file)
      (insert-file-contents simple-scratch-file))))

(defun simple-scratch-save ()
  "Save the scratch buffer"
  (interactive)
  (with-current-buffer (get-buffer-create simple-scratch-buffer-name)
    ;; not use `write-file' as it will change buffer name
    (write-region (point-min) (point-max) simple-scratch-file)))

(defvar-keymap simple-scratch-mode-map
  "<remap> <revert-buffer>"       #'simple-scratch-load
  "<remap> <revert-buffer-quick>" #'simple-scratch-load
  "<remap> <save-buffer>"         #'simple-scratch-save)

;;;###autoload
(define-minor-mode simple-scratch-mode
  "Simple Scratch."
  :group 'simple-scratch
  (if simple-scratch-mode
      (progn
        ;; (run-with-idle-timer 300 t #'simple-scratch-save)
        (add-hook 'kill-emacs-hook #'simple-scratch-save))
      (remove-hook 'kill-emacs-hook #'simple-scratch-save)))

;;;###autoload
(defun simple-scratch-active ()
  "Active the scratch buffer"
  (interactive)
  (simple-scratch-load)
  (switch-to-buffer simple-scratch-buffer-name :no-record)
  (simple-scratch-mode +1))

(provide 'simple-scratch)
