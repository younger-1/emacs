;;; simple-scratch.el --- Preserve the scratch buffer across Emacs sessions -*- lexical-binding: t -*-

;; Author: Xavier Young <younger321@foxmail.com>
;; URL: https://github.com/younger-1/simple-scratch.el
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "28"))

(defcustom simple-scratch-dir (file-name-as-directory (expand-file-name "simple-scratch" user-emacs-directory))
  ""
  :type 'directory)

(defvar simple-scratch-buffer-name nil)

;;;###autoload
(defun simple-scratch-open ()
  "Open a new scratch buffer"
  (interactive)
  (make-directory simple-scratch-dir t)
  (let* ((default-directory simple-scratch-dir)
         (buffer (call-interactively #'find-file)))
    (setq simple-scratch-buffer-name (buffer-name buffer))))


;;;###autoload
(defun simple-scratch-dwim ()
  "Open a new scratch buffer if `simple-scratch-buffer-name' is not set or is current buffer, else switch to it"
  (interactive)
  (if-let* ((buf (and simple-scratch-buffer-name
                      (get-buffer simple-scratch-buffer-name)))
            ((not (eq buf (current-buffer)))))
      (switch-to-buffer buf)
    (simple-scratch-open)))

(keymap-global-set "C-h j j" #'simple-scratch-dwim)

(provide 'simple-scratch)
