;;; -*- lexical-binding: t -*-

;; (defconst xy/init-dir
;;   (file-name-directory (or load-file-name buffer-file-name))
;;   "The root directory Emacs configuration.")

(defconst xy/init-dir user-emacs-directory)

;; Reducing clutter in ~/.emacs.d by redirecting files to ~/.emacs.d/var/
(setq user-emacs-directory (concat user-emacs-directory "var/"))

(provide 'init-util)
