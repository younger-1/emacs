;; -*- lexical-binding: t -*-

;; (defconst xy/init-dir
;;   (file-name-directory (or load-file-name buffer-file-name))
;;   "The root directory Emacs configuration.")

(defconst xy/init-dir user-emacs-directory)

;; Reducing clutter in ~/.emacs.d by redirecting files to ~/.emacs.d/var/
(setq user-emacs-directory (concat user-emacs-directory "var/"))

;; Set `native-comp-eln-load-path'
(startup-redirect-eln-cache (expand-file-name "eln-cache" user-emacs-directory))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load-file custom-file))

;; For finer granularity, use `system-type' or `system-configuration' directly.
(defconst xy/linux-p
  (eq system-type 'gnu/linux) ; 'berkeley-unix 'gnu 'gnu/kfreebsd
  ;; (memq window-system '(x))
  "Are we running on a GNU/Linux system?")
(defconst xy/win-p
  (eq system-type 'windows-nt) ; 'cygwin 'ms-dos
  ;; (memq window-system '(win32 pc))
  "Are we running on a MS-Window system?")
(defconst xy/mac-p
  (eq system-type 'darwin)
  ;; (memq window-system '(mac ns))
  "Are we running on a Mac system?")

(provide 'init-util)
