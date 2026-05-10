;;; -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8 -*-

(defconst xy/init-dir user-emacs-directory)
(setq user-emacs-directory (concat user-emacs-directory "var/"))

(add-to-list 'load-path (expand-file-name "lisp" xy/init-dir))
(add-to-list 'load-path (expand-file-name "site-lisp" xy/init-dir))

(setq custom-file (expand-file-name "custom.el" xy/init-dir))
(when (file-exists-p custom-file)
  (load-file custom-file))

(add-hook 'emacs-startup-hook
          (defun xy/-print-init-time ()
            (message "** [xy] Emacs ready in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f") gcs-done)))

(select-frame-set-input-focus (selected-frame))

(toggle-frame-maximized)

(set-face-attribute 'default nil
                    :family nil
                    :height 160)

(keymap-global-set "C-," (defun xy/open-init-dir ()
                           (interactive)
                           (dired xy/init-dir)))

(startup-redirect-eln-cache (expand-file-name "eln-cache" user-emacs-directory))

(setq package-quickstart-file (concat user-emacs-directory "package-quickstart.el"))
(setq package-user-dir (concat user-emacs-directory "elpa"))
(package-activate-all)

(setq use-package-always-ensure nil)
(setq use-package-always-defer t)
(setq use-package-enable-imenu-support t)
(setq use-package-expand-minimally t)
(setq use-package-compute-statistics init-file-debug)

;; theme
(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs t)
(load-theme 'modus-operandi)

(setq initial-major-mode #'fundamental-mode)

(fido-vertical-mode)
;; (icomplete-vertical-mode)
(setq completions-detailed t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-and-compile
  (defsubst xy/symbol-value (sym)
    (cond
     ((not (symbolp sym)) sym)
     ((boundp sym) (symbol-value sym))
     ((fboundp sym) (funcall sym))
     (t :unbound)))

  (defsubst xy/message (sym)
    (message "[xy] %s -> %s" sym (xy/symbol-value sym)))

  (xy/message 'after-init-time)
  (xy/message #'current-time)
  (xy/message #'emacs-init-time)
  (xy/message #'emacs-uptime)

  ;; (xy/message 'load-path)
  ;; (xy/message 'exec-path)
  ;; (xy/message 'features)
  ;; (xy/message 'load-history)
  (xy/message 'path-separator)
  (xy/message 'message-log-max)

  ;; (setq user-emacs-directory (concat user-emacs-directory "var/"))
  (xy/message 'user-emacs-directory)

  ;; (require 'package)
  (xy/message 'package-enable-at-startup)
  (xy/message 'package-user-dir)
  (xy/message 'package-quickstart-file)

  (xy/message 'package-quickstart)
  (xy/message 'package-archives)

  (xy/message 'after-init-hook)
  (xy/message 'inhibit-default-init)
  (xy/message 'initial-major-mode)

  (xy/message 'create-lockfiles)
  (xy/message 'tab-always-indent)
  (xy/message 'what-cursor-show-names)
  (xy/message 'xy/mac-p)

  (xy/message 'use-package-always-ensure)
  (xy/message 'use-package-always-defer)
  (xy/message 'use-package-enable-imenu-support)
  (xy/message 'use-package-expand-minimally)
  (xy/message 'dired-mark-region)

  (xy/message 'magit-define-global-key-bindings)
  (xy/message 'difftastic-bindings-alist)
  (xy/message 'flymake-collection-hook-config)
  (xy/message 'expand-region-preferred-python-mode)
  (xy/message 'rg-keymap-prefix)

  (xy/message
   (symbol-file 'user-emacs-directory 'defvar))
  (xy/message
   (symbol-file 'package-enable-at-startup 'defvar))
  (xy/message
   (find-lisp-object-file-name 'user-emacs-directory 'defvar))
  (xy/message
   (find-lisp-object-file-name 'package-enable-at-startup 'defvar)))
