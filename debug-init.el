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

;; (require 'init-util)
;; (require 'init-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs compile阶段，和当前运行环境完全隔离，互不影响（除了load-path）
;; 相当于新开启了一个子进程，默认加载的feature记录在load-history
;; 因此compile阶段看到的变量，除了来自 dump/C层/预加载，大部分来自loaddefs
(eval-and-compile
  (defmacro xy/message (form)
    "如果form是单一变量/表达式，直接求值；如果抛出 void-variable 未定义错误，就接住它并返回 :unbound

原生的异常捕获 (condition-case) 支持词法闭包，比如: (let ((x 10)) (xy/message x))
而boundp 和 symbol-value 只能看到全局动态变量，看不到 let 绑定的局部词法变量"
    `(message "[xy] %s -> %S" ',form
              (condition-case nil
                  ,form
                (void-variable :unbound))))

  (xy/message after-init-time)
  (xy/message (current-time))
  (xy/message (emacs-init-time))
  (xy/message (emacs-uptime))

  ;; (xy/message load-path) ; special, reflect as runtime-phase value
  ;; (xy/message exec-path)
  ;; (xy/message features)
  ;; (xy/message load-history)

  (xy/message path-separator)
  (xy/message message-log-max)
  (xy/message (symbol-file 'message-log-max 'defvar))
  (xy/message (find-lisp-object-file-name 'message-log-max 'defvar))

  ;; (setq user-emacs-directory (concat user-emacs-directory "var/"))
  (xy/message user-emacs-directory)
  (xy/message (symbol-file 'user-emacs-directory 'defvar))
  (xy/message (find-lisp-object-file-name 'user-emacs-directory 'defvar))

  ;; (require 'package)
  (xy/message package-enable-at-startup)
  (xy/message (symbol-file 'package-enable-at-startup 'defvar))
  (xy/message (find-lisp-object-file-name 'package-enable-at-startup 'defvar))

  (xy/message package-user-dir)
  (xy/message package-quickstart-file)

  (xy/message package-quickstart)
  (xy/message package-archives)

  (xy/message package-activated-list)
  (xy/message (package-installed-p 'embark))
  (xy/message (package-installed-p 'corfu))
  (xy/message (package-installed-p 'projtree))
  (xy/message (package-installed-p 'embark-sidebar))

  (xy/message after-init-hook)
  (xy/message inhibit-default-init)
  (xy/message initial-major-mode)

  (xy/message create-lockfiles)
  (xy/message tab-always-indent)
  (xy/message what-cursor-show-names)
  (xy/message xy/mac-p)

  (xy/message (autoloadp (symbol-function #'dired)))
  (xy/message (autoloadp (symbol-function #'use-package)))

  (xy/message use-package-always-ensure)
  (xy/message use-package-always-defer)
  (xy/message use-package-enable-imenu-support)
  (xy/message use-package-expand-minimally)
  (xy/message dired-mark-region)

  (xy/message magit-define-global-key-bindings)
  (xy/message difftastic-bindings-alist)
  (xy/message flymake-collection-hook-config)
  (xy/message expand-region-preferred-python-mode)
  (xy/message rg-keymap-prefix)

  )
