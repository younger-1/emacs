;; -*- lexical-binding: t -*-

(use-core server
  ;; :if (dispay-graphic-p)
  ;; :after-call doom-first-input-hook doom-first-file-hook focus-out-hook
  :defer 1
  :config
  ;; (setq server-client-instructions nil)
  ;; Local socket file is `server-name' under `server-socket-dir'
  ;; TCP server file is `server-name' under `server-auth-dir'
  (when xy/win-p
    (setq server-use-tcp t)
    ;; 将目录换回为标准路径 ;; emacsclient does not read the init file
    (setq server-auth-dir (expand-file-name "server" xy/init-dir)))
  (unless (or (server-running-p) (daemonp))
    (server-start))
  ;; (add-hook 'server-switch-hook
  ;;    (lambda ()
  ;;      (let ((server-buf (current-buffer)))
  ;;        (bury-buffer)
  ;;        (switch-to-buffer-other-frame server-buf))))
  ;; (add-hook 'server-done-hook
  ;;    (lambda ()
  ;;      (kill-buffer nil)
  ;;      (delete-frame)))
  )

;; Automatically byte-compiles and native-compiles Emacs Lisp libraries
;; Ensure adding the following compile-angel code at the very beginning of init file, before all other packages.
;; (use-package compile-angel
;;   :demand t
;;   :config
;;   ;; (setq compile-angel-verbose t)
;;   ;; (setq compile-angel-debug t)
;;   ;; (setq compile-angel-byte-compile-report-issues t)
;;
;;   ;; (push "/init.el" compile-angel-excluded-files)
;;   (push "/early-init.el" compile-angel-excluded-files)
;;
;;   ;; A local mode that compiles .el files whenever the user saves them.
;;   (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode)
;;   ;; A global mode that compiles .el files before they are loaded.
;;   (compile-angel-on-load-mode +1))

;; Garbage Collector Magic Hack
;; To minimize GC interference with user activity
;; - 1. During normal use a high GC threshold is set.
;; - 2. When idling GC is triggered and a low threshold is set.
(use-package gcmh
  :hook
  (emacs-startup . gcmh-mode)
  (focus-out-hook . gcmh-idle-garbage-collect)
  :config
  ;; 1 GB -> 800 MB
  (setq gcmh-high-cons-threshold (* 800 1024 1024))
  (setq gcmh-idle-delay 'auto))

;; Profiling the startup time of Emacs
(use-package esup
  :bind ("C-c x p" . esup)
  :config
  ;; https://github.com/jschaf/esup/issues/85
  ;; This is a work around of a bug where esup tries to step into the byte-compiled version of `cl-lib’, and fails horribly:
  (setq esup-depth 0))

(use-package benchmark-init
  :bind
  ("C-c x m" . benchmark-init/show-durations-tree)
  ("C-c x M" . benchmark-init/show-durations-tabulated)
  ;; :init ;; only activate when doing benchmark
  ;; (benchmark-init/activate)
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'emacs-startup-hook #'benchmark-init/deactivate))

(use-package bug-hunter
  :bind ("C-c x b" . bug-hunter-init-file))

(use-package restart-emacs
  :bind ("C-c x r" . restart-emacs))

(provide 'init-perf)
