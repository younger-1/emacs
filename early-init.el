;;; -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8 -*-
;;
;; Advantages of Using early-init.el
;; -- @see https://www.reddit.com/r/emacs/comments/1jlnysh/advantages_of_using_earlyinitel_and_initel_over/
;; -- set a dark background if you use a dark theme, to prevent the flash of light
;; -- set the initial frame size to prevent the frame resize "flash" at startup
;; -- disable some GUI elements before frame pops up (buttons-bar, tool-bar, scroll-bar), this makes startup a bit faster
;; -- disable native comp jit for some or all files (helpful to prevent recompile of some loaddefs.el.gz files at Emacs start)
;; -- change the garbage collector threshold very early to speed up Emacs startup even more

(when (< emacs-major-version 30)
  (user-error "[xy] emacs version is %s, require emacs-30." emacs-major-version))

(setq debug-on-error (or init-file-debug '(void-variable)))

(setq garbage-collection-messages init-file-debug)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (defun xy/-reset-gc-cons ()
            "真正的诀窍在于初始化完成后再把它降到合理的水平"
            (setq gc-cons-threshold 800000
                  gc-cons-percentage 0.1)))

(add-hook 'focus-out-hook #'garbage-collect)
(run-with-idle-timer 15 t #'garbage-collect)

(unless (or (daemonp) noninteractive)
  (let ((default-file-name-handler-alist file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (add-hook 'emacs-startup-hook
              (defun xy/-reset-file-name-handler-alist ()
                "Set the file-name-handler to nil since regexing is cpu intensive."
                (setq file-name-handler-alist default-file-name-handler-alist)))))

(add-hook 'emacs-startup-hook
          (defun xy/-print-init-time ()
            (message "** [xy] Emacs ready in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f") gcs-done)))

(setq load-prefer-newer t)
(setq byte-compile-warnings '(not obsolete))
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors (or init-file-debug 'silent))
  (setq native-comp-jit-compilation t))

(setq package-enable-at-startup nil)
