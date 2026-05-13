;; -*- lexical-binding: t -*-

;; (message "** [xy] boot init.el")
;; (message "** [xy] load-path: %s" load-path)

;; (set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)
;; (setenv "LC_CTYPE" "UTF-8")
;; (setenv "LC_ALL" "en_US.UTF-8")
;; (setenv "LANG" "en_US.UTF-8")

;; Not need `eval-and-compile', as `load-path' got the same value at compile-phase and runtime-phase
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

;; Prefer `load' to `require', they all can: byte compile, native compile
;; Only use `require' which is more friendly to: autoload, 重复加载控制, 依赖关系维护
(load "init-util") ; common/lib/util/helper: var, macro
(load "init-core") ; settings: theme, font, env, keybinding, options, hooks
(load "init-package") ; package and use-package
(load "init-emacs") ; TODO move to init-core
(load "init-basic") ; perf, help, history, search, minibuffer, completion
(load "init-keymap") ; repeat, transient, casual, evil
(load "init-ui") ; buffer, window, dired, project, appearnace, highlight, todo
(load "init-git") ; vc, diff
(load "init-edit")
(load "init-prog")
(load "init-tool")
