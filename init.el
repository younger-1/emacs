;;; -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8; fill-column: 80 -*-

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load-file custom-file))

(setq user-full-name    "Xavier Young"
      user-mail-address "younger321@foxmail.com")

(set-default-coding-systems 'utf-8)

;; For finer granularity, use `system-type' or `system-configuration' directly.
(defconst xy/win-p
  (eq system-type 'windows-nt) ;; 'cygwin 'ms-dos
  "Are we running on a MS-Window system?")
(defconst xy/linux-p
  (eq system-type 'gnu/linux) ;; 'berkeley-unix 'gnu 'gnu/kfreebsd
  "Are we running on a GNU/Linux system?")
(defconst xy/mac-p
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst xy/font-size (if xy/win-p 120 140))
(defconst xy/font-name "Hack Nerd Font")
(set-face-attribute 'default nil :height xy/font-size :family xy/font-name)

(defun xy/set-default-face-advanced ()
  "It's useful for setting faces that may get overwritten by switch themes."
  ;; Set the default monospaced font
  (set-face-attribute 'default nil
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :family xy/font-name
                      :height xy/font-size)
  ;; Set an alternative monospaced font. Can be the same as above.
  ;; It should have the same character width as the default font
  (set-face-attribute 'fixed-pitch nil
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 1.0)
  ;; Set an alternative monospaced font, preferably with serifs (optional)
  ;; It should have the same character width as the other two fonts above
  (set-face-attribute 'fixed-pitch-serif nil
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 1.0)
  ;; Set the proportional font (toggle by "M-x variable-pitch-mode")
  (set-face-attribute 'variable-pitch nil
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 1.0)
  ;; Set the fonts for the active mode line
  (set-face-attribute 'mode-line nil
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 0.9)
  ;; Set the fonts for the inactive mode line
  (set-face-attribute 'mode-line-inactive nil
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 0.8))

(add-hook 'xy/after-enable-theme-hook #'xy/set-default-face-advanced)

(when (fboundp 'set-fontset-font)
  ;; LXGW WenKai Mono
  (set-fontset-font t 'han "霞鹜文楷等宽" nil 'append)
  ;; Sarasa Mono SC
  (set-fontset-font t 'han "等距更纱黑体 SC" nil 'append)
  ;; Microsoft YaHei
  (set-fontset-font t 'han "微软雅黑" nil 'append)
  ;; Heiti SC
  (set-fontset-font t 'han "黑体-简" nil 'append))

;; (load-theme 'deeper-blue)
;; (load-theme 'wombat)
(progn
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui nil
        modus-themes-prompts '(background)
        modus-themes-headings '((1 . (1.4))
                                (2 . (1.2))
                                (3 . (1.1))))
  ;; (load-theme 'modus-vivendi)
  (load-theme 'modus-operandi))

(defun xy/load-theme (theme &optional no-confirm no-enable)
  "Load a single theme interactively. Without prefix argument, disable all other enabled themes."
  (interactive (eval (cadr (interactive-form 'load-theme))))
  (if (called-interactively-p)
      (message "[xy]: load theme: %s" theme))
  (unless current-prefix-arg
    (mapc #'disable-theme custom-enabled-themes))
  (funcall-interactively 'load-theme theme :no-confirm no-enable))

(global-set-key (kbd "C-h C-t") #'xy/load-theme)
(global-set-key (kbd "C-h M-t") #'disable-theme)

(defvar xy/after-enable-theme-hook nil
  "Normal hook run after enabling a theme.")
(defun xy/after-enable-theme (&rest _args)
  "Run `xy/after-enable-theme-hook'."
  (run-hooks 'xy/after-enable-theme-hook))
(advice-add 'enable-theme :after #'xy/after-enable-theme)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

(defconst xy/elpa-lisp-d package-user-dir)
(defconst xy/emacs-lisp-d (file-name-directory (directory-file-name doc-directory)))

(dir-locals-set-class-variables
 'read-only
 '((nil . ((eval . (view-mode-enter nil #'kill-buffer))
           (tab-width . 8)))))
(dolist (dir (list xy/elpa-lisp-d xy/emacs-lisp-d))
  (dir-locals-set-directory-class (file-truename dir) 'read-only))

;; http://xahlee.info/emacs/emacs/emacs_keybinding_functions.html
(keymap-global-set "C-;" "C-x C-;")
(keymap-global-set "C-x ," (defun xy/open-init-file ()
                             "打开emacs配置文件"
                             (interactive)
                             (find-file user-init-file)))
(keymap-global-set "C-x ." (defun xy/open-init-dir ()
                             "打开emacs配置目录"
                             (interactive)
                             (dired user-emacs-directory)))
;; Or you should practice more by using `C-]' for `abort-recursive-edit'
;; @see (info "(emacs) Quitting")
(keymap-global-set "C-g" (defun xy/keyboard-quit-dwim ()
                           "Do-What-I-Mean behaviour for a general `keyboard-quit'."
                           (interactive)
                           (cond
                            ((region-active-p)
                             (keyboard-quit))
                            ((derived-mode-p 'completion-list-mode)
                             (delete-completion-window))
                            ((> (minibuffer-depth) 0)
                             (abort-recursive-edit))
                            (t
                             (keyboard-quit)))))
(keymap-global-set "<f12>" #'modus-themes-toggle)

;; Make "C-z" available as a prefix key in the same manner as "C-x" and "C-c".
;; To avoid clashes, new keybindings introduced by Emacs Onboard will usually
;; begin with the prefix "C-z" (with only a few exceptions).
(global-unset-key (kbd "C-z"))

(define-prefix-command 'ctl-z-map nil "Additional prefix key C-z")
(global-set-key (kbd "C-z") 'ctl-z-map)

(define-prefix-command 'ctl-z-c-map nil "Commonly used commands")
(define-key ctl-z-map (kbd "c") 'ctl-z-c-map)

(define-prefix-command 'ctl-z-e-map nil "Emacs built-ins")
(define-key ctl-z-map (kbd "e") 'ctl-z-e-map)

(define-prefix-command 'ctl-z-o-map nil "Org-mode")
(define-key ctl-z-map (kbd "o") 'ctl-z-o-map)

(define-prefix-command 'ctl-z-s-map nil "Scratch buffers")
(define-key ctl-z-map (kbd "s") 'ctl-z-s-map)

(define-prefix-command 'ctl-z-w-map nil "Web-related")
(define-key ctl-z-map (kbd "w") 'ctl-z-w-map)

(define-prefix-command 'ctl-z-x-map nil "Global REPL bindings")
(define-key ctl-z-map (kbd "x") 'ctl-z-x-map)

(setq package-archives '(("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")))
(setq package-archive-priorities '(("gnu"    . 90)
                                   ("nongnu" . 80)
                                   ("melpa"  . 10)))
;; Enable `package-quickstart-refresh'
(setq package-quickstart t)
(setq package-install-upgrade-built-in t)
;; (setq package-native-compile t)

(package-activate-all)
(unless (file-exists-p package-user-dir)
  (package-refresh-contents))

(setq use-package-always-ensure t)
(setq use-package-expand-minimally t)
(setq use-package-enable-imenu-support t)

(use-package emacs
  :ensure nil
  :config
  (setq tab-always-indent 'complete)
  (setq-default indent-tabs-mode nil)
  (setq backward-delete-char-untabify-method 'hungry)

  ;; (global-visuxal-line-mode)
  ;; (setq word-wrap t)
  (setq word-wrap-by-category t)

  ;; TODO `completion-preview-mode' in Emacs 30.
  (setq completions-detailed t)
  (setq completion-styles '(basic flex)) ;; @see `completion-styles-alist' for available style
  (setq completion-category-overrides ;; @see `completion-category-defaults' for available category
        '((file (styles partial-completion))))
  (setq completion-auto-select t
        completion-auto-help 'visible ;; Display *Completions* upon first request
        completion-no-auto-exit t
        ;; completions-sort 'historical
        completions-format 'one-column)
  (setq completion-ignore-case t
        read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t)
  (setq read-extended-command-predicate #'command-completion-default-include-p)
  (setq history-delete-duplicates t)

  (setq suggest-key-bindings 999)
  (setq eval-expression-print-length 30)
  (setq message-log-max 2000)

  (setq make-backup-files nil
        auto-save-default nil
        create-lockfiles nil)

  (setq dired-listing-switches "-alh")

  ;; `simple.el'
  (setq kill-do-not-save-duplicates t)
  (setq save-interprogram-paste-before-kill t)
  (setq what-cursor-show-names t) ;; For `C-x ='
  (setq set-mark-command-repeat-pop t)
  ;; `files.el'
  (setq confirm-kill-emacs #'yes-or-no-p)
  (setq require-final-newline t)
  (setq backup-by-copying t)
  (setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
  ;; `paren.el'
  (setq show-paren-context-when-offscreen 'overlay)
  ;; `mouse.el'
  (setq mouse-yank-at-point t)
  ;; `paragraphs.el'
  (setq sentence-end-double-space nil) ;; Don't assume that sentences should have two spaces after periods. This ain't a typewriter
  ;; `vc-hooks.el'
  (setq vc-follow-symlinks t)
  ;; `C-code'
  (setq use-short-answers t)
  ;; (fset 'yes-or-no-p 'y-or-n-p)
  (setq-default cursor-type 'bar)
  (setq large-file-warning-threshold (* 64 1024 1024)) ;; 10m -> 64m
  (setq read-process-output-max (* 1024 1024)) ;; 4k -> 1m
  (setq undo-outer-limit (* 120 1024 1024)) ;; 24m -> 120m
  ;; (setq visible-bell t)
  (setq frame-title-format
        '((:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   "%b")))))

(use-package emacs
  :ensure nil
  :hook
  (prog-mode . show-paren-local-mode)
  (prog-mode . electric-indent-local-mode)
  (prog-mode . electric-pair-local-mode)
  (prog-mode . subword-mode)
  (emacs-startup . global-display-line-numbers-mode)
  (emacs-startup . pixel-scroll-precision-mode)
  (emacs-startup . delete-selection-mode)
  (emacs-startup . window-divider-mode)
  (emacs-startup . blink-cursor-mode)
  ;; (emacs-startup . tab-bar-mode)
  ;; (emacs-startup . tab-bar-history-mode)
  ;; (emacs-startup . global-tab-line-mode)
  ;; (emacs-startup . global-display-fill-column-indicator-mode)
  (before-save . delete-trailing-whitespace)
  ;; (after-save . executable-make-buffer-file-executable-if-script-p) ;; Only work if buffer begin with "#!"
  :bind
  ("<backtab>" . #'back-to-indentation)
  ("C-x C-b" . #'ibuffer)
  ("C-x k" . #'kill-current-buffer)
  ("C-x O" . #'switch-to-minibuffer)
  ("M-/" . #'hippie-expand))

(use-package lisp-mode
  :ensure nil
  :init
  (defun xy/check-parens-before-save ()
    (add-hook 'before-save-hook #'check-parens 0 :local))
  (add-hook 'lisp-mode-hook #'xy/check-parens-before-save)
  (add-hook 'emacs-lisp-mode-hook #'xy/check-parens-before-save)
  :bind (:map lisp-mode-shared-map
              ("C-M-<backspace>" . #'backward-kill-sexp)
              ("C-h e c" . #'check-parens)))

(use-package icomplete
  :ensure nil
  :defer 1
  :config
  (fido-vertical-mode))

(use-package help
  :ensure nil
  :init
  (setq help-window-select t)
  (setq describe-bindings-outline t)

  (defun xy/find-feature ()
    "Find loaded features"
    (interactive)
    (require 'loadhist)
    (find-library
     (let* ((coll (mapcar #'symbol-name features))
            (completion-extra-properties
             '(:annotation-function
               (lambda (k) ;; only accept string
                 (let ((desc (feature-file (intern k))))
                   (if desc
                       (format "\n\t\t%s" desc)
                     ""))))))
       (completing-read "Features: " coll))))

  :bind (("C-h R" . #'info-display-manual)
         ("C-h S" . #'info-lookup-symbol)
         ("C-h F" . #'Info-goto-emacs-command-node)
         ("C-h K" . #'Info-goto-emacs-key-command-node)
         ;;
         ("C-h C-f" . #'find-function)
         ("C-h C-v" . #'find-variable)
         ("C-h C-k" . #'find-function-on-key)
         ("C-h C-l" . #'find-library)
         ("C-h C-b" . #'describe-keymap)
         ("C-h C-p" . #'finder-by-keyword)
         ;;
         ("C-h f" . #'describe-function)
         ("C-h c" . #'describe-command)
         ("C-h v" . #'describe-variable)
         ("C-h k" . #'describe-key)
         ("C-h b" . #'describe-bindings)
         ("C-h s" . #'describe-symbol)
         ("C-h m" . #'describe-mode)
         ("C-h n" . #'describe-minor-mode)
         ;;
         ("C-h l" . #'xy/find-feature)
         ("C-h L" . #'unload-feature)
         ;;
         ("C-h j i" . #'describe-icon)
         ("C-h j c" . #'describe-char)
         ("C-h j f" . #'describe-face)
         ("C-h j g" . #'describe-font)
         ("C-h j h" . #'describe-fontset)
         ("C-h j t" . #'describe-theme)
         ("C-h j s" . #'describe-syntax)
         ("C-h j w" . #'describe-widget)
         ("C-h j I" . #'describe-input-method)
         ("C-h j l" . #'describe-language-environment)
         ("C-h j p" . #'describe-text-properties)
         ("C-h j C" . #'describe-current-coding-system)
         ("C-h j b" . #'button-describe)
         ("C-h j W" . #'widget-describe)
         ("C-h j y" . #'cl-describe-type)
         ;;
         ("C-h a" . nil) ; `apropos-command'
         ("C-h a a" . #'apropos)
         ("C-h a c" . #'apropos-command)
         ("C-h a d" . #'apropos-documentation)
         ("C-h a w" . #'apropos-value)
         ("C-h a W" . #'apropos-local-value)
         ("C-h a u" . #'apropos-user-option)
         ("C-h a l" . #'apropos-library)
         ("C-h a f" . #'apropos-function)
         ("C-h a v" . #'apropos-variable)
         ("C-h a V" . #'apropos-local-variable)
         ;;
         ("C-h h" . nil) ; `view-hello-file'
         ("C-h h a" . #'display-about-screen)
         ("C-h h q" . #'view-emacs-FAQ)
         ("C-h h p" . #'view-emacs-problems)
         ("C-h h n" . #'view-emacs-news)
         ("C-h h t" . #'view-emacs-todo)
         ("C-h h d" . #'view-emacs-debugging)
         ("C-h h e" . #'view-echo-area-messages)
         ("C-h h l" . #'view-lossage)
         ;;
         ("C-h e" . nil) ; `view-echo-area-messages'
         ("C-h e j" . #'pp-eval-last-sexp)
         ("C-h e p" . #'pp-eval-expression)
         ("C-h e f" . #'eval-defun)
         ("C-h e b" . #'eval-buffer)
         ("C-h e r" . #'eval-region)
         ("C-h e d" . #'edebug-defun)
         ;;
         ("C-h w" . nil) ; `where-is'
         ("C-h w c" . #'where-is)
         ("C-h w k" . #'describe-key-briefly)
         ;;
         ("C-h t" . #'help-with-tutorial)
         :map help-mode-map
         ("b" . #'beginning-of-buffer)
         ("e" . #'end-of-buffer)
         ("j" . #'next-line)
         ("k" . #'previous-line)))

(use-package info
  :ensure nil
  :init
  (defun xy/info-elisp () (interactive) (info "elisp"))
  (defun xy/info-eintr () (interactive) (info "eintr"))
  :bind (("C-h i" . nil) ; `info'
         ("C-h i i" . #'info)
         ("C-h i r" . #'info-emacs-manual)
         ("C-h i e" . #'xy/info-elisp)
         ("C-h i t" . #'xy/info-eintr)
         :map Info-mode-map
         ("M-n" . nil) ; `clone-buffer`
         ("j" . #'next-line)
         ("k" . #'previous-line)
         ("." . #'Info-search-next)
         ("a" . #'info-apropos)))

(use-package saveplace
  :ensure nil
  :defer 1
  :config
  (save-place-mode))

(use-package savehist
  :ensure nil
  :defer 1
  :config
  (savehist-mode)
  (setq savehist-additional-variables '(kill-ring      ; persist clipboard
                                        register-alist ; persist keyboard macro
                                        mark-ring global-mark-ring ; persist mark
                                        (search-ring . 50) (regexp-search-ring . 50) ; persist search
                                        comint-input-ring)))

(use-package recentf
  :ensure nil
  :defer 1
  :bind
  ("C-x f" . recentf-open)
  ("C-x R" . recentf-open-files)
  :config
  (recentf-mode)
  (setq recentf-max-saved-items 500
        recentf-max-menu-items 25))

(use-package autorevert
  :ensure nil
  :defer 1
  :config
  (global-auto-revert-mode)
  (setq auto-revert-remote-files t)
  (setq global-auto-revert-non-file-buffers t))

(use-package package
  :ensure nil
  :init
  (defun xy/open-package-quickstart ()
    (interactive)
    (find-file package-quickstart-file))
  (defun xy/open-elpa-d ()
    (interactive)
    ;; (find-file package-user-dir)
    (let ((default-directory (file-name-as-directory package-user-dir)))
      (call-interactively 'find-file)))
  :bind (("C-h p" . nil) ; `finder-by-keyword'
         ("C-h p p" . #'describe-package)
         ("C-h p R" . package-refresh-contents)
         ("C-h p q" . package-quickstart-refresh)
         ("C-h p l" . package-list-packages)
         ("C-h p r" . package-reinstall)
         ("C-h p d" . package-delete)
         ("C-h p D" . package-autoremove)
         ;;
         ("C-h p i" . package-install)
         ("C-h p I" . package-install-selected-packages)
         ("C-h p u" . package-upgrade)
         ("C-h p U" . package-upgrade-all)
         ;;
         ("C-h p v i" . package-vc-install)
         ("C-h p v I" . package-vc-install-selected-packages)
         ("C-h p v u" . package-vc-upgrade)
         ("C-h p v U" . package-vc-upgrade-all)
         ("C-h p v r" . package-vc-rebuild)
         ;;
         ("C-h p q" . #'xy/open-package-quickstart)
         ("C-h p a" . #'xy/open-elpa-d)
         ("C-h p j" . #'use-package-jump-to-package-form)
         ("C-h p k" . #'use-package-report)))

(use-package repeat
  :ensure nil
  :hook (emacs-startup . repeat-mode))

(use-package hl-line
  :ensure nil
  :hook
  (prog-mode . hl-line-mode)
  (text-mode . hl-line-mode)
  (package-menu-mode . hl-line-mode))

(use-package ediff
  :ensure nil
  :defer t
  :config
  (setq ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-split-window-function #'split-window-horizontally
        ediff-merge-split-window-function #'split-window-horizontally))

(use-package which-key
  :hook (window-setup . which-key-mode))

(use-package macrostep
  :bind (:map lisp-mode-shared-map
              ("C-h e m" . macrostep-expand)))
