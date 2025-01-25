;;; -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8; fill-column: 80 -*-

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load-file custom-file))

(setq user-full-name    "Xavier Young"
      user-mail-address "younger321@foxmail.com")

(set-default-coding-systems 'utf-8)

(if (eq system-type 'windows-nt)
    (set-face-attribute 'default nil :height 120)
  (set-face-attribute 'default nil :height 140))

(set-face-attribute 'default nil :family "Hack Nerd Font")
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
(keymap-global-set "C-x ," (defun xy/open-init-file ()
                             "打开emacs配置文件"
                             (interactive)
                             (find-file user-init-file)))
(keymap-global-set "<backtab>" #'back-to-indentation)
(keymap-global-set "C-x C-b" #'ibuffer)
(keymap-global-set "C-M-<backspace>" #'backward-kill-sexp)
(keymap-global-set "C-x k" #'kill-current-buffer)
(keymap-global-set "C-x O" #'switch-to-minibuffer)
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
(keymap-global-set "C-;" "C-x C-;")

(setq package-archives '(("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")))
(setq package-archive-priorities '(("gnu"    . 90)
                                   ("nongnu" . 80)
                                   ("melpa"  . 10)))
;; Enable `package-quickstart-refresh'
(setq package-quickstart t)

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

  (setq mouse-yank-at-point t)

  ;; TODO `completion-preview-mode' in Emacs 30.
  (setq completions-detailed t)
  (setq completion-styles '(basic flex))
  (setq completion-category-overrides
        '((file (styles . (partial-completion)))))
  (setq completion-auto-select t
        completion-auto-help 'visible ;; Display *Completions* upon first request
        ;; completions-sort 'historical
        completions-format 'one-column)
  (setq completion-ignore-case t
        read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t)
  (setq read-extended-command-predicate #'command-completion-default-include-p)

  (setq suggest-key-bindings 999)
  (setq eval-expression-print-length 30)
  (setq message-log-max 2000)

  (setq make-backup-files nil
        auto-save-default nil
        create-lockfiles nil)

  (setq frame-title-format
        '((:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   "%b"))))

  ;; `simple.el'
  (setq kill-do-not-save-duplicates t)
  (setq what-cursor-show-names t) ;; For `C-x ='
  (setq set-mark-command-repeat-pop t)
  ;; `files.el'
  (setq confirm-kill-emacs #'yes-or-no-p)
  ;; `vc-hooks'
  (setq vc-follow-symlinks t)
  ;; `C-code'
  (setq read-process-output-max (* 64 1024)) ;; 64k
  (setq use-short-answers t))

(use-package emacs
  :ensure nil
  :hook
  (emacs-startup . global-display-line-numbers-mode)
  ;; (emacs-startup . global-display-fill-column-indicator-mode)
  (emacs-startup . pixel-scroll-precision-mode)
  (emacs-startup . delete-selection-mode)
  (prog-mode . show-paren-local-mode)
  (prog-mode . electric-indent-local-mode)
  (prog-mode . electric-pair-local-mode)
  (emacs-startup . window-divider-mode)
  (emacs-startup . blink-cursor-mode)
  :config
  (setq show-paren-context-when-offscreen 'overlay)
  (setq-default cursor-type 'bar))

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
  :bind ("C-x R" . recentf-open-files)
  :config
  (recentf-mode)
  (setq recentf-max-saved-items 500))

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

(use-package which-key
  :hook (window-setup . which-key-mode))

(use-package lisp-mode
  :ensure nil
  ;; BUG: this macro expand to bad thing
  ;; :hook (before-save . #'check-parens)
  :bind (:map lisp-mode-shared-map
              ("C-h e c" . #'check-parens)))

(use-package macrostep
  :bind (:map lisp-mode-shared-map
              ("C-h e m" . macrostep-expand)))
