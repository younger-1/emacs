;;; -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8 -*-

(add-hook 'emacs-startup-hook
          (defun xy/-print-init-time ()
            (message "** [xy] Emacs ready in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f") gcs-done)))

;; https://dolzhenko.me/blog/2025/02/emacs-frames-are-not-coming-into-the-foreground-on-mac-os/
;; -- Bringing the Emacs frame up front
(select-frame-set-input-focus (selected-frame))

;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
(toggle-frame-maximized)

(set-face-attribute 'default nil
                    :family nil
                    :height 140)

(keymap-global-set "C-," (defun xy/open-init-dir ()
                           (interactive)
                           (dired user-emacs-directory)))
(keymap-global-set "C-c y f" (defun xy/select-font ()
                               (interactive)
                               (set-face-attribute 'default nil
                                                   :family (completing-read "Default font: " (font-family-list)))))

(defun xy/load-theme (theme &optional no-confirm no-enable)
  "Load a single theme interactively. Without prefix argument, disable all other enabled themes."
  (interactive (eval (cadr (interactive-form 'load-theme))))
  (if (called-interactively-p)
      (message "[xy]: load theme: %s" theme))
  (unless current-prefix-arg
    (mapc #'disable-theme custom-enabled-themes))
  (funcall-interactively 'load-theme theme :no-confirm no-enable))

(keymap-global-set "C-c y l" #'xy/load-theme)
(keymap-global-set "C-c y u" #'disable-theme)

(global-set-key [down-mouse-3] #'context-menu-open)
(global-set-key [M-down-mouse-3] `(menu-item ,(purecopy "Menu Bar") ignore :filter ,(lambda (_) (mouse-menu-bar-map))))
(keymap-global-set "M-s-," #'customize-group)
(keymap-global-set "s-x" #'execute-extended-command)
(keymap-global-set "s-X" #'execute-extended-command-for-buffer)
(keymap-global-set "s-<return>" #'toggle-frame-fullscreen)  ; <f11>
(keymap-global-set "S-s-<return>" #'toggle-frame-maximized) ; M-<f10>

(package-activate-all)

(setq use-package-always-ensure nil)
(setq use-package-enable-imenu-support t)
(setq use-package-expand-minimally t)
(setq use-package-compute-statistics init-file-debug)


;;; basic
(use-package emacs
  :init
  (show-paren-mode)
  (electric-indent-mode)
  (electric-pair-mode)
  (global-subword-mode)
  :hook
  ;; (prog-mode . show-paren-local-mode)
  ;; (prog-mode . electric-indent-local-mode)
  ;; (prog-mode . electric-pair-local-mode)
  ;; (prog-mode . subword-mode)
  ;; (emacs-startup . global-display-line-numbers-mode)
  ;; (emacs-startup . column-number-mode) ; modeline
  ;; (emacs-startup . size-indication-mode) ; modeline
  ;; (emacs-startup . pixel-scroll-precision-mode)
  (emacs-startup . delete-selection-mode)
  (emacs-startup . window-divider-mode)
  :bind
  ("<backtab>" . #'back-to-indentation)
  ("C-;" . #'comment-line)
  ("C-x K" . #'bury-buffer)
  ("C-x O" . #'switch-to-minibuffer)
  ("C-x x v" . #'view-buffer)
  ("C-x x f" . #'follow-mode)
  :config
  ;; theme
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t)
  (load-theme 'modus-operandi)

  ;; bar
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

  ;; frame
  (setq frame-inhibit-implied-resize t)
  (setq frame-resize-pixelwise t)
  (setq frame-title-format
        '((:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   "%b"))))

  ;; tab
  (setq-default indent-tabs-mode nil)
  (setq tab-always-indent 'complete)
  (setq backward-delete-char-untabify-method 'hungry)

  ;; mark
  ;; (setq mark-even-if-inactive nil)

  ;; kill
  (setq kill-do-not-save-duplicates t)
  (setq save-interprogram-paste-before-kill t)

  ;; register
  (setopt register-use-preview 'insist)

  ;; scroll
  ;; (setq scroll-preserve-screen-position t)
  ;; (setq scroll-margin 2
  ;;       scroll-conservatively 3)

  ;; mouse
  (setq mouse-yank-at-point t)
  (setq mouse-autoselect-window t)

  ;; limit
  (setq large-file-warning-threshold (* 64 1024 1024)) ; 10m -> 64m
  (setq read-process-output-max (* 1024 1024)) ; 4k -> 1m
  (setq undo-limit (* 10 160000) ; 10x
        undo-strong-limit (* 10 240000)
        undo-outer-limit (* 10 24000000))
  (setq echo-keystrokes 0.1)
  (setq suggest-key-bindings 999)
  (setq eval-expression-print-length nil
        eval-expression-print-level nil)
  (setq message-log-max 3000)

  ;; lock/backup/auto-save
  (setq create-lockfiles nil)
  (setq make-backup-files nil)
  (setq auto-save-default nil)

  ;; misc
  (setq word-wrap-by-category t)
  (setq truncate-partial-width-windows 80)
  (setq uniquify-buffer-name-style 'forward)

  ;; edit
  (setq comment-empty-lines t)
  (setq-default fill-column 80)
  (setq sentence-end-double-space nil)
  (setq require-final-newline t)

  ;; isearch
  (setq isearch-lazy-count t)
  (setq isearch-lazy-highlight 'all-windows)
  (setq isearch-allow-scroll 'unlimited
        isearch-allow-motion t
        isearch-motion-changes-direction t)
  (setq isearch-yank-on-move 'shift)

  (defun xy/isearch-exit-mark-match ()
    "Exit isearch and mark the current match."
    (interactive)
    (isearch-exit)
    (push-mark isearch-other-end)
    (activate-mark))
  (keymap-set isearch-mode-map "C-<return>" #'xy/isearch-exit-mark-match)

  (defun xy/isearch-project ()
    "Run `project-find-regexp' using the last search string as the regexp"
    (interactive)
    (isearch-exit)
    (let ((query (if isearch-regexp
                     isearch-string
                   (regexp-quote isearch-string))))
      (project-find-regexp query)))
  (keymap-set isearch-mode-map "M-s p" #'xy/isearch-project)

  ;; `simple.el'
  (setq what-cursor-show-names t)
  (setq set-mark-command-repeat-pop t)
  ;; `files.el'
  (setq delete-by-moving-to-trash t)
  (setq confirm-kill-emacs #'yes-or-no-p)
  (setq remote-file-name-inhibit-cache 50)
  (setq remote-file-name-inhibit-delete-by-moving-to-trash t)
  (setq find-file-visit-truename t
        vc-follow-symlinks t)
  ;; `paren.el'
  (setq show-paren-context-when-offscreen 'overlay
        blink-matching-paren-highlight-offscreen t)
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t)
  ;; `compile.el'
  (setq compilation-scroll-output 'first-error)
  ;; `C-code'
  (setq-default show-trailing-whitespace t)
  (setq-default indicate-buffer-boundaries 'left)

  ;; others
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load-file custom-file))

  (setq user-full-name    "Xavier Young"
        user-mail-address "younger321@foxmail.com")

  (put 'narrow-to-region 'disabled nil)
  (put 'narrow-to-page 'disabled nil)
  (put 'scroll-left 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'erase-buffer 'disabled nil)
  (put 'dired-find-alternate-file 'disabled nil)
  (put 'list-timers 'disabled nil)

  (defconst xy/elpa-lisp-d (expand-file-name package-user-dir))
  (defconst xy/emacs-lisp-d (file-name-directory (directory-file-name doc-directory)))

  (dir-locals-set-class-variables
   :read-only
   '((nil . ((eval . (view-mode-enter nil #'kill-buffer))
             (tab-width . 8)))))
  (dolist (dir (list xy/elpa-lisp-d xy/emacs-lisp-d))
    (dir-locals-set-directory-class (file-truename dir) :read-only)))


;;; help
(use-package help
  :init
  ;; (setq help-window-select t)
  ;; (setq help-window-keep-selected t)
  (setopt help-at-pt-display-when-idle t)
  (setq help-clean-buttons t)
  (setq describe-bindings-outline t)
  (setq describe-bindings-show-prefix-commands t)

  (defun xy/loaded-feature ()
    "Find loaded features"
    (interactive)
    (require 'loadhist)
    (find-library
     (let* ((coll (mapcar #'symbol-name features))
            (completion-extra-properties
             '(:annotation-function
               (lambda (k) ; only accept string
                 (let ((desc (feature-file (intern k))))
                   (if desc
                       (format "\n\t\t%s" desc)
                     ""))))))
       (completing-read "Features: " coll))))
  (defun xy/help-show-plist ()
    (interactive)
    (require 'apropos)
    (apropos-describe-plist (symbol-at-point)))

  :bind (;; @see `help-map'
         ("C-h C-h" . #'help-for-help)
         ("C-h ?" . #'help-for-help)
         ("C-h ." . #'display-local-help)
         ;;
         ("C-h C-f" . #'find-function) ; `view-emacs-FAQ'
         ("C-h C-v" . #'find-variable)
         ("C-h C-k" . #'find-function-on-key)
         ("C-h C-l" . #'find-library)
         ("C-h C-b" . #'describe-keymap)
         ("C-h C-p" . #'finder-by-keyword) ; `view-emacs-problems'
         ;;
         ("C-h f" . #'describe-function)
         ("C-h c" . #'describe-command) ; `describe-key-briefly'
         ("C-h v" . #'describe-variable)
         ("C-h k" . #'describe-key)
         ("C-h b" . #'describe-bindings)
         ("C-h B" . #'describe-personal-keybindings)
         ("C-h s" . #'describe-symbol) ; `describe-syntax'
         ("C-h m" . #'describe-mode)
         ("C-h n" . #'describe-minor-mode) ; `view-emacs-news'
         ("C-h x" . #'command-history) ; `describe-command'.  @tip Use x to repeat the command on the current line.
         ;;
         ("C-h i" . #'info)
         ("C-h R" . #'info-display-manual)
         ("C-h S" . #'info-lookup-symbol)
         ("C-h F" . #'Info-goto-emacs-command-node)
         ("C-h K" . #'Info-goto-emacs-key-command-node)
         ;;
         ("C-h a" . nil) ; `apropos-command'
         ("C-h a a" . #'apropos)
         ("C-h a c" . #'apropos-command)
         ("C-h a d" . #'apropos-documentation)
         ("C-h a w" . #'apropos-value)
         ("C-h a W" . #'apropos-local-value)
         ("C-h a o" . #'apropos-user-option)
         ("C-h a l" . #'apropos-library)
         ("C-h a f" . #'apropos-function)
         ("C-h a v" . #'apropos-variable)
         ("C-h a V" . #'apropos-local-variable)
         ;;
         ("C-h d" . nil) ; `apropos-documentation'
         ("C-h d i" . #'describe-icon)
         ("C-h d c" . #'describe-char)
         ("C-h d f" . #'describe-face)
         ("C-h d F" . #'list-faces-display)
         ("C-h d g" . #'describe-font)
         ("C-h d h" . #'describe-fontset)
         ("C-h d t" . #'describe-theme)
         ("C-h d s" . #'describe-syntax)
         ("C-h d w" . #'describe-widget) ; or "C-u C-h ."
         ("C-h d b" . #'button-describe)
         ("C-h d W" . #'widget-describe)
         ("C-h d I" . #'describe-input-method)
         ("C-h d l" . #'describe-language-environment)
         ("C-h d p" . #'describe-text-properties)
         ("C-h d C" . #'describe-coding-system)
         ("C-h d y" . #'cl-describe-type)
         ;;
         ("C-h h" . nil) ; `view-hello-file'
         ("C-h h q" . #'view-emacs-FAQ)
         ("C-h h p" . #'view-emacs-problems)
         ("C-h h n" . #'view-emacs-news)
         ("C-h h t" . #'view-emacs-todo)
         ("C-h h d" . #'view-emacs-debugging)
         ;;
         ("C-h o" . nil) ; `describe-symbol'
         ("C-h o s" . #'shortdoc)
         ("C-h o l" . #'xy/loaded-feature)
         ("C-h o u" . #'unload-feature)
         ;;
         ("C-h u f" . #'add-file-local-variable)
         ("C-h u d" . #'add-dir-local-variable)
         ;;
         ("C-h w" . nil) ; `where-is'
         ("C-h w c" . #'where-is)
         ("C-h w k" . #'describe-key-briefly)
         ;;
         ;; j u y z
         ("C-h e" . #'view-echo-area-messages)
         ("C-h l" . #'view-lossage)
         ("C-h t" . #'help-with-tutorial)
         ("C-h g" . nil) ; `describe-gnu-project'
         ("C-h q" . nil) ; `help-quit'
         ("C-h C" . nil) ; `describe-input-method'
         ("C-h I" . nil) ; `describe-coding-system'
         ("C-h P" . nil) ; `describe-package'
         ;;
         ("C-h C-a" . #'about-emacs)
         ("C-h C-q" . #'help-quick-toggle)
         ("C-h C-s" . #'search-forward-help-for-help)
         ("C-h C-c" . nil) ; `describe-copying'
         ("C-h C-d" . nil) ; `view-emacs-debugging'
         ("C-h C-e" . nil) ; `view-external-packages'
         ("C-h C-m" . nil) ; `view-order-manuals'
         ("C-h C-n" . nil) ; `view-emacs-news'
         ("C-h C-o" . nil) ; `describe-distribution'
         ("C-h C-t" . nil) ; `view-emacs-todo'
         ("C-h C-w" . nil) ; `describe-no-warranty'
         :map help-mode-map
         ("C" . #'set-variable)
         ("P" . #'xy/help-show-plist)
         ("b" . #'beginning-of-buffer)
         ("e" . #'end-of-buffer)))

(use-package info
  :ensure nil
  :init
  (defun xy/info-elisp () (interactive) (info "elisp"))
  (defun xy/info-eintr () (interactive) (info "eintr"))
  :bind (("C-h r" . nil) ; `info-emacs-manual'
         ("C-h r r" . #'info-emacs-manual)
         ("C-h r e" . #'xy/info-elisp)
         ("C-h r t" . #'xy/info-eintr)
         :map Info-mode-map
         ("." . #'Info-search-next)
         ("a" . #'info-apropos)))

(use-package package
  :init
  (defun xy/open-package-quickstart ()
    (interactive)
    (find-file package-quickstart-file))
  (defun xy/open-elpa-d ()
    (interactive)
    (let ((default-directory (file-name-as-directory package-user-dir)))
      (call-interactively 'find-file)))
  :bind (("C-h p" . nil) ; `finder-by-keyword'
         ("C-h p p" . #'describe-package)
         ("C-h p R" . package-refresh-contents)
         ("C-h p q" . package-quickstart-refresh)
         ("C-h p l" . package-list-packages-no-fetch)
         ("C-h p L" . package-list-packages)
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

(use-package cus-edit
  :bind
  ("C-h , ," . #'customize)
  ("C-h , ." . #'customize-group)
  ("C-h , b" . #'customize-browse)
  ("C-h , m" . #'customize-mode)
  ("C-h , o" . #'customize-option)
  ("C-h , t" . #'customize-themes)
  ("C-h , f" . #'customize-face)
  ("C-h , i" . #'customize-icon)
  ("C-h , a a" . #'customize-apropos)
  ("C-h , a o" . #'customize-apropos-options)
  ("C-h , a f" . #'customize-apropos-faces)
  ("C-h , a g" . #'customize-apropos-groups)
  ;;
  ("C-h , c" . #'customize-changed)
  ("C-h , s" . #'customize-saved)
  ("C-h , u" . #'customize-unsaved)
  ("C-h , r" . #'customize-rogue))


;;; history
(use-package recentf
  :defer 0.1
  :bind
  ("C-x f" . recentf-open)
  ("C-x F" . recentf-open-files)
  :config
  (recentf-mode +1)
  (add-to-list 'recentf-exclude xy/elpa-lisp-d)
  (add-to-list 'recentf-exclude xy/emacs-lisp-d)
  (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:")
  (setq recentf-max-saved-items 500
        recentf-max-menu-items 25))

(use-package saveplace
  :defer 0.1
  :config
  (save-place-mode +1))

(use-package savehist
  :defer 0.1
  :config
  (setq savehist-additional-variables '(kill-ring      ; clipboard
                                        register-alist ; keyboard macro
                                        mark-ring global-mark-ring ; mark
                                        search-ring regexp-search-ring ; search
                                        comint-input-ring))
  (setq history-length (* 100 2)
        history-delete-duplicates t)
  (setq list-command-history-max (* 32 6))
  (setq kill-ring-max (* 120 1))
  (setq mark-ring-max (* 16 2)
        global-mark-ring-max (* 16 2))
  (setq search-ring-max (* 16 2)
        regexp-search-ring-max (* 16 2))
  (setq comint-input-ring-size (* 500 1))
  (savehist-mode +1))


;;; minibuffer
(use-package minibuffer
  :config
  ;;; completion
  (setq completions-detailed t)
  (setq completion-styles '(basic initials substring partial-completion flex))
  (setq completion-category-overrides
        '((file (styles partial-completion))))
  (setq completion-ignore-case t
        read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t)
  (setq read-extended-command-predicate #'command-completion-default-include-p)

  ;;; completion buffer
  (setq completion-auto-help 'always
        completion-auto-select 'second-tab
        completion-no-auto-exit t
        completions-format 'one-column
        completions-sort 'historical
        completions-group t
        completions-max-height 20)

  ;;; minibuffer
  (setq enable-recursive-minibuffers t)
  (add-hook 'emacs-startup-hook #'minibuffer-depth-indicate-mode)
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;;;; minibuffer UX
  (setq use-short-answers t)
  (setq use-file-dialog nil)
  (setq use-dialog-box nil)
  (setq resize-mini-windows 'grow-only))

;; (use-package icomplete
;;   :defer 0.2
;;   :config
;;   (setq icomplete-compute-delay 0.01)
;;   (fido-vertical-mode +1))


(use-package dabbrev
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))


;;; keymap
(use-package ffap
  :defer 1
  :bind
  ("C-x M-f" . #'ffap-menu)
  :config
  (ffap-bindings))

(use-package repeat
  :defer 0.2
  :config
  (repeat-mode +1)
  (setq repeat-exit-key "RET"))

(use-package which-key
  :defer 1
  :config
  (which-key-mode +1)
  (setq which-key-lighter nil)
  (setq which-key-idle-delay 1.0
        which-key-idle-secondary-delay .0)
  (setq which-key-sort-order 'which-key-key-order-alpha
        which-key-sort-uppercase-first nil))


;;; buffer
(use-package ibuffer
  :bind
  ("C-x C-b" . #'ibuffer-jump)
  ("C-x M-b" . #'ibuffer)
  ("C-x 4 C-b" . #'ibuffer-other-window)
  :hook (ibuffer-mode . ibuffer-auto-mode)
  :config
  (defvar xy/boring-buffers '("\\` ")
    "List of buffer names of buffers to hide on several occasions.")
  (setq ibuffer-never-show-predicates xy/boring-buffers))


;;; window
(use-package winner
  :defer 1
  :bind
  ("C-x 4 u" . #'winner-undo)
  ("C-x 4 r" . #'winner-redo)
  :config
  (winner-mode +1))


;;; dired
(use-package dired
  :hook
  (dired-mode . dired-hide-details-mode)
  (dired-mode . dired-omit-mode)
  :config
  (setq dired-listing-switches "-lhFA -v")
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq dired-dwim-target t)
  (setq dired-recursive-copies 'always)
  (setq dired-create-destination-dirs 'ask)
  (setq dired-vc-rename-file t)
  (setq dired-omit-verbose nil))


;;; lisp
(use-package lisp-mode
  :init
  (defun xy/check-parens-before-save ()
    (add-hook 'before-save-hook #'check-parens 0 :local))
  (add-hook 'lisp-mode-hook #'xy/check-parens-before-save)
  (add-hook 'emacs-lisp-mode-hook #'xy/check-parens-before-save)
  :bind (("C-c e e" . #'pp-eval-last-sexp)
         ("C-c e p" . #'pp-eval-expression)
         ("C-c e j" . #'eval-print-last-sexp)
         ("C-c e f" . #'eval-defun)
         ("C-c e b" . #'eval-buffer)
         ("C-c e r" . #'eval-region)
         ("C-c e d" . #'edebug-defun)
         :map lisp-mode-shared-map
         ("C-c e c" . #'check-parens)))


;;; ui
(use-package hl-line
  :hook
  (emacs-startup . global-hl-line-mode))


;;; util
(use-package autorevert
  :defer 0.2
  :config
  (global-auto-revert-mode +1)
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-remote-files t))

(use-package outline
  :hook
  (emacs-lisp-mode . outline-minor-mode)
  :config
  (keymap-set outline-overlay-button-map "S-<return>" #'outline-cycle-buffer)
  (setq outline-minor-mode-use-buttons 'in-margins))

(use-package xref
  :defer t
  :config
  (setq xref-history-storage 'xref-window-local-history))

(use-package eldoc
  :defer t
  :config
  ;; (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  (setq eldoc-minor-mode-string nil))

(use-package ediff
  :defer t
  :config
  (setq ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-split-window-function #'split-window-horizontally
        ediff-merge-split-window-function #'split-window-horizontally))
