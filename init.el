;;; -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8 -*-

;;; preface
;; (set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load-file custom-file))

;; For finer granularity, use `system-type' or `system-configuration' directly.
(defconst xy/linux-p
  (eq system-type 'gnu/linux) ; 'berkeley-unix 'gnu 'gnu/kfreebsd
  "Are we running on a GNU/Linux system?")
(defconst xy/win-p
  (eq system-type 'windows-nt) ; 'cygwin 'ms-dos
  "Are we running on a MS-Window system?")
(defconst xy/mac-p
  (eq system-type 'darwin)
  "Are we running on a Mac system?")


;;; theme
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
  (load-theme 'modus-operandi)
  (keymap-global-set "<f12>" #'modus-themes-toggle))

(defun xy/load-theme (theme &optional no-confirm no-enable)
  "Load a single theme interactively. Without prefix argument, disable all other enabled themes."
  (interactive (eval (cadr (interactive-form 'load-theme))))
  (if (called-interactively-p)
      (message "[xy]: load theme: %s" theme))
  (unless current-prefix-arg
    (mapc #'disable-theme custom-enabled-themes))
  (funcall-interactively 'load-theme theme :no-confirm no-enable))

(keymap-global-set "C-h C-t" #'xy/load-theme)
(keymap-global-set "C-h M-t" #'disable-theme)

(defvar xy/after-enable-theme-hook nil
  "Normal hook run after enabling a theme.")
(defun xy/after-enable-theme (&rest _args)
  "Run `xy/after-enable-theme-hook'."
  (run-hooks 'xy/after-enable-theme-hook))
(advice-add 'enable-theme :after #'xy/after-enable-theme)


;;; font
(defconst xy/font-size (if xy/win-p 120 140))
(defconst xy/font-name "Maple Mono NF CN")
(set-face-attribute 'default nil :height xy/font-size :family xy/font-name)

(defun xy/select-font ()
  (interactive)
  (set-face-attribute 'default nil
                      :family (completing-read "Default font: " (font-family-list))))

(keymap-global-set "C-h C-y" #'xy/select-font)
;; @tip from `term/ns-win'
;; (keymap-global-set "s-t" #'menu-set-font)

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
  ;; brew install font-maple-mono-nf-cn
  (set-fontset-font t 'han "Maple Mono NF CN" nil 'append)
  ;; brew install font-lxgw-wenkai
  ;; LXGW WenKai Mono
  (set-fontset-font t 'han "霞鹜文楷等宽" nil 'append)
  ;; brew install font-sarasa-gothic
  ;; Sarasa Mono SC
  (set-fontset-font t 'han "等距更纱黑体 SC" nil 'append)
  ;; Microsoft YaHei
  (set-fontset-font t 'han "微软雅黑" nil 'append)
  ;; Heiti SC
  (set-fontset-font t 'han "黑体-简" nil 'append))


;;; keymap
;; @see http://xahlee.info/emacs/emacs/emacs_keybinding_functions.html
(keymap-global-set "M-/" #'hippie-expand)
(keymap-global-set "<backtab>" #'back-to-indentation)
;; @tip from `bindings'
;; (keymap-global-set "C-M-<backspace>" #'backward-kill-sexp)
(keymap-global-set "M-S-SPC" #'cycle-spacing)
(keymap-global-set "M-z" #'zap-up-to-char)

;; @tip from `mouse'
;; By binding these to down-going events, we let the user use the up-going event to make the selection, saving a click.
(global-set-key [down-mouse-3] #'context-menu-open)
(global-set-key [M-down-mouse-3] (mouse-menu-bar-map))
;; (global-set-key [C-down-mouse-1] #'mouse-buffer-menu)
;; (global-set-key [C-down-mouse-2] #'facemenu-menu)
;; (global-set-key [C-down-mouse-3] (mouse-menu-major-mode-map))

;; @tip not convenient use emacs in terminal or ms-windows yet
(keymap-global-set "s-x" #'execute-extended-command)
(keymap-global-set "s-X" #'execute-extended-command-for-buffer)
(keymap-global-set "s-/" "C-x C-;")
(keymap-global-set "s-<" (defun xy/open-init-file ()
                             (interactive)
                             (find-file user-init-file)))
(keymap-global-set "s->" (defun xy/open-init-dir ()
                             (interactive)
                             (dired user-emacs-directory)))

;; @tip I should practice more by using `C-]' for `abort-recursive-edit'
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

;; BUG: unfill not working because it no re-select marked region
(keymap-global-set "M-q" (defun xy/fill-or-unfill ()
                           "Like `fill-paragraph', but unfill if used twice."
                           (interactive)
                           (let ((fill-column
                                  (if (eq last-command 'xy/fill-or-unfill)
                                      (progn (setq this-command nil)
                                             (point-max))
                                    fill-column)))
                             (call-interactively #'fill-paragraph))))

;;; path
(defconst xy/mason-bin-dir (expand-file-name "~/.local/share/nvim/mason/bin"))
(add-to-list 'exec-path xy/mason-bin-dir)

(when (memq window-system '(ns))
  ;; https://www.emacswiki.org/emacs/ExecPath
  (defun xy/set-exec-path-from-shell-PATH ()
    "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell. This is particularly useful under macOS, where GUI apps are not started from a shell."
    (interactive)
    (let ((path-from-shell (replace-regexp-in-string
                            "[ \t\n]*$" "" (shell-command-to-string
                                            "$SHELL --login -c 'echo $PATH'"))))
      (setenv "PATH" path-from-shell)
      (setq exec-path (split-string path-from-shell path-separator))))
  (add-hook 'emacs-startup-hook 'xy/set-exec-path-from-shell-PATH))

(when xy/win-p
  (defconst xy/git-bin-dir (expand-file-name (file-name-concat (getenv "SCOOP") "apps/git/current/usr/bin")))
  (when (file-exists-p xy/git-bin-dir)
    ;; For (executable-find "ls")
    (add-to-list 'exec-path xy/git-bin-dir)
    ;; For (shell-command-to-string "ls")
    (setenv "PATH" (concat xy/git-bin-dir ";" (getenv "PATH"))))
  (setenv "LANG" "en_US")
  (cd "~/"))


;;; builtin package setup
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

;; (package-initialize)
(package-activate-all)
(unless (file-exists-p package-user-dir)
  (package-refresh-contents))

(setq use-package-always-ensure t)
(setq use-package-expand-minimally t)
(setq use-package-enable-imenu-support t)


;;; startup frame and screen
(use-package emacs
  :ensure nil
  :init
  (setq inhibit-default-init t)
  ;; (setq inhibit-startup-screen t)
  ;; (setq initial-major-mode #'org-mode)

  (setq frame-inhibit-implied-resize t)
  (setq frame-resize-pixelwise t)
  (setq default-frame-alist '((fullscreen . maximized)
                              (menu-bar-lines . 1)
                              (tool-bar-lines . 0)
                              (vertical-scroll-bars . nil)
                              (horizontal-scroll-bars . nil)
                              (cursor-type . bar)
                              (alpha . 100)
                              (alpha-background . 80)))
  ;; The variables must also be set to `nil' so users don't have to call the functions twice to re-enable them.
  (setq tool-bar-mode nil
        scroll-bar-mode nil)
  (setq frame-title-format
        '((:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   "%b"))))
  :config
  ;; (menu-bar-mode +1)
  ;; (tool-bar-mode -1)
  ;; (scroll-bar-mode -1)
  )


;;; basic
(use-package emacs
  :ensure nil
  :config
  ;;; tab
  (setq-default indent-tabs-mode nil)
  (setq tab-always-indent 'complete)
  (setq backward-delete-char-untabify-method 'hungry)

  ;;; completion
  ;; TODO `completion-preview-mode' in Emacs 30.
  (setq completions-detailed t)
  (setq completion-styles '(basic flex)) ; @see `completion-styles-alist' for available style
  (setq completion-category-overrides ; @see `completion-category-defaults' for available category
        '((file (styles partial-completion))))
  (setq completion-ignore-case t
        read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t)
  (setq read-extended-command-predicate #'command-completion-default-include-p)

  ;;;; completion buffer
  (setq completion-auto-help 'visible
        completion-auto-select 'second-tab
        completion-no-auto-exit t
        ;; completions-format 'one-column
        ;; completions-sort 'historical
        completions-max-height 20)

  ;;;; minibuffer
  ;; Allow nested minibuffers.
  (setq enable-recursive-minibuffers t)
  (add-hook 'emacs-startup-hook #'minibuffer-depth-indicate-mode)
  ;; Keep the cursor out of the read-only portions of the minibuffer.
  (setq minibuffer-prompt-properties
        '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (setq history-delete-duplicates t)

  ;;;; minibuffer UX
  (setq use-short-answers t)
  ;; Disable GUIs because they are inconsistent across systems, desktop environments, and themes, and they don't match the look of Emacs.
  (setq use-file-dialog nil)
  (setq use-dialog-box nil)
  (setq resize-mini-windows 'grow-only)

  ;;; kill
  (setq kill-do-not-save-duplicates t)
  (setq save-interprogram-paste-before-kill t)
  ;; (setq select-enable-clipboard nil)

  ;;; register
  (setopt register-use-preview 'insist)

  ;;; scroll
  (setq scroll-preserve-screen-position t)
  (setq scroll-margin 1) ; for C-l and auto-scroll
  (setq scroll-conservatively 999) ; for auto-scroll never centers point
  (setq next-screen-context-lines 15) ; for C-v/M-v
  (setq scroll-error-top-bottom t) ; for C-v/M-v move point to top/bottom
  (setq hscroll-margin 10
        hscroll-step 0
        auto-hscroll-mode 'current-line)
  (setq isearch-allow-scroll 'unlimited)
  ;; scroll performance
  ;; (setq fast-but-imprecise-scrolling t)
  ;; (setq jit-lock-defer-time 0.05)

  ;;; mouse
  (setq mouse-yank-at-point t)
  ;; (setq mouse-autoselect-window t)

  ;;; limit
  (setq large-file-warning-threshold (* 64 1024 1024)) ; 10m -> 64m
  (setq read-process-output-max (* 1024 1024)) ; 4k -> 1m
  (setq undo-limit (* 10 160000) ; 10x
        undo-strong-limit (* 10 240000)
        undo-outer-limit (* 10 24000000))
  (setq history-length 150)
  (setq list-command-history-max 100)
  (setq suggest-key-bindings 999)
  ;; Disable truncation of printed s-expressions
  ;; in the message buffer (C-x_C-e `eval-last-sexp') and scratch buffer (C-j `eval-print-last-sexp')
  (setq eval-expression-print-length nil
        eval-expression-print-level nil)
  (setq message-log-max 2000)
  ;; (lossage-size 500)

  ;;; lock
  (setq create-lockfiles nil)

  ;;; backup
  (setq make-backup-files nil)
  (setq backup-by-copying t)
  (setq backup-directory-alist
        `(("." . ,(concat user-emacs-directory "backup"))))
  (setq tramp-backup-directory-alist backup-directory-alist)

  ;;; auto-save
  ;; Enable `auto-save-mode' to prevent data loss in crash. Use `recover-file' or `recover-session' to restore unsaved changes.
  ;; Disable it can stop creating #filename# files.
  ;; By default, auto-saves happen every 300 keystrokes, or after around 30 seconds of idle time
  (setq auto-save-default nil)
  (setq auto-save-list-file-prefix
        (concat user-emacs-directory "auto-save"))
  (setq tramp-auto-save-directory
        (concat user-emacs-directory "tramp-auto-save"))
  (setq kill-buffer-delete-auto-save-files t)
  ;; auto-save for file
  ;; - 1.only saves file-visiting buffers
  ;; - 2.directly saving to the file itself without creating backup files
  ;; (auto-save-visited-mode +1)

  ;;; wrap
  ;; (global-visual-line-mode +1)
  ;; (setq word-wrap t)
  (setq word-wrap-by-category t)

  ;; truncate
  ;; @tip use "C-x x t" (`toggle-truncate-lines')
  ;; (add-hook 'prog-mode-hook
  ;;           (defun xy/truncate-lines ()
  ;;             (setq-local truncate-lines t)))
  (setq-default truncate-lines t)

  ;;; buffer
  (setq uniquify-buffer-name-style 'forward)
  (add-to-list 'display-buffer-alist
               '("\\*.*compilation\\*" (display-buffer-no-window))) ; Keep the compilation buffer in the background, except when there's an error

  ;;; window
  ;; (setq split-height-threshold nil
  ;;       split-width-threshold 0)

  ;; (require 'comint)
  (setq comint-input-ignoredups t
        comint-prompt-read-only t
        comint-scroll-to-bottom-on-input 'this
        comint-buffer-maximum-size (* 2 1024))

  ;; (require 'proced)
  (setq proced-auto-update-interval 1)
  (setq-default proced-auto-update-flag t)

  ;; (require 'net-utils)
  (setq netstat-program-options '("-atupe"))

  ;; (require 'calendar)
  (setq calendar-date-style 'iso
        calendar-week-start-day 1
        calendar-weekend-days '(6 0))

  (setq epg-pinentry-mode 'loopback)

  ;; `simple.el'
  (setq what-cursor-show-names t) ; For `C-x ='
  (setq set-mark-command-repeat-pop t)
  ;; Recenter to the middle of the window for `compile-goto-error', `wgrep', `embark-export'.
  (setq next-error-recenter '(4))
  ;; By default, emacs "updates" its ui more often than it needs to
  (setq idle-update-delay 1.0)
  ;; `files.el'
  (setq confirm-kill-emacs #'yes-or-no-p)
  (setq require-final-newline t)
  (setq remote-file-name-inhibit-cache 50)
  (setq remote-file-name-inhibit-delete-by-moving-to-trash t)
  ;; (setq find-file-suppress-same-file-warnings t)
  ;; Resolve symlinks so that operations are conducted from the file's directory
  (setq find-file-visit-truename t
        vc-follow-symlinks t)
  ;; `paren.el'
  (setq show-paren-context-when-offscreen 'overlay
        blink-matching-paren-highlight-offscreen t)
  (setopt show-paren-delay 0.2)
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t)
  ;; `paragraphs.el'
  (setq sentence-end-double-space nil) ; Don't assume that sentences should have two spaces after periods. This ain't a typewriter
  ;; `compile.el'
  (setq compilation-scroll-output 'first-error)
  ;; (setq compilation-always-kill t
  ;;       compilation-ask-about-save nil)
  ;; `C-code'
  ;; No beeping or blinking
  ;; (setq ring-bell-function #'ignore
  ;;       visible-bell nil)
  (setq show-trailing-whitespace t)
  (setq-default fill-column 80)
  (setq-default display-line-numbers-widen t) ; widen line numbers when in narrow
  (setq-default cursor-type 'bar))


;;; hooks and keymaps
(use-package emacs
  :ensure nil
  :hook
  (prog-mode . show-paren-local-mode)
  (prog-mode . electric-indent-local-mode)
  (prog-mode . electric-pair-local-mode)
  (prog-mode . subword-mode)
  (emacs-startup . global-display-line-numbers-mode)
  (emacs-startup . column-number-mode) ; modeline
  (emacs-startup . size-indication-mode) ; modeline
  (emacs-startup . pixel-scroll-precision-mode)
  (emacs-startup . delete-selection-mode)
  (emacs-startup . window-divider-mode)
  (emacs-startup . blink-cursor-mode)
  ;; (emacs-startup . context-menu-mode)
  ;; (emacs-startup . tab-bar-mode)
  ;; (emacs-startup . tab-bar-history-mode)
  ;; (emacs-startup . global-tab-line-mode)
  ;; (emacs-startup . global-display-fill-column-indicator-mode)
  (before-save . delete-trailing-whitespace)
  ;; (after-save . executable-make-buffer-file-executable-if-script-p) ; Only work if buffer begin with "#!"
  :init
  (defun xy/scratch ()
    "Jump to the *scratch* buffer. If it does not exist, create it."
    (interactive)
    (switch-to-buffer "*scratch*"))

  (when xy/linux-p
    (defun xy/wsl-kill (start end)
      "Copy/Kill text from an Emacs buffer for pasting it into a Windows app"
      (interactive "r")
      (let ((default-directory "/mnt/c/"))
        (shell-command-on-region start end "clip.exe")))
    (defun xy/wsl-yank ()
      "Paste/Yank text into Emacs buffer that has been copied from a Windows app"
      (interactive)
      (let ((coding-system-for-read 'dos)
            (default-directory "/mnt/c/"))
        (insert
         (substring (shell-command-to-string "powershell.exe -NoLogo -NoProfile -command 'Get-Clipboard'") 0  -1))))
    (keymap-global-set "C-z C-w" #'xy/wsl-kill)
    (keymap-global-set "C-z C-y" #'xy/wsl-yank))
  :bind
  ;; @tip "s-k" is `kill-current-buffer'
  ;; ("C-x k" . #'kill-current-buffer)
  ("C-x K" . #'bury-buffer)
  ("C-x O" . #'switch-to-minibuffer)
  ("C-x l" . #'count-words)
  ("C-x x v" . #'view-buffer)
  ("C-x x f" . #'follow-mode)
  ;;
  ("C-z" . nil) ; `suspend-frame'
  ("C-z C-r" . #'redraw-display)
  ("C-z s s" . #'xy/scratch)
  ("C-z e s" . #'shell)
  ("C-z e e" . #'eshell)
  ("C-z w w" . #'browse-url)
  ("C-z w W" . #'browse-web))


;;; misc
(use-package emacs
  :ensure nil
  :config
  (setq user-full-name    "Xavier Young"
        user-mail-address "younger321@foxmail.com")

  (put 'narrow-to-region 'disabled nil)
  (put 'narrow-to-page 'disabled nil)
  (put 'scroll-left 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'erase-buffer 'disabled nil)

  (defconst xy/elpa-lisp-d (expand-file-name package-user-dir))
  (defconst xy/emacs-lisp-d (file-name-directory (directory-file-name doc-directory)))

  (dir-locals-set-class-variables
   :read-only
   '((nil . ((eval . (view-mode-enter nil #'kill-buffer))
             (tab-width . 8)))))
  (dolist (dir (list xy/elpa-lisp-d xy/emacs-lisp-d))
    (dir-locals-set-directory-class (file-truename dir) :read-only)))

;; Ensure adding the following compile-angel code at the very beginning of init file, before all other packages.
;; (use-package compile-angel
;;   :demand t
;;   :config
;;   ;; When set to nil, compile-angel won't show which file is being compiled.
;;   (setq compile-angel-verbose t)

;;   (push "/early-init.el" compile-angel-excluded-files)
;;   (push "/init.el" compile-angel-excluded-files)

;;   ;; A local mode that compiles .el files whenever the user saves them.
;;   ;; (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode)

;;   ;; A global mode that compiles .el files before they are loaded.
;;   (compile-angel-on-load-mode))


;;; help
(use-package help
  :ensure nil
  :init
  (setq help-window-select t)
  (setq help-window-keep-selected t)
  (setq help-enable-autoload t
        help-enable-completion-autoload t
        help-enable-symbol-autoload t)
  (setopt help-at-pt-display-when-idle t)
  (setq help-clean-buttons t)
  (setq describe-bindings-outline t)
  ;; (setq apropos-do-all t)
  ;; (setq apropos-sort-by-scores 'verbose)

  (defun xy/find-feature ()
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
         ("C-h x" . #'list-command-history)
         ("C-h X" . #'command-history)
         ;;
         ("C-h l" . #'xy/find-feature) ; `view-lossage'
         ("C-h L" . #'unload-feature) ; `describe-language-environment'
         ;;
         ("C-h j i" . #'describe-icon)
         ("C-h j c" . #'describe-char)
         ("C-h j f" . #'describe-face)
         ("C-h j F" . #'list-faces-display)
         ("C-h j g" . #'describe-font)
         ("C-h j h" . #'describe-fontset)
         ("C-h j t" . #'describe-theme)
         ("C-h j s" . #'describe-syntax)
         ("C-h j w" . #'describe-widget) ; or "C-u C-h ."
         ("C-h j b" . #'button-describe)
         ("C-h j W" . #'widget-describe)
         ("C-h j I" . #'describe-input-method)
         ("C-h j l" . #'describe-language-environment)
         ("C-h j p" . #'describe-text-properties)
         ("C-h j C" . #'describe-current-coding-system)
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
         ("C-h o" . nil) ; `describe-symbol'
         ("C-h o f" . #'add-file-local-variable)
         ("C-h o d" . #'add-dir-local-variable)
         ;;
         ("C-h t" . #'help-with-tutorial)
         ("C-h u" . #'shortdoc)
         ;;
         ("C-h d" . nil)
         ("C-h I" . nil)
         ("C-h C" . nil)
         ("C-h C-c" . nil)
         ("C-h C-d" . nil)
         ("C-h C-e" . nil)
         ("C-h C-m" . nil)
         ("C-h C-n" . nil)
         ("C-h C-o" . nil)
         ("C-h C-w" . nil)
         :map help-mode-map
         ("C" . #'set-variable)
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

(use-package package
  :ensure nil
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

(use-package tooltip
  :ensure nil
  :config
  (tooltip-mode -1)
  (setq tooltip-resize-echo-area t))


;;; history
;; Pick recently visited files
(use-package recentf
  :ensure nil
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

;; Goto the last location within a file upon reopening
(use-package saveplace
  :ensure nil
  :defer 0.1
  :config
  (save-place-mode +1))

;; Save various kind of history between sessions
(use-package savehist
  :ensure nil
  :defer 0.1
  :config
  (savehist-mode +1)
  (setq savehist-additional-variables '(kill-ring      ; clipboard
                                        register-alist ; keyboard macro
                                        mark-ring global-mark-ring ; mark
                                        search-ring regexp-search-ring ; search
                                        comint-input-ring)))


;;; minibuffer
(use-package icomplete
  :ensure nil
  :defer 0.2
  :config
  (fido-vertical-mode +1))

(defvar xy/boring-buffers '("\\` "
                            "\\`\\*Echo Area"
                            "\\`\\*Minibuf"
                            "\\`\\*Completions"
                            "\\`\\*Flymake log"
                            "\\`\\*Semantic SymRef"
                            "\\`\\*Backtrace"
                            "\\`\\*tramp"
                            "\\`\\*EGLOT"
                            ;; And some hidden buffers can be visited by ...
                            "\\`\\*scratch"        ; "C-z s s"
                            "\\`\\*Messages"       ; "C-h h e"
                            "\\`\\*Bookmark List"  ; "C-x r l"
                            "\\`\\*Ibuffer"        ; "C-x C-b"
                            )
  "List of buffer names of buffers to hide on several occasions.")


;;; keymap
(use-package ffap
  :ensure nil
  :defer 1
  :bind
  ("C-x M-f" . #'ffap-menu)
  :config
  ;; @tip
  ;; (keymap-global-set "S-<mouse-3>" 'ffap-at-mouse)
  ;; (keymap-global-set "C-S-<mouse-3>" 'ffap-menu)
  (ffap-bindings))

(use-package repeat
  :ensure nil
  :defer 0.2
  :config
  (repeat-mode +1)
  (setq repeat-exit-key "RET"))

(use-package which-key
  :defer 1
  :config
  (which-key-mode +1)
  (setq which-key-lighter nil)
  (setq which-key-idle-delay .5
        which-key-idle-secondary-delay .0)
  (setq which-key-sort-order 'which-key-key-order-alpha
        which-key-sort-uppercase-first nil))

(use-package keyfreq
  :defer 1
  :bind ("C-h w f" . keyfreq-show)
  :config
  (setq keyfreq-excluded-commands
        '(;; self-insert-command
          ;; forward-char
          ;; backward-char
          ;; previous-line
          ;; next-line
          pixel-scroll-precision
          mwheel-scroll))
  (keyfreq-mode +1)
  (keyfreq-autosave-mode +1))


;;; buffer
(use-package ibuffer
  :ensure nil
  :bind
  ("C-x C-b" . #'ibuffer-jump)
  ("C-x M-b" . #'ibuffer)
  ("C-x 4 C-b" . #'ibuffer-other-window)
  :hook (ibuffer-mode . ibuffer-auto-mode)
  :config
  (setq ibuffer-never-show-predicates xy/boring-buffers))


;;; window
(use-package winner
  :ensure nil
  :defer 1
  :bind
  ("C-x 4 u" . #'winner-undo)
  ("C-x 4 r" . #'winner-redo)
  :config
  (winner-mode +1))

(use-package windmove
  :ensure nil
  :defer 1
  :config
  (windmove-default-keybindings 'ctrl)
  (windmove-swap-states-default-keybindings '(ctrl shift)))


;;; ui
(use-package hl-line
  :ensure nil
  :hook
  (prog-mode . hl-line-mode)
  (text-mode . hl-line-mode)
  (help-mode . hl-line-mode)
  (Info-mode . hl-line-mode)
  (dired-mode . hl-line-mode)
  (package-menu-mode . hl-line-mode))


;;; lisp
(use-package lisp-mode
  :ensure nil
  :init
  (defun xy/check-parens-before-save ()
    (add-hook 'before-save-hook #'check-parens 0 :local))
  (add-hook 'lisp-mode-hook #'xy/check-parens-before-save)
  (add-hook 'emacs-lisp-mode-hook #'xy/check-parens-before-save)
  :bind ( :map lisp-mode-shared-map
          ("C-h e c" . #'check-parens)))

(use-package macrostep
  :bind ( :map lisp-mode-shared-map
          ("C-h e m" . macrostep-expand)))


;;; dired
(use-package dired
  :ensure nil
  :hook
  (dired-mode . dired-hide-details-mode)
  (dired-mode . dired-omit-mode)
  (dired-mode . hl-line-mode)
  :config
  ;; flags for `insert-directory-program'. Or: -alh, --group-directories-first
  (setq dired-listing-switches "-lhFA -v")
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq dired-dwim-target t) ; next windows as target for file copy, rename etc
  ;; use trash
  (setq delete-by-moving-to-trash t)
  (setq dired-recursive-deletes 'always) ; don't ask when directory not empty
  (setq dired-recursive-copies 'always)
  (setq dired-create-destination-dirs 'ask)
  ;; (require 'image-dired)
  (setq image-dired-thumb-margin 1
        image-dired-thumb-relief 0
        ;; Store thumbnails in the system-wide thumbnail location
        ;; e.g. ~/.local/cache/thumbnails to make them reusable by other programs
        image-dired-thumbnail-storage 'standard-large))

(use-package dired-subtree
  :after dired
  :bind ( :map dired-mode-map
          ("<tab>" . dired-subtree-toggle)
          ("<backtab>" . dired-subtree-remove)
          ("C-<tab>" . dired-subtree-cycle)
          ;; ("TAB" . dired-subtree-toggle)
          ;; ("S-TAB" . dired-subtree-remove)
          ;; ("C-TAB" . dired-subtree-cycle)
          )
  :config
  (setq dired-subtree-use-backgrounds nil))


;;; util
(use-package autorevert
  :ensure nil
  :defer 0.2
  :config
  (global-auto-revert-mode +1)
  ;; @tip C-x x g is `revert-buffer-quick', s-u is `revert-buffer'
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-remote-files t))

(use-package trashed
  :bind ("C-x C-d" . trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

(use-package outline
  :hook
  (emacs-lisp-mode . outline-minor-mode)
  :config
  ;; @tip
  ;; RET at beginning of headers line trigger `outline-cycle'
  ;; TAB on a heading line trigger `outline-cycle'
  ;; (setq outline-minor-mode-cycle t)
  ;; C-q `outline-hide-sublevels': Obly top n (default 1, can prefix) headers visible
  ;; C-t `outline-hide-body': Hide all body lines in buffer, leaving all headings visible.
  (setopt outline-minor-mode-prefix (kbd "C-c v")) ; v for view
  ;;; UI
  ;; (setq outline-minor-mode-highlight 'append)
  (setq outline-minor-mode-use-buttons 'in-margins))

(use-package xref
  :ensure nil
  :config
  (setq xref-history-storage 'xref-window-local-history))

(use-package eldoc
  :ensure nil
  :defer t
  :config
  (setq eldoc-minor-mode-string nil)
  (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  (setq eldoc-echo-area-display-truncation-message nil))

(use-package ediff
  :ensure nil
  :defer t
  :config
  (setq ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-split-window-function #'split-window-horizontally
        ediff-merge-split-window-function #'split-window-horizontally))


;;; tool
(use-package flymake
  :ensure nil
  :defer t
  ;; :hook (emacs-lisp-mode)
  :bind ( :map flymake-mode-map
          ("M-g n" . flymake-goto-next-error)
          ("M-g p" . flymake-goto-prev-error)
          ("M-g e" . flymake-show-buffer-diagnostics)
          ("M-g E" . flymake-show-project-diagnostics))
  :config
  (remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake))

(use-package org
  :ensure nil
  :defer t
  :bind
  ("C-z o d" . #'xy/open-org-dir)
  ("C-z o o" . #'xy/open-org-notes)
  ("C-z o c" . #'org-capture)
  ("C-z o a" . #'org-agenda)
  ("C-z o l" . #'org-insert-link)
  ("C-z o L" . #'org-store-link)
  ("C-z o ;" . #'org-toggle-link-display)
  ("C-z o p" . #'org-publish)
  :init
  (defun xy/open-org-dir ()
    (interactive)
    (dired org-directory))
  (defun xy/open-org-notes ()
    "Visit the Org notes file."
    (interactive)
    (find-file org-default-notes-file))
  :config
  (add-hook 'org-mode-hook #'visual-line-mode)
  ;; Alignment of tags at the end of headlines
  (setq  org-auto-align-tags t
         org-tags-column 0)
  (setq org-reverse-note-order t) ; Put newer notes on top of the file
  (setq org-directory "~/org/"
        org-default-notes-file (concat org-directory "notes.org"))
  (setq org-todo-keywords ; Set some sensible default states for todo-items
        '((sequence "TODO(t)" "PROJ(p)" "LOOP(r)" "STRT(s)" "WAIT(w)" "HOLD(h)" "IDEA(i)" "|" "DONE(d)" "KILL(k)")
          (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
          (sequence "|" "OKAY(o)" "YES(y)" "NO(n)")))
  (setq org-publish-timestamp-directory ; Where to place the directory containing the timestamps about changed files
        (concat user-emacs-directory "org-timestamps/"))
  (setq org-html-checkbox-type 'unicode
        org-html-prefer-user-labels t
        org-html-self-link-headlines t))


;;; terminal
(unless (display-graphic-p)
  (keymap-global-set "<mouse-4>" #'scroll-down-line)
  (keymap-global-set "<mouse-5>" #'scroll-up-line)
  (keymap-global-set "S-<mouse-4>" (defun xy/scroll-right () (interactive) (scroll-right 2)))
  (keymap-global-set "S-<mouse-5>" (defun xy/scroll-left () (interactive) (scroll-left 2)))
  (keymap-global-set "M-<mouse-4>" (defun xy/scroll-down++ () (interactive) (scroll-down-line 5)))
  (keymap-global-set "M-<mouse-5>" (defun xy/scroll-up++ () (interactive) (scroll-up-line 5)))

  (use-package xt-mouse
    :defer 1
    :config
    (xterm-mouse-mode +1))

  ;; NOTE: need xclip at linux
  ;; Allow Emacs to copy to and paste from the GUI clipboard when running in a text terminal
  (when (and xy/linux-p (executable-find "xclip"))
    (use-package xclip
      :defer 1
      :config
      (xclip-mode +1))))


;;; icon
;; Remember to run `nerd-icons-install-fonts' nerd icon if system doesn't have
;; Then restart Emacs to see the effect.
(use-package nerd-icons
  :defer t)

;; Use human readable file size in ibuffer by `nerd-icons-ibuffer-human-readable-size'
(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode))

(use-package nerd-icons-dired
  :hook (dired-mode))

(use-package nerd-icons-completion
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))
