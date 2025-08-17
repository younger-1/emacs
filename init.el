;;; -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8 -*-

;;; preface
(message "** [xy] boot init.el")

(eval-and-compile
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory)))

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
  (setopt modus-themes-italic-constructs t
          modus-themes-bold-constructs t
          modus-themes-mixed-fonts t
          modus-themes-variable-pitch-ui nil
          modus-themes-prompts '(light)
          ;; modus-themes-completions '((matches . (underline))
          ;;                            (selection . (bold)))
          modus-themes-headings '((1 . (1.4))
                                  (2 . (1.2))
                                  (3 . (1.1))))
  ;; (load-theme 'modus-vivendi)
  ;; (load-theme 'modus-operandi)
  (load-theme 'modus-operandi-deuteranopia)
  (setq modus-themes-to-toggle '(modus-operandi-deuteranopia modus-vivendi))
  (keymap-global-set "C-c y m t" #'modus-themes-toggle)
  (keymap-global-set "C-c y m s" #'modus-themes-select))

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

(keymap-global-set "C-c y f" #'xy/select-font)

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
                      :height 1.0)
  ;; Set the fonts for the inactive mode line
  (set-face-attribute 'mode-line-inactive nil
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 1.0))

(add-hook 'xy/after-enable-theme-hook #'xy/set-default-face-advanced)

(when (fboundp 'set-fontset-font)
  ;; Heiti SC (mac)
  (set-fontset-font t 'han "黑体-简" nil 'prepend)
  ;; Microsoft YaHei (windows)
  (set-fontset-font t 'han "微软雅黑" nil 'prepend)
  ;; Sarasa Mono SC -> brew install font-sarasa-gothic
  (set-fontset-font t 'han "等距更纱黑体 SC" nil 'prepend)
  ;; LXGW WenKai Mono -> brew install font-lxgw-wenkai
  (set-fontset-font t 'han "霞鹜文楷等宽" nil 'prepend)
  ;; brew install font-maple-mono-nf-cn
  (set-fontset-font t 'han "Maple Mono NF CN" nil 'prepend))


;;; keymap
;; free keys: C-x c/g/j/z
;; [C-x e] `kmacro-end-and-call-macro'
;; [C-x f] `set-fill-column'
;; [C-x i] `insert-file'
;; [C-x l] `count-lines-page'
;; [C-x m] `compose-mail'
;; [C-x q] `kbd-macro-query'
;;
;; @see http://xahlee.info/emacs/emacs/emacs_keybinding_functions.html
;; @see (info "(elisp) Key Binding Conventions") to know which keys are safe for users
(keymap-global-set "C-," (defun xy/open-init-file ()
                           (interactive)
                           (find-file user-init-file)))
(keymap-global-set "C-<" (defun xy/open-init-dir ()
                           (interactive)
                           (dired user-emacs-directory)))
;; (keymap-global-set "C-S-v" #'scroll-other-window)
;; (keymap-global-set "M-S-v" #'scroll-other-window-down) ; FIXME: M-S-v is not M-V

;; @tip from `subr'
;; `global-map' `ctl-x-map' `esc-map'

;; @tip from `bindings'
;;   <next> /   C-v /   [fn-down] -> `scroll-up-command'
;; M-<next> / M-C-v / [M-fn-down] -> `scroll-other-window'
;; C-<next> / C-x < -> `scroll-left'
;;
;; C-M-a / C-M-e -> `beginning-of-defun' / `end-of-defun'
;; C-M-h / C-M-x -> `mark-defun' / `eval-defun'
;; C-M-k / C-M-<backspace> -> `kill-sexp' / `backward-kill-sexp'
;; C-M-t -> `transpose-sexps'
;; C-o / C-M-o -> `open-line' / `split-line'
;; M-j / C-M-j -> `default-indent-new-line'
;; C-x ESC ESC -> `repeat-complex-command'
(keymap-global-set "M-=" #'count-words)
(keymap-global-set "M-z" #'zap-up-to-char)
;; M-SPC -> `cycle-spacing'
;; M-m -> `back-to-indentation'
(keymap-global-set "<backtab>" #'back-to-indentation)
;; M-; -> `comment-dwim'
(keymap-global-set "C-;" #'comment-line)

;; @tip from `indent'
;; C-x TAB -> `indent-rigidly'
;; C-M-\ -> `indent-region'
;; M-i -> `tab-to-tab-stop'
;; C-M-q -> `indent-pp-sexp' ; from `emacs-lisp-mode-map'

;; @tip from `files' / `window'
(keymap-global-set "S-<return>" #'save-buffer)
;; M-r -> `move-to-window-line-top-bottom'

;; @tip from `mouse' / `menu-bar' / `tmm' / `facemenu'
;; <f10> -> `menu-bar-open' ; M-` -> `tmm-menubar'
;; By binding these to down-going events, we let the user use the up-going event to make the selection, saving a click.
(global-set-key [down-mouse-3] #'context-menu-open)
(global-set-key [M-down-mouse-3] `(menu-item ,(purecopy "Menu Bar") ignore :filter ,(lambda (_) (mouse-menu-bar-map))))
;; (global-set-key [C-down-mouse-1] #'mouse-buffer-menu)
;; (global-set-key [C-down-mouse-2] #'facemenu-menu)
;; (global-set-key [C-down-mouse-3] (mouse-menu-major-mode-map))

;; disable `mouse-wheel-text-scale' by `mouse-wheel-mode' when use `pixel-scroll-precision-mode'
(keymap-global-unset "C-<wheel-down>")
(keymap-global-unset "C-<wheel-up>")

;; @tip from `term/ns-win'
;; s-w -> `delete-frame'
;; s-t -> `menu-set-font'
;; s-, -> `customize'
(keymap-global-set "M-s-," #'customize-group)
(keymap-global-set "s-x" #'execute-extended-command)
(keymap-global-set "s-X" #'execute-extended-command-for-buffer)
(keymap-global-set "s-<return>" #'toggle-frame-fullscreen) ; <f11>
(keymap-global-set "S-s-<return>" #'toggle-frame-maximized) ; M-<f10>
;; s-z -> undo
(keymap-global-set "s-Z" #'undo-redo)

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

(when (and xy/mac-p (display-graphic-p)) ; (memq window-system '(ns))
  ;; https://www.emacswiki.org/emacs/ExecPath
  (defun xy/set-exec-path-from-shell-PATH ()
    "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell. This is particularly useful under macOS, where GUI apps are not started from a shell."
    (interactive)
    (let ((path-from-shell (replace-regexp-in-string
                            "[ \t\n]*$" "" (shell-command-to-string
                                            "$SHELL --login -c 'echo $PATH'"))))
      ;; For (shell-command-to-string "gls")
      (setenv "PATH" path-from-shell)
      ;; For (executable-find "gls")
      ;; (setq exec-path (split-string path-from-shell path-separator))
      (dolist (path (split-string path-from-shell path-separator))
        (add-to-list 'exec-path path))))
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
                         ("gnu-dev". "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu-devel/")
                         ("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")))
;; (setq package-archive-priorities '(("gnu"    . 90)
;;                                    ("nongnu" . 80)
;;                                    ("melpa"  . 10)))
;; Enable `package-quickstart-refresh'
(setq package-quickstart t)
(setq package-install-upgrade-built-in t)
;; (setq package-native-compile t)

;; (package-initialize)
(package-activate-all)
(unless (file-exists-p package-user-dir)
  (package-refresh-contents))

(setq use-package-always-ensure t)
(setq use-package-always-defer t)
(setq use-package-enable-imenu-support t)
(setq use-package-expand-minimally t)
(setq use-package-compute-statistics init-file-debug)
;; (setq use-package-minimum-reported-time (if init-file-debug 0 0.1))
;; (setq use-package-verbose init-file-debug)


;;; startup frame and screen
(use-package emacs
  :ensure nil
  :custom
  (inhibit-startup-echo-area-message user-login-name)
  :init
  (setq inhibit-default-init t)
  ;; (setq inhibit-startup-screen t)
  ;; (setq inhibit-x-resources t)
  ;; (setq initial-major-mode #'org-mode)

  ;; Font compacting can be very resource-intensive, especially when rendering icon fonts on Windows. This will increase memory usage.
  (setq inhibit-compacting-font-caches t)

  (setq frame-inhibit-implied-resize t)
  (setq frame-resize-pixelwise t)
  (setq default-frame-alist '((fullscreen . maximized)
                              (menu-bar-lines . 1)
                              (tool-bar-lines . 0)
                              (vertical-scroll-bars . nil)
                              (horizontal-scroll-bars . nil)
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
  ;; tab
  (setq-default indent-tabs-mode nil)
  ;; (setq-default tab-width 4)
  ;; TAB key for indentation+completion. `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)
  (setq backward-delete-char-untabify-method 'hungry)

  ;; mark
  (setq mark-even-if-inactive nil)

  ;; kill
  (setq kill-do-not-save-duplicates t)
  (setq save-interprogram-paste-before-kill t)
  ;; (setq select-enable-clipboard nil)

  ;; register
  (setopt register-use-preview 'insist)

  ;; scroll
  ;; @tip
  ;; C-v/M-v/auto-scroll -> keep point, scroll buffer up/dwon relative to the window
  ;; C-p/C-n/M-}/M-{ -> move point, trigger auto-scroll if point out of window
  (setq scroll-preserve-screen-position t) ; vim flavor
  (setq scroll-margin 2 ; for C-l and auto-scroll
        scroll-conservatively 3) ; avoid auto-scroll if point move off margin
  (setq next-screen-context-lines 15) ; for C-v/M-v
  (setq scroll-error-top-bottom t) ; for C-v/M-v move point to top/bottom
  (setq hscroll-margin 10
        hscroll-step 0
        auto-hscroll-mode 'current-line)
  ;; scroll performance
  ;; (setq fast-but-imprecise-scrolling t)
  ;; (setq jit-lock-defer-time 0.05)

  ;; mouse
  (setq mouse-yank-at-point t)
  (setq mouse-autoselect-window t)

  ;; limit
  (setq large-file-warning-threshold (* 64 1024 1024)) ; 10m -> 64m
  (setq read-process-output-max (* 1024 1024)) ; 4k -> 1m
  (setq undo-limit (* 10 160000) ; 10x
        undo-strong-limit (* 10 240000)
        undo-outer-limit (* 10 24000000))
  (setq message-log-max 3000)
  ;; (lossage-size 500)
  ;; Reduce truncation of printed s-expressions in the message buffer (C-x_C-e `eval-last-sexp') and scratch buffer (C-j `eval-print-last-sexp')
  (setq eval-expression-print-length (* 12 3) ; 3x
        eval-expression-print-level (* 4 3))
  (setq echo-keystrokes 0.1)
  (setq suggest-key-bindings 999)

  ;; lock
  (setq create-lockfiles nil)
  (setq remote-file-name-inhibit-locks t)

  ;; backup
  (setq make-backup-files nil)
  (setq backup-by-copying t)
  (setq backup-directory-alist
        `(("." . ,(concat user-emacs-directory "backup"))))
  (setq tramp-backup-directory-alist backup-directory-alist)

  ;; auto-save
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
  ;; -- 1.only saves file-visiting buffers
  ;; -- 2.directly saving to the file itself without creating backup files
  ;; (auto-save-visited-mode +1)

  ;; wrap
  ;; (global-visual-line-mode +1)
  (setq-default word-wrap t)
  (setq word-wrap-by-category t)

  ;; truncate
  ;; Auto truncate lines
  (setq truncate-partial-width-windows 80)
  ;; @tip use "C-x x t" (`toggle-truncate-lines')
  ;; (setq-default truncate-lines t)
  (defun xy/truncate-lines ()
    (setq-local truncate-lines t))
  (add-hook 'prog-mode-hook #'xy/truncate-lines)
  (add-hook 'log-view-mode-hook #'xy/truncate-lines)

  ;; buffer
  (setq uniquify-buffer-name-style 'forward)
  ;; Keep the compilation buffer in the background, except when there's an error
  (add-to-list 'display-buffer-alist
               '("\\*.*compilation\\*" (display-buffer-no-window)))

  ;; window
  ;; (setq split-height-threshold nil
  ;;       split-width-threshold 0)

  ;; cursor
  (setq-default cursor-type 'box)
  (setq x-stretch-cursor t)
  (blink-cursor-mode -1)

  ;; edit
  ;; (setq next-line-add-newlines t)
  (setq comment-empty-lines t)
  ;; (setq comment-multi-line t)
  (setq-default fill-column 80)
  ;; Disable the obsolete practice of end-of-line spacing from the typewriter era.
  (setq sentence-end-double-space nil)
  ;; According to the POSIX, a line is defined as "a sequence of zero or more non-newline characters followed by a terminating newline".
  (setq require-final-newline t)

  ;; proced
  (setq proced-auto-update-interval 1)
  (setq-default proced-auto-update-flag t)

  ;; flyspell
  ;; (setq flyspell-issue-welcome-flag nil)
  ;; Greatly improves flyspell performance by preventing messages from being displayed for each word when checking the entire buffer.
  ;; (setq flyspell-issue-message-flag nil)

  ;; ispell
  ;; Disable ispell completion to avoid annotation errors when no `ispell' dictionary is set.
  ;; (setq text-mode-ispell-word-completion nil)
  (setq ispell-silently-savep t)

  ;; GnuPG
  (setq epg-pinentry-mode 'loopback)

  ;; misc
  (require 'net-utils)
  (setq netstat-program-options '("-atupe"))

  (setq calendar-date-style 'iso
        calendar-week-start-day 1
        calendar-weekend-days '(6 0))

  ;; [M-s M-w] -> `eww-search-words'
  (setq eww-search-prefix "https://www.bing.com/search?q=")

  ;; `simple.el'
  (setq what-cursor-show-names t) ; For `C-x ='
  (setq set-mark-command-repeat-pop t)
  ;; Recenter to the middle of the window for `compile-goto-error', `wgrep', `embark-export'.
  (setq next-error-recenter '(4))
  ;; (setq next-error-message-highlight 'keep)
  ;; (setq list-matching-lines-jump-to-current-line t)
  ;; By default, emacs "updates" its ui more often than it needs to
  (setq idle-update-delay 1.0)
  ;; `files.el'
  (setq delete-by-moving-to-trash t)
  (setq confirm-kill-emacs #'yes-or-no-p)
  ;; (setq confirm-kill-processes nil)
  (setq remote-file-name-inhibit-cache 50)
  (setq remote-file-name-inhibit-delete-by-moving-to-trash t)
  ;; (setq find-file-suppress-same-file-warnings t)
  ;; Resolve symlinks so that operations are conducted from the real file's directory
  (setq find-file-visit-truename t
        vc-follow-symlinks t)
  (setq view-read-only t)
  ;; `paren.el'
  (setq show-paren-context-when-offscreen 'overlay
        blink-matching-paren-highlight-offscreen t)
  (setopt show-paren-delay 0.2)
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t)
  ;; `compile.el'
  (setq compilation-scroll-output 'first-error)
  ;; (setq compilation-always-kill t
  ;;       compilation-ask-about-save nil)
  ;; `C-code'
  ;; No beeping or blinking
  ;; (setq ring-bell-function #'ignore
  ;;       visible-bell nil)
  (setq-default display-line-numbers-widen t) ; widen line numbers when in narrow
  ;; (setq-default show-trailing-whitespace t)
  ;; (setq-default indicate-empty-lines t)
  (setq-default indicate-buffer-boundaries 'left))


;;; hooks and keymaps
(use-package emacs
  :ensure nil
  :init
  ;; (show-paren-mode +1) ;; default
  ;; (electric-indent-mode +1) ;; default
  ;; (electric-pair-mode +1)
  ;; (global-subword-mode +1)
  :hook
  ;; (prog-mode . show-paren-local-mode)
  ;; (prog-mode . electric-indent-local-mode)
  (prog-mode . electric-pair-local-mode)
  (prog-mode . subword-mode)
  ;;
  (emacs-startup . global-display-line-numbers-mode)
  (emacs-startup . column-number-mode) ; modeline
  (emacs-startup . size-indication-mode) ; modeline
  ;; (emacs-startup . pixel-scroll-precision-mode)
  (emacs-startup . delete-selection-mode)
  (emacs-startup . window-divider-mode)
  (emacs-startup . undelete-frame-mode)
  ;; (emacs-startup . context-menu-mode)
  (emacs-startup . global-display-fill-column-indicator-mode)
  (before-save . delete-trailing-whitespace)
  ;; (after-save . executable-make-buffer-file-executable-if-script-p) ; Only work if buffer begin with "#!"

  :config
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
  ;; @tip s-k is `kill-current-buffer'
  ;; ("C-x k" . #'kill-current-buffer)
  ("C-x K" . #'bury-buffer)
  ("C-x O" . #'switch-to-minibuffer)
  ;; ("C-x l" . #'count-words)
  ("C-x x f" . #'follow-mode)
  ;;
  ("C-x D" . #'diff-buffer-with-file)
  ;; ("C-x D" . #'diff-buffers)
  ;;
  ("C-z" . nil) ; `suspend-frame'
  ("C-z C-r" . #'redraw-display)
  ("C-z s s" . #'xy/scratch)
  ;;
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
  (put 'set-goal-column 'disabled nil)
  (put 'erase-buffer 'disabled nil)
  (put 'dired-find-alternate-file 'disabled nil)
  (put 'list-timers 'disabled nil)

  (defconst xy/elpa-lisp-d (expand-file-name package-user-dir))
  (defconst xy/emacs-lisp-d (file-name-directory (directory-file-name doc-directory)))

  (dir-locals-set-class-variables
   :read-only
   '((nil . (;; (eval . (view-mode-enter nil #'kill-buffer))
             (buffer-read-only . t)
             (tab-width . 8)))))
  (dolist (dir (list xy/elpa-lisp-d xy/emacs-lisp-d))
    (dir-locals-set-directory-class (file-truename dir) :read-only)))

;; Ensure adding the following compile-angel code at the very beginning of init file, before all other packages.
;; (use-package compile-angel
;;   :demand t
;;   :config
;;   ;; When set to nil, compile-angel won't show which file is being compiled.
;;   (setq compile-angel-verbose t)
;;
;;   (push "/early-init.el" compile-angel-excluded-files)
;;   (push "/init.el" compile-angel-excluded-files)
;;
;;   ;; A local mode that compiles .el files whenever the user saves them.
;;   ;; (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode)
;;
;;   ;; A global mode that compiles .el files before they are loaded.
;;   (compile-angel-on-load-mode))

(use-package server
  :ensure nil
  ;; :if (dispay-graphic-p)
  ;; :after-call doom-first-input-hook doom-first-file-hook focus-out-hook
  :defer 1
  :config
  ;; (setq server-client-instructions nil)
  ;; File with `server-name' under `server-auth-dir'
  (setq server-use-tcp t)
  (unless (or (server-running-p) (daemonp))
    (server-start)))


;;; help
(use-package help
  :ensure nil
  :init
  ;; (setq help-window-select t)
  ;; (setq help-window-keep-selected t)
  ;; (add-to-list 'display-buffer-alist
  ;;              '("*Help*" display-buffer-same-window))
  (setq help-enable-autoload t
        help-enable-completion-autoload t
        help-enable-symbol-autoload t)
  (setopt help-at-pt-display-when-idle t)
  (setq help-clean-buttons t)
  (setq apropos-do-all t)
  ;; (setq apropos-sort-by-scores 'verbose)
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
  (defun xy/set-variable ()
    "Like \\[set-variable] but also run :set property of user options

Once variable is read in minibuffer, C-h will run \\[describe-variable] on it.

With a prefix argument, set VARIABLE to VALUE buffer-locally.

When called interactively, the user is prompted for VARIABLE and
then VALUE.  The current value of VARIABLE will be put in the
minibuffer history so that it can be accessed with \\`M-n', which
makes it easier to edit it."
    (interactive)
    (let* ((default-var (variable-at-point))
           (ov (if (custom-variable-p default-var) "option" "variable"))
           ;; `read-variable' only show user options
           ;; (var (read-variable
           ;;       (format "Set (default: %s %s): " ov default-var)
           ;;       default-var))
           ;; @see `describe-variable'
           (var (intern (completing-read
                         (format "Set (default: %s %s): " ov default-var)
                         #'help--symbol-completion-table
                         (lambda (vv)
                           (or (get vv 'variable-documentation)
                               (and (not (keywordp vv))
                                    (boundp vv))))
                         t nil nil
                         (if (symbolp default-var) (symbol-name default-var)))))
           (ov (if (custom-variable-p var) "option" "variable"))
           (minibuffer-help-form `(describe-variable ',var))
           (scope (cond ((local-variable-p var)
			 "(buffer-local)")
			((or current-prefix-arg
			     (local-variable-if-set-p var))
			 "buffer-locally")
		        (t "globally")))
           (prompt (format "Set %s %s %s to value: " ov var scope))
           (val (read-from-minibuffer prompt nil
                                      read-expression-map t
                                      'set-variable-value-history
                                      (format "%S" (symbol-value var)))))
      (if (or current-prefix-arg
              (local-variable-if-set-p var))
          (progn
            (make-local-variable var)
            (set var val))
        (eval `(setopt ,var ,val)))))

  :bind (;; @see `help-map'
         ("C-h C-h" . nil)
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
         ;;
         ("C-h u f" . #'add-file-local-variable)
         ("C-h u d" . #'add-dir-local-variable)
         ("C-h u l" . #'xy/loaded-feature)
         ("C-h u u" . #'unload-feature)
         ("C-h u c" . #'xy/set-variable)
         ("C-h u p" . #'xy/help-show-plist)
         ;;
         ("C-h w" . nil) ; `where-is'
         ("C-h w c" . #'where-is)
         ("C-h w k" . #'describe-key-briefly)
         ;;
         ;; j u y z
         ("C-h e" . #'view-echo-area-messages) ; or click echo area
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
         ;; @tip
         ;; ("i" . #'help-goto-info)
         ;; ("I" . #'help-goto-lispref-info)
         ;; ("s" . #'help-view-source)
         ;; ("c" . #'help-customize)
         ("C" . #'xy/set-variable)
         ("P" . #'xy/help-show-plist)
         ("S-SPC" . nil) ; `scroll-down-command', available as M-v/DEL(<backspace>)
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
         ("C-h r i" . #'xy/info-eintr)
         :map Info-mode-map
         ;; ("M-n" . nil) ; `clone-buffer'
         ("S-SPC" . nil) ; `Info-scroll-down', available as DEL(<backspace>)
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

(use-package cus-edit
  :ensure nil
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
  ("C-x f" . nil)
  ("C-x f r" . recentf-open)
  ("C-x f R" . recentf-open-files)
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
  ;; `completing-read' and `read-from-minibuffer'
  ;; -- The argument HISTORY specifies which history list variable to use for saving the input and for minibuffer history commands.
  ;; -- It defaults to ‘minibuffer-history’
  ;; `savehist-minibuffer-history-variables'
  ;; `savehist-ignored-variables'
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

(use-package autorevert
  :ensure nil
  :defer 0.5
  :config
  (global-auto-revert-mode +1)
  ;; @tip "C-x x g" is `revert-buffer-quick', "s-u" is `revert-buffer'
  (setq global-auto-revert-non-file-buffers t)
  ;; Set to nil if too slow
  (setq auto-revert-remote-files t))


;;; isearch
(use-package isearch
  :ensure nil
  :config
  ;; @tip `isearch-mode-map'
  ;; [M-s M-.] -> `isearch-forward-thing-at-point' can use active region
  ;; To enable `minibuffer-local-isearch-map' which derived from `minibuffer-local-map'
  ;; -- 1.M-e -> `isearch-edit-string'
  ;; -- 2.M-p/M-n -> `isearch-ring-retreat' / `isearch-ring-advance'
  ;; -- 3.[C-s RET] -> `isearch-exit' do nonincremental search
  (setq isearch-lazy-count t)
  (setq isearch-lazy-highlight 'all-windows)
  (setq isearch-allow-scroll 'unlimited ; allow action of C-v/M-v/C-l
        isearch-allow-motion t ; change action of C-v/M-v/M-</M->
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
  (keymap-global-set "M-s p" #'xy/isearch-project)
  (keymap-set isearch-mode-map "M-s p" #'xy/isearch-project))

;; (use-package smartscan
;;   :bind ( :map smartscan-map
;;           ("M-n" . smartscan-symbol-go-forward)
;;           ("M-p" . smartscan-symbol-go-backward))
;;   :config
;;   (global-smartscan-mode +1))


;;; minibuffer
(use-package minibuffer
  :ensure nil
  ;; @see `minibuffer-local-map' or (info "(emacs) Minibuffer History")
  ;; @see `minibuffer-local-completion-map'
  :config
  ;; completion
  (setq completions-detailed t)
  (setq completion-styles '(basic initials substring partial-completion flex)) ; @see `completion-styles-alist' for available style
  (setq completion-category-overrides ; @see `completion-category-defaults' for available category
        '((file (styles basic partial-completion)))) ; partial-completion enable open multiple files with `find-file' using wildcards
  (setq completion-ignore-case t
        read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t)
  ;; M-x only show commands which are applicable to major mode and active minor modes
  (setq read-extended-command-predicate #'command-completion-default-include-p)
  ;; (setq completion-cycle-threshold nil)

  ;; completion buffer
  ;; -- `completion-list-mode-map', which derived from `special-mode-map'
  ;; -- `completion-auto-help' demo for basic style
  ;; 1. t
  ;; "buf" TAB|TAB       |"f" TAB     |TAB            |"t" TAB
  ;; buffer-  |buffer-(*)|buffer-face-|buffer-face-(*)|buffer-face-toggle
  ;; 2. always
  ;; "buf" TAB |"f" TAB        |"t" TAB
  ;; buffer-(*)|buffer-face-(*)|buffer-face-toggle
  ;; 3. visible
  ;; "buf" TAB|TAB       |"f" TAB        |"t" TAB
  ;; buffer-  |buffer-(*)|buffer-face-(*)|buffer-face-toggle
  (setq completion-auto-help 'always
        completion-auto-select 'second-tab
        completion-no-auto-exit t
        completions-format 'one-column
        completions-sort 'historical
        completions-group t
        completions-max-height 20)

  ;; minibuffer
  ;; Allow nested minibuffers.
  (setq enable-recursive-minibuffers t)
  (add-hook 'emacs-startup-hook #'minibuffer-depth-indicate-mode)
  ;; Keep the cursor out of the read-only portions of the minibuffer.
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; minibuffer UX
  (setq use-short-answers t)
  ;; Disable GUIs because they are inconsistent across systems, desktop environments, and themes, and they don't match the look of Emacs.
  (setq use-file-dialog nil)
  (setq use-dialog-box nil)
  (setq resize-mini-windows 'grow-only))

(use-package nerd-icons-completion
  :defer 0.2
  :config
  (nerd-icons-completion-mode +1)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; VERTical Interactive COmpletion
;; minibuffer completion with vertical UI
(use-package vertico
  :defer 0.2
  :bind ( :map vertico-map ; derived from `minibuffer-local-map'
          ;; @tip
          ;; M-w -> `vertico-save' Save current candidate to kill ring.
          ;; M-RET -> `vertico-exit-input', reserve for `embark-export'
          ;; -- Other ways for exiting with input when create a new buffer/file
          ;; -- 1.moving the point to the prompt.
          ;; -- 2.C-u RET
          ("M-RET" . nil)
          ("S-<return>" . vertico-exit-input)
          ("C-j" . vertico-next-group) ; as M-} / M-{
          ("C-k" . vertico-previous-group))
  :config
  (setq vertico-count 15)
  (setq vertico-resize nil)
  (setq vertico-cycle t)
  (vertico-mode +1)
  (vertico-mouse-mode +1)
  (vertico-indexed-mode +1)
  (keymap-set vertico-map "M-q" #'vertico-quick-insert)
  (keymap-set vertico-map "C-q" #'vertico-quick-exit)

  ;; Repeat Vertico sessions
  (keymap-global-set "M-z" #'vertico-repeat)
  (keymap-set vertico-map "M-x" #'vertico-repeat-select)
  (keymap-set vertico-map "M-P" #'vertico-repeat-previous)
  (keymap-set vertico-map "M-N" #'vertico-repeat-next)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)

  ;; Suspend the current Vertico session
  (keymap-global-set "M-Z" #'vertico-suspend)

  ;; Ido-like directory navigation
  (keymap-set vertico-map "RET" #'vertico-directory-enter)
  (keymap-set vertico-map "DEL" #'vertico-directory-delete-char)
  ;; Deletion without kill in most cases, use C-<backspace> to kill
  (keymap-set vertico-map "M-DEL" #'vertico-directory-delete-word)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

  ;; Toggling between the different display modes
  ;;   M-B -> `vertico-multiform-buffer'
  ;;   M-F -> `vertico-multiform-flat'
  ;;   M-G -> `vertico-multiform-grid'
  ;;   M-R -> `vertico-multiform-reverse'
  ;;   M-U -> `vertico-multiform-unobtrusive'
  ;;   M-V -> `vertico-multiform-vertical'
  (setq vertico-multiform-commands
        '((imenu buffer (vertico-buffer-display-action . (display-buffer-same-window)))
          (execute-extended-command-for-buffer (:not indexed mouse))))
  (setq vertico-multiform-categories ; categories at `marginalia-annotator-registry'
        '((file buffer)
          (project-file buffer)
          (buffer buffer)
          (command (:not indexed))))
  (vertico-multiform-mode +1))

;; Consult provides search and navigation commands based on `completing-read'
(use-package consult
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c s f" . consult-fd)
         ("C-c s d" . consult-find)
         ("C-c s c" . consult-locate)
         ("C-c s g" . consult-ripgrep)
         ;; ("C-c s g" . consult-grep)
         ("C-c s G" . consult-git-grep)
         ;;
         ("C-c s h" . consult-history)
         ("C-c s k" . consult-kmacro)
         ("C-c s m" . consult-man)
         ("C-c s i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x f b" . consult-bookmark)
         ("C-x f f" . consult-recent-file)
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("C-M-y" . #'yank-pop) ; show the view of kill history
         ("M-y" . consult-yank-pop) ; show the view of kill ring
         ;; [M-Y] alone is same as `consult-yank-pop'
         ;; [C-y M-Y] yank without moving the last-yank pointer
         ("M-Y" . consult-yank-replace)
         ;;
         ("C-h C-m" . consult-mode-command) ; as `execute-extended-command-for-buffer'
         ("C-h C-n" . consult-minor-mode-menu)
         ("C-c y c" . consult-theme)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  (setq consult-narrow-key "<")

  (consult-info-define "emacs" "efaq" "elisp" "eintr" "cl")
  (consult-info-define 'all "widget" "ediff" "eglot" "flymake" "eshell" "tramp" "org" "gnus" "calc" "eww")
  ;; "magit" "dash"
  (consult-info-define 'completion
                       "vertico" "consult" "marginalia" "orderless" "embark" "corfu" "cape"))

;; Emacs completion style that matches multiple regexps in any order
;; -- `orderless-matching-styles'
;; -- `orderless-affix-dispatch-alist'
(use-package orderless
  :after vertico :demand t
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)

  ;; @corfu/readme
  (defun xy/-orderless-fast-dispatch (word index total)
    (and (= index 0) (= total 1) (length< word 4)
         (cons 'orderless-literal-prefix word)))
  (orderless-define-completion-style xy/orderless-fast
    (orderless-style-dispatchers '(xy/-orderless-fast-dispatch))
    (orderless-matching-styles '(orderless-literal orderless-regexp))))

;; Enriches the completion display with annotation
;; 1.provide classifiers for embark
;; -- `marginalia-classifiers'
;; 2.provide annotators for minibuffer
;; -- `marginalia-annotator-registry'
;; -- `marginalia--symbol-class'
(use-package marginalia
  :after vertico :demand t
  :bind ( :map minibuffer-local-map
          ("M-A" . marginalia-cycle)
          ;; To make the binding available in the *Completions* buffer
          :map completion-list-mode-map
          ("M-A" . marginalia-cycle))
  :config
  (marginalia-mode +1)

  ;; Define a new annotator for face category
  (defun xy/face-annotator (cand)
    (when-let (sym (intern-soft cand))
      (concat (propertize " " 'display '(space :align-to center))
              (propertize "The quick brown fox jumps over the lazy dog" 'face sym))))

  (add-to-list 'marginalia-annotator-registry
               '(face marginalia-annotate-face xy/face-annotator builtin none)))

;; Emacs Mini-Buffer Actions Rooted in Keymaps
;; a keyboard-based version of a right-click contextual menu
;; to perform context-sensitive actions on target(s) at point
;; which works both in minibuffer and normal buffers
(use-package embark
  :bind
  ;; @see `embark-keymap-alist'
  ;; `embark-act' acts as a right-click context menu at point and `embark-dwim' acts like left-click
  (;; ("M-SPC" . embark-act)
   ;; ("M-S-SPC" . embark-act-all)
   ("C-." . embark-act)
   ("C->" . embark-act-all)
   ("M-." . embark-dwim) ; acts like `xref-find-definitions' on the symbol at point.
   ;;
   ("S-SPC" . embark-select)
   ;;
   ("M-RET" . embark-export) ; @see `embark-exporters-alist', falls back to the generic `embark-collect'
   ("M-S-<return>" . embark-collect) ; 1.embark keymap; 2.follow target in original buf.
   ("C-M-<return>" . embark-live)
   ;;
   ("C-h TAB" . embark-bindings)  ; as `execute-extended-command-for-buffer'
   :map minibuffer-local-map
   ("M-," . embark-become)) ; @see `embark-become-keymaps'
  :init
  ;; Used for backup of `which-key-C-h-dispatch', saved as `which-key--prefix-help-cmd-backup'
  ;; (setq prefix-help-command #'embark-prefix-help-command)
  ;; Show the Embark target at point via Eldoc
  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  ;; @consult/wiki
  ;; -- Manual preview for non-Consult commands using Embark
  (define-key minibuffer-local-map (kbd "M-.") #'xy/embark-preview)
  (defun xy/embark-preview ()
    "Previews candidate in minibuffer, unless it's a consult command"
    (interactive)
    (unless (bound-and-true-p consult--preview-function)
      (save-selected-window
        (let ((embark-quit-after-action nil))
          (embark-dwim))))))

;; 1. `embark-export' exporters:
;; -- `occur-mode' for `consult-line' `consult-outline' `consult-mark'
;; -- `grep-mode' for `consult-grep' `consult-git-grep' `consult-ripgrep'
;; 2. `embark-live' collectors: add to `embark-candidate-collectors' for `outline-minor-mode' and `imenu'
(use-package embark-consult
  :after embark :demand t ;; load consult after embark to provide `consult-imenu' for `embark-export'
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package embark-sidebar
  :after embark :demand t
  :vc ( :url "https://github.com/kn66/embark-sidebar.el"
        :rev :newest)
  :bind ("C-x d e" . embark-sidebar-toggle)
  :config
  (embark-sidebar-mode +1))


;;; completion
;; (use-package completion-preview
;;   :ensure nil
;;   :defer 0.2
;;   :bind ( :map completion-preview-active-mode-map
;;           ("M-n" . #'completion-preview-next-candidate)
;;           ("M-p" . #'completion-preview-prev-candidate))
;;   :config
;;   (global-completion-preview-mode +1))

;; COmpletion in Region FUnction
;; in-buffer completion with a child frame popup by setting `completion-in-region-function'
;; Command `completion-at-point' -> Function `completion-in-region' -> Variable `completion-in-region-function'
(use-package corfu
  :defer 0.2
  :bind ( :map corfu-map
          ;; ("RET" . nil) ; Free RET for newline etc.
          ;; ("TAB" . corfu-next) ; Use TAB for cycling
          ;; ("S-TAB" . corfu-previous)
          ("M-q" . corfu-quick-complete))
  :config
  ;; (setq corfu-preview-current nil)
  (setq corfu-auto t)
  (setq corfu-auto-prefix 2)
  (setq corfu-cycle t)
  ;; Recommended enable globally since many modes provide Capfs and Dabbrev can be used globally (M-/).
  (global-corfu-mode +1)

  ;; Sort completions by history
  (corfu-history-mode +1)

  ;; Show documentation in popup.
  ;; @tip M-g:`corfu-info-location', M-h:`corfu-info-documentation'
  (corfu-popupinfo-mode +1)
  (setq corfu-popupinfo-delay '(1 . 0.5))
  ;; (corfu-indexed-mode +1)

  ;; Buffer-local/Corfu-only completion styles
  (add-hook 'corfu-mode-hook
            (defun xy/-in-buffer-completion-style ()
              (setq-local completion-styles '(xy/orderless-fast basic)
                          completion-category-overrides nil
                          completion-category-defaults nil))))

;; Completion At Point Extensions
;; Capfs(`completion-at-point-functions') are completion backends used by `completion-at-point' command
(use-package cape
  :bind ("C-c p" . cape-prefix-map)
  :init
  (setq text-mode-ispell-word-completion #'cape-dict)
  ;; Add more completion backends. The latters take precedence over formers.
  (add-hook 'completion-at-point-functions #'cape-dict)
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  )

(use-package nerd-icons-corfu
  :after corfu :demand t
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))


;;; abbrev
;; @todo
;; https://www.emacswiki.org/emacs/AbbrevMode
;; https://www.emacswiki.org/emacs/HippieExpand
;; @see (info "(emacs) Dynamic Abbrevs")
(use-package dabbrev
  :ensure nil
  ;; or (keymap-global-set "M-/" #'hippie-expand)
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (setq dabbrev-upcase-means-case-search t)
  (setq dabbrev-ignored-buffer-modes
        '(archive-mode image-mode docview-mode tags-table-mode pdf-view-mode))

  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'authinfo-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))


;;; keymap
(use-package repeat
  :ensure nil
  :defer 0.3
  :config
  (repeat-mode +1)
  ;; (setq repeat-exit-key "RET")
  (setq repeat-exit-key "q")

  (defvar-keymap xy/undo-repeat-map
    :keymap undo-repeat-map
    "U" #'undo-only
    "r" #'undo-redo) ; useful to shorten "undo records" by balancing out previous `undo'

  (defvar-keymap xy/page-navigation-repeat-map
    :keymap page-navigation-repeat-map
    :repeat t
    "{" #'backward-paragrafph
    "}" #'forward-paragraph)

  (defvar-keymap xy/navi-repeat-map
    :repeat ( :enter (forward-word backward-word) ;; forward-page backward-page
              :exit (transpose-sexps kill-sexp backward-kill-sexp kill-backward-up-list raise-sexp mark-sexp)
              :hints
              ((kill-backward-up-list . "kill-backward-up-list")
               (up-list . "up-list")))
    ;; sexp
    "f" #'forward-sexp
    "b" #'backward-sexp
    ;; list
    "n" #'forward-list
    "p" #'backward-list
    "d" #'down-list
    "u" #'backward-up-list
    "N" #'up-list
    ;; edit
    "t" #'transpose-sexps
    "k" #'kill-sexp
    "DEL" #'backward-kill-sexp
    "U" #'kill-backward-up-list
    "r" #'raise-sexp
    "SPC" #'mark-sexp
    ;; defun
    "a" #'beginning-of-defun
    "e" #'end-of-defun
    "h" #'mark-defun
    "x" #'eval-defun))

(use-package which-key
  :ensure nil
  :defer 0.3
  :config
  (which-key-mode +1)
  (setq which-key-lighter nil)
  (setq which-key-idle-delay .5
        which-key-idle-secondary-delay .0)
  ;; @tip Press h/C-h after which-key's paging will run `which-key-show-standard-help', which run `describe-prefix-bindings'
  (setq which-key-use-C-h-commands t)
  (setq which-key-sort-order 'which-key-key-order-alpha
        which-key-sort-uppercase-first nil))

;; (use-package ffap
;;   :ensure nil
;;   :defer 1
;;   :bind
;;   ("C-x M-f" . #'ffap-menu)
;;   :config
;;   ;; @tip
;;   ;; (keymap-global-set "S-<mouse-3>" 'ffap-at-mouse)
;;   ;; (keymap-global-set "C-S-<mouse-3>" 'ffap-menu)
;;   (ffap-bindings))

(use-package keyfreq
  :defer 0.3
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
(use-package midnight
  :ensure nil
  :defer 2
  :config
  (midnight-mode +1))

;; Useful to kill multiple buffers
(use-package ibuffer
  :ensure nil
  :bind
  ("C-x C-b" . #'ibuffer-jump) ; @prefix Display ibuffer in other window
  ("C-x 4 C-b" . #'ibuffer-other-window) ; @prefix Show only file-visiting buffers
  :hook (ibuffer-mode . ibuffer-auto-mode)
  :config
  (defvar xy/boring-buffers '("\\` "
                              ;; "\\`\\*Echo Area"
                              ;; "\\`\\*Minibuf"
                              ;; "\\`\\*Completions"
                              "\\`\\*Flymake log"
                              "\\`\\*Semantic SymRef"
                              ;; "\\`\\*Backtrace"
                              "\\`\\*tramp"
                              "\\`\\*EGLOT"
                              ;; And some hidden buffers can be visited by ...
                              ;; "\\`\\*scratch"        ; "C-z s s"
                              ;; "\\`\\*Messages"       ; "C-h e"
                              "\\`\\*Bookmark List"  ; "C-x r l"
                              )
    "List of buffer names of buffers to hide on several occasions.")

  ;; (setq ibuffer-use-other-window t)
  (setq ibuffer-never-show-predicates xy/boring-buffers))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode))


;;; window
(use-package winner
  :ensure nil
  :defer 0.5
  :bind
  ("C-x 4 u" . #'winner-undo)
  ("C-x 4 r" . #'winner-redo)
  :config
  (winner-mode +1))

(use-package windmove
  :ensure nil
  :defer 0.5
  :config
  ;; @tip shift and ctrl-shift is used by Org-Mode
  (windmove-default-keybindings 'ctrl)
  (windmove-swap-states-default-keybindings '(ctrl shift))
  (windmove-display-default-keybindings '(ctrl meta))
  (windmove-delete-default-keybindings))

(use-package ace-window
  :bind ;; ([remap other-window] . ace-window)
  ("M-o" . ace-window)
  :config
  ;; (custom-set-faces
  ;;  '(aw-leading-char-face
  ;;    ((t (:inherit ace-jump-face-foreground :height 2.0)))))
  (set-face-attribute 'aw-leading-char-face nil :height 2.0))

;; @see https://emacs.stackexchange.com/questions/5371/how-to-change-emacs-windows-from-vertical-split-to-horizontal-split
(defun xy/window-split-toggle ()
  "Toggle between horizontal and vertical split with two windows."
  (interactive)
  (if (> (length (window-list)) 2)
      (error "Can't toggle with more than 2 windows!")
    (let ((func (if (window-full-height-p)
                    #'split-window-vertically
                  #'split-window-horizontally)))
      (delete-other-windows)
      (funcall func)
      (save-selected-window
        (other-window 1)
        (switch-to-buffer (other-buffer))))))
(keymap-global-set "C-x 4 t" #'xy/window-split-toggle)

;; Native frame transposition coming to Emacs 31
;; -- https://p.bauherren.ovh/blog/tech/new_window_cmds
;; -- https://news.ycombinator.com/item?id=43619437
;; (use-package window-x
;;   :bind ("C-x 4 t" . #'rotate-windows))


;;; dired
(use-package dired
  :ensure nil
  :bind (("C-x d" . nil)
         ("C-x d d" . dired)
         ("C-x d j" . dired-jump)
         :map dired-mode-map
         ("v" . dired-view-file))
  :hook
  (dired-mode . dired-hide-details-mode)
  (dired-mode . dired-omit-mode)
  :config
  ;; @tip see `dired-mode-map' for summary and usage
  ;; Flags for `insert-directory-program'. Or: -alh, --group-directories-first
  ;; (setq dired-listing-switches "-laGgh1v --group-directories-first --time-style=long-iso")
  (setq dired-listing-switches "-lhFA -v")
  (setq dired-kill-when-opening-new-dired-buffer t)
  ;; Propose a target for intelligent moving or copying.
  ;; e.g. use next windows as target for file copy, rename etc
  (setq dired-dwim-target t)
  (setq dired-recursive-copies 'always)
  (setq dired-create-destination-dirs 'ask)
  (setq dired-vc-rename-file t)
  (setq dired-omit-verbose nil)
  ;; (setq dired-omit-files (concat "\\`[.]\\'"))
  (setq ls-lisp-dirs-first t)
  (setq image-dired-thumb-size 150
        image-dired-thumb-margin 1
        image-dired-thumb-relief 0
        ;; Store thumbnails in the system-wide thumbnail location
        ;; e.g. ~/.local/cache/thumbnails to make them reusable by other programs
        image-dired-thumbnail-storage 'standard-large))

(use-package nerd-icons-dired
  :hook (dired-mode))

(use-package trashed
  :bind ("C-x d t" . trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

(use-package dired-subtree
  :after dired
  :bind ( :map dired-mode-map
          ("<tab>" . dired-subtree-toggle)
          ("<backtab>" . dired-subtree-remove)
          ("C-<tab>" . dired-subtree-cycle)
          ;; ("TAB" . dired-subtree-toggle)
          ;; ("S-TAB" . dired-subtree-remove)
          ;; ("C-TAB" . dired-subtree-cycle)
          ("[" . dired-subtree-up)
          )
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package dired-sidebar
  :bind ("C-x d s" . dired-sidebar-toggle-sidebar)
  :config
  (setq dired-sidebar-should-follow-file t)
  (add-to-list 'dired-sidebar-special-refresh-commands 'dired-sidebar-mouse-subtree-cycle-or-find-file))

(use-package neotree
  :bind ("C-x d n" . neotree)
  :config
  (setq neo-theme (if (display-graphic-p) 'nerd-icons 'arrow)))

(use-package treemacs
  :bind ( :map global-map
          ("C-x d m" . treemacs)))

(use-package projtree
  :vc ( :url "https://github.com/petergardfjall/emacs-projtree"
        :rev :newest)
  :bind ("C-x d p" . projtree-mode))


;;; transient
(use-package transient-showcase
  :vc ( :url "https://github.com/positron-solutions/transient-showcase"
        :rev :newest)
  :bind ("C-c t s" . tsc-showcase))

(use-package disproject
  :bind ( :map ctl-x-map
          ;; Replace `project-prefix-map'
          ;; ("p" . disproject-dispatch)
          ("P" . disproject-dispatch)))


;;; ui
(use-package tab-line
  :ensure nil
  :hook
  (emacs-startup . global-tab-line-mode)
  :config
  ;; (add-to-list 'tab-line-format '(:eval (tab-line-format)))
  (defun xy/tab-line-close-tab (buffer)
    "Close the tab associated with BUFFER, and `delete-window' if only one tab"
    (cond
     ((length= (tab-line-tabs-window-buffers) 1)
      (delete-window))
     ((eq buffer (current-buffer))
      (bury-buffer))
     (t
      (set-window-prev-buffers nil
                               (assq-delete-all buffer (window-prev-buffers)))
      (set-window-next-buffers nil
                               (delq buffer (window-next-buffers))))))
  (setq tab-line-close-tab-function #'xy/tab-line-close-tab))

(use-package tab-bar
  :ensure nil
  :hook
  (emacs-startup . tab-bar-mode)
  (emacs-startup . tab-bar-history-mode)
  ;; :bind ( :map tab-bar-mode-map
  ;;         ("C-<tab>" . nil)
  ;;         ([(control shift tab)] . nil))
  :config
  (setopt tab-bar-show 1)
  (setopt tab-bar-tab-hints t)
  (setopt tab-bar-select-tab-modifiers '(super))
  (setopt tab-bar-tab-name-function #'tab-bar-tab-name-current-with-count))

;; (use-package window-tool-bar
;;   :ensure nil
;;   :hook
;;   ;; (emacs-startup . global-window-tool-bar-mode)
;;   (special-mode . window-tool-bar-mode))

(use-package hl-line
  :ensure nil
  :hook
  (prog-mode . hl-line-mode)
  (text-mode . hl-line-mode)
  (help-mode . hl-line-mode)
  (Info-mode . hl-line-mode)
  (dired-mode . hl-line-mode)
  (package-menu-mode . hl-line-mode))

(use-package hide-mode-line
  :hook
  (inferior-python-mode) ; `run-python'
  (completion-list-mode))

(use-package breadcrumb
  :defer 1
  :config
  (breadcrumb-mode +1)
  (setq breadcrumb-imenu-crumb-separator " "))

(use-package vundo
  :defer 0.8
  :bind (("C-x C-u" . vundo))
  :config
  (vundo-popup-mode +1)
  (setq vundo-glyph-alist vundo-unicode-symbols))


;;; util
(use-package gcmh
  :hook (after-init)
  :config
  (setq gcmh-high-cons-threshold (* 128 1024 1024)))

;; https://www.emacswiki.org/emacs/VisibleMark
(use-package visible-mark
  :defer 0.5
  :config
  (global-visible-mark-mode +1)
  (setq visible-mark-max 2)
  (setq visible-mark-faces `(visible-mark-face1 visible-mark-face2)))

;; https://www.emacswiki.org/emacs/AutoMark
(use-package auto-mark
  :ensure nil ; site-lisp
  :defer 0.5
  :config
  (setq auto-mark-command-class-alist
        '((anything . anything)
          (goto-line . jump)
          (indent-for-tab-command . ignore)
          (undo . ignore)))
  (setq auto-mark-command-classifiers
        (list (lambda (command)
                (if (and (eq command 'self-insert-command)
                         (eq last-command-event ? ))
                    'ignore))))
  (global-auto-mark-mode +1))


;;; tool
(use-package view
  :ensure nil
  :bind
  (("C-x x v" . #'view-buffer)
   ("C-x 4 v" . #'view-buffer-other-window)
   ("C-x 5 v" . #'view-buffer-other-frame)
   ("C-x x V" . #'view-file)
   :map view-mode-map
   ("q" . #'switch-to-prev-buffer))
  :config
  (keymap-set ctl-x-4-map "V" #'view-file-other-window)
  (keymap-set ctl-x-5-map "V" #'view-file-other-frame))

(use-package outline
  :ensure nil
  ;; :hook
  ;; (emacs-lisp-mode . outline-minor-mode)
  :config
  ;; @tip Click left margin with mouse-1/S-mouse-1. see `outline-minor-mode-cycle-map'
  ;; @tip RET at beginning of headers line trigger `outline-cycle'. And for S-RET:
  (keymap-set outline-overlay-button-map "S-<return>" #'outline-cycle-buffer)
  ;; For TAB/S-TAB
  ;; (setq outline-minor-mode-cycle t)
  ;; C-q `outline-hide-sublevels': Obly top n (default 1, can prefix) headers visible
  ;; C-t `outline-hide-body': Hide all body lines in buffer, leaving all headings visible.
  (setopt outline-minor-mode-prefix (kbd "C-c v")) ; v for view
  ;;; UI
  ;; (setq outline-minor-mode-highlight 'append)
  (setq outline-minor-mode-use-buttons 'in-margins))

(use-package wakatime-mode
  :defer 1
  :config
  (global-wakatime-mode))


;;; motion
(use-package avy
  :bind (("M-s ;" . avy-resume)
         ("M-s j" . avy-goto-char)
         ("M-s M-j" . avy-goto-word-1)
         ("M-s n" . avy-goto-char-2)
         ("M-s M-n" . avy-goto-line)
         ("M-s /" . avy-goto-char-timer)
         :map isearch-mode-map
         ("M-s j" . avy-isearch)))


;;; prog
(use-package imenu-list
  :bind
  ("C-c l" . imenu-list-smart-toggle)
  :config
  ;; (setq imenu-list-position 'left)
  (setq imenu-list-focus-after-activation t)
  (setq imenu-list-auto-resize t))

;; (info "(emacs) Programming Language Doc")
(use-package eldoc
  :ensure nil
  :init
  ;; (global-eldoc-mode +1) ;; default
  ;; :hook
  ;; (prog-mode . eldoc-mode)
  :bind ("C-h ." . #'eldoc-doc-buffer)
  :config
  ;; (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  ;; (setq eldoc-echo-area-display-truncation-message nil)
  (setq eldoc-minor-mode-string nil))

;; @tip
;; M-. / M-, -> `xref-find-definitions' / `xref-go-back'
;; C-M-. / C-M-, -> `xref-find-apropos' / `xref-go-forward'
;; M-? -> `xref-find-references'
(use-package xref
  :ensure nil
  :config
  ;; Use completion system instead of popup window.
  (setq xref-show-definitions-function 'xref-show-definitions-completing-read
        xref-show-xrefs-function 'xref-show-definitions-completing-read)
  (setq xref-history-storage 'xref-window-local-history))

(use-package flymake
  :ensure nil
  ;; :hook (emacs-lisp-mode)
  :bind ( :map flymake-mode-map
          ("M-g n" . flymake-goto-next-error)
          ("M-g p" . flymake-goto-prev-error)
          ("M-g e" . flymake-show-buffer-diagnostics)
          ("M-g E" . flymake-show-project-diagnostics))
  :config
  ;; (setq flymake-show-diagnostics-at-end-of-line 'short)
  (remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake))

(use-package dumb-jump
  :defer 1
  :config
  ;; @see `dumb-jump-find-rules'
  (setq dumb-jump-prefer-searcher 'rg)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))


;;; vc
(use-package vc
  :ensure nil
  :config
  (setq vc-git-diff-switches '("--histogram")))


;;; diff
(use-package diff
  :ensure nil
  :config
  (setq diff-refine 'font-lock)
  (setq diff-font-lock-prettify nil)
  (setq diff-font-lock-syntax t))

(use-package ediff
  :ensure nil
  :config
  ;; use a single frame and split windows horizontally
  (setq ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-split-window-function #'split-window-horizontally
        ediff-merge-split-window-function #'split-window-horizontally))

;; Highlight uncommitted changes using VC
(use-package diff-hl
  :defer 0.5
  :hook (dired-mode . diff-hl-dired-mode)
  :bind ( :map diff-hl-command-map
          ("." . diff-hl-amend-mode)
          ("-" . diff-hl-set-reference-rev)
          ("_" . diff-hl-reset-reference-rev)
          ("RET" . diff-hl-show-hunk)
          ("SPC" . diff-hl-mark-hunk)
          ("n" . diff-hl-next-hunk)
          ("p" . diff-hl-previous-hunk)
          ("M-s" . #'xy/toggle-diff-hl-staged))
  :custom-face
  ;; (diff-hl-change ((t (:inherit custom-changed :foreground unspecified :background unspecified))))
  ;; (diff-hl-insert ((t (:inherit diff-added :background unspecified))))
  ;; (diff-hl-delete ((t (:inherit diff-removed :background unspecified))))
  :config
  (global-diff-hl-mode +1)
  ;; Makes fringe and margin react to mouse clicks
  ;; (global-diff-hl-show-hunk-mouse-mode +1)
  ;; Diffing on-the-fly (i.e. without saving the buffer first)
  (diff-hl-flydiff-mode +1)

  (setq diff-hl-show-staged-changes nil)
  (defun xy/toggle-diff-hl-staged ()
    (interactive)
      (if diff-hl-show-staged-changes
          (setq diff-hl-show-staged-changes nil)
        (setq diff-hl-show-staged-changes t))
      (diff-hl-magit-post-refresh))

  (unless (display-graphic-p)
    ;; Fall back to the display margin since the fringe is unavailable in tty
    (diff-hl-margin-mode +1)
    ;; Avoid restoring `diff-hl-margin-mode'
    (with-eval-after-load 'desktop
      (add-to-list 'desktop-minor-mode-table '(diff-hl-margin-mode nil))))

  (defun xy/diff-hl-fringe-bmp-function (_type _pos)
    (define-fringe-bitmap 'my-diff-hl-bmp
      (vector (if xy/linux-p #b11111100 #b11100000)) 1 8 '(center t)))
  ;; (setq diff-hl-fringe-bmp-function #'xy/diff-hl-fringe-bmp-function)

  ;; Integration with magit
  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))


;;; git
(use-package magit
  :bind (;; ("C-x g"   . magit-status)
         ;; ("C-x C-g" . magit-dispatch)
         ;; ("C-x M-g" . magit-file-dispatch)
         ("C-x g g" . magit-status)
         ("C-x g a" . magit-log-all)
         ("C-x g b" . magit-branch-checkout)
         ("C-x g c" . magit-log-current)
         ("C-x g C" . magit-log-buffer-file)
         ("C-x g '" . magit-blame-addition)
         ("C-x g d" . magit-diff-dwim)
         ("C-x g D" . magit-diff-buffer-file)
         ;;
         ("C-x m" . nil)
         ("C-x m m" . magit-dispatch)
         ("C-x m f" . magit-file-dispatch)
         ("C-x p m" . magit-project-status)
         ("C-x m l" . magit-log)
         ("C-x m b" . magit-branch)
         ("C-x m '" . magit-blame)
         ("C-x m d" . magit-diff)
         ("C-x m e" . magit-ediff-dwim)
         ("C-x m E" . magit-ediff)
         ("C-x m z" . magit-stash)
         ("C-x m p" . magit-pull)
         ("C-x m P" . magit-push)
         ("C-x m r" . magit-rebase)
         ("C-x m M" . magit-merge)
         :map magit-section-mode-map
         ("]" . magit-section-forward-sibling)
         ("[" . magit-section-backward-sibling))
  :config
  (setq magit-diff-refine-hunk t)
  ;; Order for branch checkout: objectsize, authordate, committerdate, creatordate, taggerdate
  (setq magit-list-refs-sortby "-creatordate")
  (add-to-list 'savehist-additional-variables 'magit-revision-history)
  (setq magit-repository-directories '(("~/notes" . 0)
                                       ("~/dotter" . 0)
                                       ("~/work" . 1))))

;; Adapted from Tassilo Horn's blog post:
;; https://www.tsdh.org/posts/2022-07-20-using-eldoc-with-magit-async.html
(use-package eldoc-diffstat
  :defer 1
  :config
  (global-eldoc-diffstat-mode +1)
  (eldoc-add-command
   'magit-next-line 'magit-previous-line
   'magit-section-forward 'magit-section-backward
   'magit-section-forward-sibling 'magit-section-backward-sibling))

;; Make magit's diff have syntax highlight, like `vc-diff'
(use-package magit-delta
  ;; :if (executable-find "delta")
  :hook (magit-mode . magit-delta-mode)
  :config
  ;; @see https://github.com/dandavison/magit-delta/issues/13#issuecomment-690534938
  ;; --line-numbers/--side-by-side cannot be used with magit-delta since it creates invalid patches
  ;; (setq magit-delta-delta-args '("--max-line-distance" "0.6" "--true-color" "always" "--color-only"))
  ;; (add-to-list 'magit-delta-delta-args "--diff-highlight")
  ;; (add-to-list 'magit-delta-delta-args "--diff-so-fancy")
  (add-to-list 'magit-delta-delta-args "--no-gitconfig"))


;;; org
(use-package org
  :ensure nil
  :bind
  ("C-c o d" . #'xy/open-org-dir)
  ("C-c o o" . #'xy/open-org-notes)
  ("C-c o a" . #'org-agenda)
  ("C-c o c" . #'org-capture)
  ("C-c o l" . #'org-store-link)
  ("C-c o ;" . #'org-toggle-link-display)
  ("C-c o p" . #'org-publish)
  :init
  (defun xy/open-org-dir ()
    (interactive)
    (dired org-directory))
  (defun xy/open-org-notes ()
    "Visit the Org notes file."
    (interactive)
    (find-file org-default-notes-file))
  :config
  ;; (add-hook 'org-mode-hook #'visual-line-mode)
  (setq org-startup-folded 'content)
  (setq org-hide-leading-stars t)
  (setq org-special-ctrl-a/e t
        org-special-ctrl-k t)

  ;; Alignment of tags at the end of headlines
  (setq org-auto-align-tags t
        org-tags-column 0)
  (setq org-reverse-note-order t) ; Put newer notes on top of the file
  (setq org-directory "~/org/"
        org-default-notes-file (concat org-directory "notes.org"))
  (setq org-todo-keywords ; Set some sensible default states for todo-items
        '((sequence "TODO(t)" "PROJ(p)" "LOOP(r)" "STRT(s)" "WAIT(w)" "HOLD(h)" "IDEA(i)" "|" "DONE(d)" "KILL(k)")
          (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
          (sequence "|" "OKAY(o)" "YES(y)" "NO(n)")))
  ;; (setq org-log-done 'note)
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
    :defer 0.5
    :config
    (xterm-mouse-mode +1))

  ;; NOTE: need xclip at linux
  ;; Allow Emacs to copy to and paste from the GUI clipboard when running in a text terminal
  (when (and xy/linux-p (executable-find "xclip"))
    (use-package xclip
      :defer 0.5
      :config
      (xclip-mode +1))))


;;; shell
;; @see https://www.masteringemacs.org/article/running-shells-in-emacs-overview
(use-package comint
  :ensure nil
  :bind (("C-z e s" . shell)
         ("C-z e t" . ansi-term)
         :map comint-mode-map
         ;; @tip
         ;; "C-c C-p/C-n/C-a": Jump to the prev/next/last prompt
         ;; "C-c C-l": `comint-dynamic-list-input-ring'
         ;; "C-c C-x": `comint-get-next-from-history'
         ;; Auto subsitution: !! expands to the last command; ^a^b replaces a with b
         ("SPC" . #'comint-magic-space))
  :config
  (setq shell-command-prompt-show-cwd t)
  ;;
  (setq comint-input-ignoredups t
        comint-prompt-read-only t
        comint-scroll-to-bottom-on-input 'this
        comint-buffer-maximum-size (* 2 1024))
  (setq comint-history-isearch 'dwim)
  ;; (setq comint-input-autoexpand 'input)
  )

;; @see https://www.masteringemacs.org/article/complete-guide-mastering-eshell
(use-package eshell
  :ensure nil
  :bind
  ("C-z e e" . eshell)
  :config
  (with-eval-after-load 'em-term
    (add-to-list 'eshell-visual-subcommands '("git" "log" "diff" "show" "ls"))
    (add-to-list 'eshell-visual-options '("git" "--help" "--paginate")))
  ;; Show help for quickly filtering files or elisp lists
  (with-eval-after-load 'em-alias
    (eshell/alias "ep" #'eshell-display-predicate-help)
    (eshell/alias "ef" #'eshell-display-modifier-help))
  ;; Enable bash keys (C-r, C-s, C-w, C-u)
  ;; (require 'em-rebind)
  ;; (eshell-rebind-initialize)
  ;; Plan 9 Smart Shell: improve the write-run-revise
  ;; (require 'em-smart)
  ;; (eshell-smart-initialize)
  )

;; @see https://www.masteringemacs.org/article/pcomplete-context-sensitive-completion-emacs
;; (use-package pcomplete)


;;; theme
(use-package solar
  :ensure nil
  :defer 1
  :config
  (setq calendar-latitude 40)
  (setq calendar-longitude 116))

;; Switch themes depending on the time of the day
;; (use-package circadian
;;   :after solar :demand t
;;   :config
;;   (setq circadian-themes '((:sunrise . modus-operandi)
;;                            (:sunset  . modus-vivendi)))
;;   (circadian-setup))

(use-package solarized-theme
  :bind ("C-c y s" . solarized-toggle-theme))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (nerd-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package ef-themes
  :bind (("C-c y e l" . ef-themes-select-light)
         ("C-c y e d" . ef-themes-select-dark)
         ("C-c y e s" . ef-themes-select)
         ("C-c y e t" . ef-themes-toggle)
         ("C-c y e r" . ef-themes-rotate)
         ("C-c y e e" . ef-themes-load-random))
  :config
  ;; EF themes: `ef-themes-collection', `ef-themes-dark-themes', `ef-themes-light-themes'
  (setq ef-themes-to-toggle '(ef-summer ef-spring))
  (setq ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui t)
  (setq ef-themes-headings
      '((0 variable-pitch light 1.9)
        (1 variable-pitch light 1.8)
        (2 variable-pitch regular 1.7)
        (3 variable-pitch regular 1.6)
        (4 variable-pitch regular 1.5)
        (5 variable-pitch 1.4) ; absence of weight means `bold'
        (6 variable-pitch 1.3)
        (7 variable-pitch 1.2)
        (t variable-pitch 1.1)))
  ;; (load-theme 'ef-summer :no-confirm)
  )

;; https://protesilaos.com/codelog/2025-05-13-emacs-doric-themes/
(use-package doric-themes
  :bind (("C-c y d t" . doric-themes-toggle)
         ("C-c y d s" . doric-themes-select)
         ("C-c y d r" . doric-themes-rotate)
         ("C-c y d r" . doric-themes-load-random))
  :config
  (setq doric-themes-to-toggle '(doric-light doric-dark))
  (setq doric-themes-to-rotate doric-themes-collection)
  ;; (doric-themes-select 'doric-light)

  ;; ;; To load a random theme instead, use something like one of these:
  ;; (doric-themes-load-random)
  ;; (doric-themes-load-random 'light)
  ;; (doric-themes-load-random 'dark)

  ;; ;; For optimal results, also define your preferred font family (or use my `fontaine' package):
  ;;
  ;; (set-face-attribute 'default nil :family "Aporetic Sans Mono" :height 160)
  ;; (set-face-attribute 'variable-pitch nil :family "Aporetic Sans" :height 1.0)
  ;; (set-face-attribute 'fixed-pitch nil :family "Aporetic Sans Mono" :height 1.0)
  )


;;; treesit
;; @see doc of `treesit-major-mode-setup'
(use-package treesit
  :ensure nil
  :bind (("C-h o i" . treesit-inspect-mode)
         ("C-h o e" . treesit-explore-mode))
  :config
  ;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
  (setq treesit-font-lock-level 4))

;; @see https://magnus.therning.org/2023-11-16-using-the-golang-mode-shipped-with-emacs.html
(use-package go-ts-mode
  :ensure nil
  ;; Remapping major mode: (add-to-list 'major-mode-remap-alist '(XXX-mode . XXX-ts-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
  ;; (add-to-list 'auto-mode-alist '("/go\\.mod\\'" . go-mod-ts-mode))
  ;; :mode (("\\.go\\'" . go-ts-mode)
  ;;        ("/go\\.mod\\'" . go-mod-ts-mode))
  :config
  ;; (dolist (lang '(go gomod)) (treesit-install-language-grammar lang))
  (add-to-list 'treesit-language-source-alist '(go "https://github.com/tree-sitter/tree-sitter-go"))
  (add-to-list 'treesit-language-source-alist '(gomod "https://github.com/camdencheek/tree-sitter-go-mod")))


;;; lisp
(use-package lisp-mode
  :ensure nil
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
         ;; :map lisp-mode-shared-map
         ("C-c e c" . #'check-parens)))

(use-package macrostep
  :bind (;; :map lisp-mode-shared-map
          ("C-c e m" . macrostep-expand)))

;; (use-package paredit
;;   ;; `lisp-data-mode' is the parent of `emacs-lisp-mode' and `lisp-mode'
;;   :hook lisp-data-mode
;;   :bind (:map paredit-mode-map
;;               ;; ("M-s" . nil) ;; `paredit-splice-sexp'
;;               ;; ("M-r" . nil) ;; `paredit-raise-sexp'
;;               ;; ("M-;" . nil) ;; `paredit-comment-dwim'
;;               ;; ("C-j" . nil) ;; `paredit-C-j'
;;               ;; ("M-)" . paredit-splice-sexp)
;;               ;; ("M-K" . paredit-raise-sexp)
;;               ;; ("ESC M-;" . paredit-comment-dwim)
;;               ("M-U" . paredit-backward-slurp-sexp)
;;               ("M-D" . paredit-backward-barf-sexp)
;;               ("M-N" . paredit-forward-slurp-sexp)
;;               ("M-P" . paredit-forward-barf-sexp))
;;   :config
;;   ;; (electric-indent-mode -1)
;;   ;; ElDoc can safely print docstring after these commands
;;   (eldoc-add-command
;;    'paredit-backward-delete
;;    'paredit-close-round
;;    'paredit-close-square
;;    'paredit-close-curly))


;;; lang
(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :config
  ;; Markdown processor: not required for editing, for rendering HTML for preview and export.
  ;; (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))


;;; python-lang
(use-package python
  :ensure nil
  :config
  ;; Remove guess indent python message
  (setq python-indent-guess-indent-offset-verbose nil))


;;; go-lang
(defun xy/install-go-tool (pkg)
  "Install or update go tools."
  (interactive)
  (unless (executable-find "go")
    (user-error "Unable to find `go' in `exec-path'!"))
  (message "Installing go tool...")
  (set-process-sentinel
   (start-process "go-tool" "*Go Tool*" "go" "install" "-v" "-x" (concat pkg "@latest"))
   (lambda (proc _)
     (let ((status (process-exit-status proc)))
       (if (= 0 status)
           (message "Installed %s" pkg)
         (message "Failed to install %s: %d" pkg status))))))

(use-package go-mode
  ;; :bind (:map go-mode-map
  ;;             ("\C-c \C-c" . compile)
  ;;             ("\C-c \C-g" . go-goto-imports)
  ;;             ("\C-c \C-k" . godoc)
  ;;             ("M-j" . godef-jump))
  :config
  ;; goimports updates your Go import lines, adding missing ones and removing unreferenced ones
  ;; it also formats your code in the same style as gofmt so it can be used as a replacement for your editor's gofmt-on-save hook
  (unless (executable-find "goimports")
    (xy/install-go-tool "golang.org/x/tools/cmd/goimports"))
  (setq gofmt-command "goimports")

  (add-hook 'go-mode-hook (lambda ()
                            (setq-local tab-width 4)
                            (add-hook 'before-save-hook #'gofmt-before-save nil t))))

;; Edit struct field tag
(use-package go-tag
  :after go-mode
  :bind (:map go-mode-map
              ("C-c C-a" . go-tag-add)
              ("C-c C-r" . go-tag-remove))
  :init
  ;; (setq go-tag-args (list "-transform" "snakecase"))
  :config
  (unless (executable-find "gomodifytags")
    (xy/install-go-tool "github.com/fatih/gomodifytags")))

;; Fill struct literal with default values
(use-package go-fill-struct
  :after go-mode
  :config
  (unless (executable-find "fillstruct")
    (xy/install-go-tool "github.com/davidrjenni/reftools/cmd/fillstruct")))


;;; lsp
(defvar xy/lsp-want-modes
  '(go-mode
    go-ts-mode
    ;; python-mode python-ts-mode
    python-base-mode
    sh-mode))

;; Eglot ("Emacs Polyglot") is an Emacs LSP client
;; (info "(eglot) Eglot Variables")
;; @see news at https://elpa.gnu.org/devel/eglot.html
(use-package eglot
  :init
  (dolist (mode xy/lsp-want-modes)
    (add-hook (intern (format "%s-hook" mode)) #'eglot-ensure))
  :bind (("C-c c e" . #'eglot)
          :map eglot-mode-map
          ("C-c c r" . #'eglot-rename)
          ("C-c c a" . #'eglot-code-actions)
          ("C-c c ?" . #'eglot-show-workspace-configuration)
          ("C-c c !" . #'eglot-signal-didChangeConfiguration)
          ;; ("M-." . #'xref-find-definitions)
          ;; ("C-h ." . #'eldoc-doc-buffer)
          ("C-c c t" . #'eglot-show-type-hierarchy)
          ("C-c c h" . #'eglot-show-call-hierarchy)
          :map eglot-diagnostics-map)
  :config
  (add-to-list 'eglot-server-programs
               '(conf-toml-mode . ("taplo" "lsp" "stdio")))
  (setq eglot-sync-connect 0)
  (setq eglot-autoshutdown t)
  (setq eglot-events-buffer-config '(:size 8000 :format full))
  (setq eglot-extend-to-xref t)
  (setq eglot-advertise-cancellation t)
  ;; (setq eglot-confirm-server-edits '((t . diff)))
  )

(use-package consult-eglot
  ;; `consult-eglot-narrow'
  :bind ( :map eglot-mode-map
          ("C-c c s" . consult-eglot-symbols)))

(use-package consult-eglot-embark
  :after (embark consult-eglot) :demand t
  :config
  (consult-eglot-embark-mode +1))

;; (use-package eglot-signature-eldoc-talkative
;;   :after eglot :demand t
;;   :config
;;   (advice-add #'eglot-signature-eldoc-function
;;               :override #'eglot-signature-eldoc-talkative))

(use-package eglot-inactive-regions
  :after eglot
  :hook (c-mode cpp-mode))

;; speedier performance and less I/O blocking
(use-package eglot-booster
  :vc ( :url "https://github.com/jdtsmith/eglot-booster"
        :rev :newest)
  :after eglot :demand t
  :config
  (eglot-booster-mode)
  (setq eglot-booster-io-only t))
