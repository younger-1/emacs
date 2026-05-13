;; -*- lexical-binding: t -*-

;;; startup frame and screen
(use-core emacs
  :custom
  (inhibit-startup-echo-area-message user-login-name)
  :init
  (setq inhibit-default-init t)
  ;; (setq inhibit-startup-screen t)
  ;; (setq inhibit-x-resources t)
  (setq initial-major-mode #'fundamental-mode)

  ;; Poor man's Initial Mode to startup faster with scratch buffer
  (progn
    (defvar-keymap xy/initial-mode-map
      "C-c C-c" #'lisp-interaction-mode)
    (define-derived-mode xy/initial-mode nil "Initial"
      "Major mode for start up buffer.\\{xy/initial-mode-map}"
      (setq-local text-mode-variant t)
      (setq-local indent-line-function 'indent-relative))
    (setq initial-major-mode 'xy/initial-mode))

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
(use-core emacs
  :config
  ;; tab
  (setq-default indent-tabs-mode nil)
  ;; (setq-default tab-width 4)
  ;; TAB key for indentation+completion. `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)
  (setq backward-delete-char-untabify-method 'all)

  ;; mark
  (setq mark-even-if-inactive nil)

  ;; kill
  ;; (setq kill-whole-line t)
  (setq kill-do-not-save-duplicates t)
  (setq save-interprogram-paste-before-kill t)
  ;; To prevent kill and yank commands from accessing the clipboard
  ;; (setq select-enable-clipboard nil)
  ;; (keymap-global-set "M-w" #'clipboard-kill-ring-save)

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
  ;; @perf
  ;; (setq fast-but-imprecise-scrolling t)
  ;; (setq jit-lock-defer-time 0.05)

  ;; limit
  (setq large-file-warning-threshold (* 64 1024 1024)) ; 10m -> 64m
  (setq read-process-output-max (* 512 1024)) ; 64k -> 512k
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
  ;; Useful for long lines in comments or markdown list
  (global-visual-wrap-prefix-mode +1)
  ;; (global-visual-line-mode +1)
  (setq-default word-wrap t)
  (setq word-wrap-by-category t)

  ;; truncate
  ;; Auto truncate lines if a split window becomes too narrow, regardless of `truncate-lines'
  (setq truncate-partial-width-windows 40)
  ;; @tip use "C-x x t" (`toggle-truncate-lines')
  ;; (setq-default truncate-lines t)
  (defun xy/truncate-lines ()
    (setq-local truncate-lines t))
  ;; (add-hook 'prog-mode-hook #'xy/truncate-lines)
  ;; (add-hook 'log-view-mode-hook #'xy/truncate-lines)

  ;; comment
  (setq-default comment-column 0)
  (setq comment-empty-lines t)
  ;; (setq comment-multi-line t)
  ;; (setq comment-auto-fill-only-comments t)
  ;; (setq comment-style 'multi-line)

  ;; buffer
  ;; (setq uniquify-buffer-name-style 'forward)
  (setq switch-to-buffer-obey-display-actions t)
  (setq switch-to-buffer-in-dedicated-window 'pop)
  ;; (setq display-buffer-base-action '((display-buffer-reuse-window display-buffer-same-window)
  ;;                                    (reusable-frames . t)))
  ;;
  ;; TODO: https://christiantietze.de/posts/2025/05/compilation-window-display-in-emacs-via-display-buffer-alist/
  ;; Do not show warnings when installing packages
  ;; from https://protesilaos.com/codelog/2024-11-28-basic-emacs-configuration/
  ;; (add-to-list 'display-buffer-alist
  ;;              '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
  ;;                (display-buffer-no-window)
  ;;                (allow-no-window . t)))
  ;; Keep the compilation buffer in the background, except when there's an error
  (add-to-list 'display-buffer-alist
               '("\\*.*compilation\\*" (display-buffer-no-window)))

  ;; window
  ;; (setq split-height-threshold nil
  ;;       split-width-threshold 0)
  ;; Avoid resizing
  ;; (setq even-window-sizes nil)
  ;; Proportional Window Resizing
  (setq window-combination-resize t)

  ;; cursor
  (setq-default cursor-type 'box)
  (setq x-stretch-cursor t)
  ;; (blink-cursor-mode -1)
  ;; (setq track-eol t)

  ;; line number
  ;; The 'visual is like 'relative but counts screen lines instead of buffer lines
  (setq display-line-numbers-type 'visual)
  ;; (setq display-line-numbers-current-absolute nil)
  ;; (setq-default display-line-numbers-widen t) ; widen line numbers when in narrow

  ;; fringe
  ;; https://emacsredux.com/blog/2015/01/18/customizing-the-fringes/
  (setq-default indicate-buffer-boundaries 'left)
  ;; (setq-default indicate-empty-lines t)

  ;; edit
  ;; (setq undo-no-redo t)
  ;; (setq next-line-add-newlines t)
  (setq open-paren-in-column-0-is-defun-start nil)
  (setq-default fill-column 80)
  ;; Disable the obsolete practice of end-of-line spacing from the typewriter era.
  (setq sentence-end-double-space nil)
  ;; According to the POSIX, a line is defined as "a sequence of zero or more non-newline characters followed by a terminating newline".
  (setq require-final-newline t)

  ;; @perf Disable Bidirectional Text Scanning (Doom Emacs)
  (setq-default bidi-display-reordering 'left-to-right
                bidi-paragraph-direction 'left-to-right)
  (setq bidi-inhibit-bpa t)

  ;; @perf Skip Fontification During Input (Doom Emacs)
  (setq redisplay-skip-fontification-on-input t)

  ;; flyspell
  ;; (setq flyspell-check-changes t)
  ;; @perf Prevent messages from being displayed for each word when checking the entire buffer.
  ;; (setq flyspell-issue-message-flag nil)

  ;; ispell
  ;; @dep @cli Spelling checker program, one of Hunspell, Aspell, Ispell or Enchant
  ;; Disable ispell completion to avoid annotation errors when no `ispell' dictionary is set.
  ;; (setq text-mode-ispell-word-completion nil)
  (setq ispell-silently-savep t)

  ;; GnuPG
  (setq epg-pinentry-mode 'loopback)

  (setq reb-re-syntax 'string)

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
  ;; (setq next-error-message-highlight t)
  (setq list-matching-lines-jump-to-current-line t)
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
  (setq delete-pair-blink-delay 0.1
        delete-pair-push-mark t)

  ;; `compile.el'
  (setq compilation-scroll-output 'first-error)
  ;; (setq compilation-always-kill t
  ;;       compilation-ask-about-save nil)

  ;; `C-code'
  (setq highlight-nonselected-windows t)
  ;; No beeping or blinking
  ;; (setq ring-bell-function #'ignore
  ;;       visible-bell nil)
  (setq-default show-trailing-whitespace t))


;;; hooks and keymaps
(use-core emacs
  :hook
  ;; (prog-mode . show-paren-local-mode)
  ;; (prog-mode . electric-indent-local-mode)
  ;; (prog-mode . electric-pair-local-mode)
  ;; (prog-mode . subword-mode)
  ;;
  (before-save . delete-trailing-whitespace)
  (after-save . executable-make-buffer-file-executable-if-script-p) ; Only work if buffer begin with "#!"
  :config
  ;; (show-paren-mode +1) ; @default
  ;; (electric-indent-mode +1) ; @default
  (electric-pair-mode +1) ; Pair everywhere, include minibuffer
  (global-subword-mode +1)
  ;;
  (global-display-line-numbers-mode +1)
  (column-number-mode +1) ; modeline
  (size-indication-mode +1) ; modeline
  (delete-selection-mode +1)
  ;; (global-display-fill-column-indicator-mode +1)

  (add-to-list 'global-display-fill-column-indicator-modes '(not calc-mode calc-trail-mode))

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
    (keymap-global-set "C-x y k" #'xy/wsl-kill)
    (keymap-global-set "C-x y y" #'xy/wsl-yank))

  :bind
  ;; free keys
  ("C-x c" . nil)
  ("C-x g" . nil)
  ("C-x j" . nil)
  ("C-x y" . nil)
  ;; rebind keys
  ("C-x f" . nil) ; `set-fill-column'
  ("C-x l" . nil) ; `count-lines-page'
  ("C-x m" . nil) ; `compose-mail'
  ;; ("C-x i" . nil) ; `insert-file'
  ;; ("C-x e" . nil) ; `kmacro-end-and-call-macro'
  ;; ("C-x q" . nil) ; `kbd-macro-query'
  ;; ("C-x ;" . nil) ; `comment-set-column'
  ;; ("C-x C-n" . nil) ; `set-goal-column'
  ;;
  ("C-z" . nil) ; `suspend-frame', use C-x C-z
  ;;
  ;; @tip s-k is `kill-current-buffer'
  ;; ("C-x k" . #'kill-current-buffer)
  ("C-x K" . #'bury-buffer)
  ("C-x O" . #'switch-to-minibuffer)
  ;;
  ("C-x x d" . #'display-fill-column-indicator-mode)
  ("C-x x f" . #'follow-mode)
  ("C-x x G" . #'redraw-display)
  ("C-x x b" . #'clone-indirect-buffer)
  ("C-x x B" . #'clone-indirect-buffer-other-window)
  ;;
  ("C-x j u" . #'browse-url)
  ("C-x j U" . #'browse-web))


;;; misc
(use-core emacs
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

  (defconst xy/elpa-lisp-dir (expand-file-name package-user-dir))
  (defconst xy/emacs-lisp-dir (file-name-directory (directory-file-name doc-directory)))

  (dir-locals-set-class-variables
   :read-only
   '((nil . (;; (eval . (view-mode-enter nil #'kill-buffer))
             (buffer-read-only . t)
             (tab-width . 8)))))
  (dolist (dir (list xy/elpa-lisp-dir xy/emacs-lisp-dir))
    (dir-locals-set-directory-class (file-truename dir) :read-only)))

(provide 'init-emacs)
