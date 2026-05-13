;; -*- lexical-binding: t -*-

;;; ui
(use-core frame
  :hook
  (emacs-startup . undelete-frame-mode) ; C-x 5 u -> `undelete-frame'
  (emacs-startup . window-divider-mode)
  :config
  (setopt window-divider-default-places t
          window-divider-default-right-width 1
          window-divider-default-bottom-width 1))

(use-core mouse
  ;; :hook
  ;; (emacs-startup . context-menu-mode)
  :config
  ;; (setq mouse-autoselect-window t)
  (setq select-active-regions 'only)
  ;; (setq mouse-drag-copy-region t)
  (setq mouse-yank-at-point t)

  (setq context-menu-functions '(context-menu-undo
                                 context-menu-region
                                 context-menu-middle-separator
                                 ;; context-menu-toolbar
                                 ;; context-menu-global
                                 context-menu-local
                                 context-menu-minor
                                 ;; context-menu-buffers
                                 context-menu-project
                                 context-menu-vc
                                 context-menu-ffap
                                 hi-lock-context-menu
                                 occur-context-menu
                                 Man-context-menu
                                 dictionary-context-menu))

  (setq dnd-indicate-insertion-point t
        dnd-scroll-margin 3
        mouse-drag-and-drop-region 'control
        mouse-drag-and-drop-region-cut-when-buffers-differ t
        mouse-drag-and-drop-region-cross-program t))

;; Better mouse-buffer-menu
(use-core msb
  :defer 0.4
  :bind ("C-c t m" . msb-mode)
  :config (msb-mode +1))

(use-core tab-line
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

(use-core tab-bar
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

;; (use-core window-tool-bar
;;   :hook
;;   ;; (emacs-startup . global-window-tool-bar-mode)
;;   (special-mode . window-tool-bar-mode))

;; Tabs and ribbons for the mode line
(use-package moody
  :defer 1
  :config
  (moody-replace-mode-line-front-space)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-eldoc-minibuffer-message-function)
  (moody-replace-vc-mode))

(use-package hide-mode-line
  :hook
  (inferior-python-mode) ; `run-python'
  (completion-list-mode))

;; Show current command and its key in the mode line
(use-package keycast
  :defer 1
  :init
  (setq keycast-mode-line-remove-tail-elements nil)
  :config
  (keycast-mode-line-mode +1)
  (setq keycast-mode-line-window-predicate #'mode-line-window-selected-p))

;; Replace list of minor modes displayed in the mode line
(use-package minions
  :defer 1
  :bind ([S-down-mouse-3] . minions-minor-modes-menu)
  :config
  (minions-mode +1)
  ;; (setq minions-mode-line-delimiters nil)
  ;; (setq minions-prominent-modes '(emms))
  )

;; (use-package mini-echo
;;   :defer 1
;;   :config
;;   (mini-echo-mode +1))

;; Print current function in mode/head line
;; (use-core which-func
;;   :after imenu :demand t
;;   :config
;;   (setq which-func-display 'header)
;;   (which-function-mode +1))

(use-package breadcrumb
  :defer 1
  :config
  (breadcrumb-mode +1)
  (setq breadcrumb-imenu-crumb-separator " "))

;; (use-package sideline
;;   :defer 1
;;   :config
;;   (global-sideline-mode +1))

;; (use-package sideline-load-cost
;;   :vc ( :url "https://github.com/emacs-sideline/sideline-load-cost"
;;         :rev :newest)
;;   :init
;;   (setq sideline-backends-right '(sideline-load-cost)))



;;; appearance
;; Show pretty symbols
;; (info "(emacs) Misc for Programs")
(use-core prog-mode
  :config
  ;; Show markup at point
  (setq prettify-symbols-unprettify-at-point 'right-edge)
  (global-prettify-symbols-mode +1))

;; (use-package page-break-lines
;;   :defer 1
;;   :bind ("C-c t p" . page-break-lines-mode)
;;   :config
;;   (global-page-break-lines-mode +1)
;;   (setq page-break-lines-max-width 80))

;; Visually distinguish "real" buffers from "unreal" buffers
;; by giving the latter a slightly different -- often darker -- background:
;; (use-package solaire-mode
;;   :bind ("C-c t s" . solaire-mode)
;;   :init
;;   ;; TODO
;;   ;; (defvar xy/theme-bg-alist
;;   ;;   '((modus-vivendi . "black")))
;;   ;; (cdr (assoc 'modus-vivendi xy/theme-bg-alist))
;;   ;; (car custom-enabled-themes)
;;   ;; (set-face-attribute 'solaire-default-face nil
;;   ;;                     :background "black")
;;   ;; (custom-theme-set-faces 'modus-vivendi
;;   ;;                         '(solaire-default-face ((t (:background "black")))))
;;   :config
;;   (solaire-global-mode +1))

;; Simple distraction-free editing
;; (use-package darkroom
;;   :bind ("C-c t w" . darkroom-tentative-mode)
;;   :config
;;   (setq darkroom-fringes-outside-margins nil))

;; Like olivetti/darkroom, but also effect current frame by `writeroom-global-effects'
(use-package writeroom-mode
  :bind (("C-c t w" . writeroom-mode)
         ("C-c t W" . global-writeroom-mode)
         :map writeroom-mode-map
         ;; @tip Use "s-?" toggle mode-line
         ("C-c {" . writeroom-decrease-width)
         ("C-c }" . writeroom-increase-width))
  :config
  (defvar-keymap xy/writeroom-mode-map
    :repeat t
    "{" #'writeroom-decrease-width
    "}" #'writeroom-increase-width
    "=" #'writeroom-adjust-width)
  (setq writeroom-fullscreen-effect 'maximized)
  (setq writeroom-width 0.618)
  ;; In which `global-writeroom-mode' will active
  (setq writeroom-major-modes '(text-mode org-mode markdown-mode Info-mode))
  (setq writeroom-restore-window-config t)
  (setq writeroom-mode-line-toggle-position 'mode-line-format))

;; A distraction-free writing environment, by automatically balance window margins
;; (use-package olivetti
;;   ;; @tip Change body width with: C-c { and C-c }
;;   :bind ("C-c t w" . olivetti-mode)
;;   :config
;;   ;; (setq olivetti-style 'fancy)
;;   (setq olivetti-body-width 0.618))


;;; highlight
(use-core hl-line
  :bind ("C-c t l" . hl-line-mode)
  :hook
  (prog-mode . hl-line-mode)
  (text-mode . hl-line-mode)
  (conf-mode . hl-line-mode)
  (help-mode . hl-line-mode)
  (Info-mode . hl-line-mode)
  (dired-mode . hl-line-mode)
  (package-menu-mode . hl-line-mode)
  (ibuffer-mode . hl-line-mode))

;; Make `hl-line-mode' more suitable for selection UIs
;; Lin 让 “选行界面” 更清晰、“编辑界面” 更柔和 (醒目选择 vs 柔和编辑)
;; TODO: https://christiantietze.de/posts/2022/03/hl-line-priority/
(use-package lin
  :after hl-line :demand t
  :bind ("C-c t L" . lin-mode)
  :config
  ;; red cyan magenta purple orange
  (setopt lin-face 'lin-purple)
  ;; @see `lin-mode-hooks'
  (lin-global-mode +1))

;; Temporarily highlights current line
;; 1. Automatic pulse after a function in the `pulsar-pulse-functions'
;; 2. Region-related changes, covers copyring, pasting, undoing, redoing
;; 3. Window-related changes, includes selection, addition, deletion, resize
;; (use-package pulsar
;;   :defer 1
;;   :bind
;;   ("C-x l" . pulsar-pulse-line) ; @orig `count-lines-page'
;;   ("C-x L" . pulsar-highlight-permanently-dwim) ; or use `pulsar-highlight-temporarily-dwim'
;;   :hook
;;   (next-error . pulsar-pulse-line-red)
;;   (minibuffer-setup . pulsar-pulse-line-green)
;;   ;; integration with `consult':
;;   (consult-after-jump . pulsar-recenter-top) ; or `pulsar-recenter-center'
;;   (consult-after-jump . pulsar-reveal-entry) ; displays the hidden contents of an Org or Outline heading
;;   ;; integration with `imenu':
;;   (imenu-after-jump . pulsar-recenter-top)
;;   (imenu-after-jump . pulsar-reveal-entry)
;;   :config
;;   (pulsar-global-mode +1)
;;   ;; (setq pulsar-pulse-on-window-change t)
;;   ;; (setq pulsar-inhibit-hidden-buffers nil)
;;   (setq pulsar-delay 0.05)
;;   (setq pulsar-iterations 15)
;;   ;; for `pulsar-pulse-functions'
;;   (setq pulsar-face 'pulsar-generic)
;;   ;; for `pulsar-pulse-region-functions'
;;   (setq pulsar-region-face 'pulsar-yellow)
;;   ;; for static highlight (temporary or permanent)
;;   (setq pulsar-highlight-face 'pulsar-magenta))

;; Pulse modified region. Undo, yank, kill and delete are supported
(use-package goggles
  :hook (prog-mode text-mode conf-mode)
  :config
  (setq goggles-pulse-delay 0.2))

;; Temporarily highlight focused windows
(use-package winpulse
  :vc ( :url "https://github.com/xenodium/winpulse"
        :rev :newest)
  :defer 1
  :config
  (winpulse-mode +1))

;; Highlight delimiters such as parentheses, brackets or braces according to their depth
(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Highlight identifiers based on hash of names
(use-package rainbow-identifiers
  :commands rainbow-identifiers-mode)

;; Highlight defined Emacs Lisp symbols
(use-package highlight-defined
  :commands highlight-defined-mode)

;; (use-package highlight-symbol
;;   :defer 0.6
;;   ;; :hook (prog-mode text-mode conf-mode special-mode)
;;   :bind
;;   ;; move within defun
;;   ("M-p" . highlight-symbol-prev-in-defun)
;;   ("M-n" . highlight-symbol-next-in-defun)
;;   ;; move within buffer
;;   ("C-c h m" . highlight-symbol-nav-mode)
;;   ;;
;;   ("C-c h s" . highlight-symbol) ; manual symbol highlighting
;;   ("C-c h S" . highlight-symbol-mode) ; automatic symbol highlighting
;;   ("C-c h r" . highlight-symbol-query-replace)
;;   ("C-c h o" . highlight-symbol-occur)
;;   ("C-c h c" . highlight-symbol-count)
;;   :config
;;   (setq highlight-symbol-highlight-single-occurrence nil)
;;   (setq highlight-symbol-idle-delay 0.5)
;;   (setq highlight-symbol-ignore-list '("^end$" "^def$" "^class$" "^module$")))

;; (use-package auto-highlight-symbol
;;   :bind ( :map auto-highlight-symbol-mode-map
;;           ("M-n" . ahs-forward)
;;           ("M-p" . ahs-backward)
;;           ("M-N" . ahs-forward-definition)
;;           ("M-P" . ahs-backward-definition)
;;           ("C-c h s" . ahs-highlight-now)
;;           ("C-c h S" . auto-highlight-symbol-mode)
;;           ("C-c h b" . ahs-back-to-start)
;;           ("C-c h n" . ahs-change-range)
;;           ("C-c h d" . ahs-display-stat)
;;           ("C-c h e" . ahs-edit-mode))
;;   :config
;;   (global-auto-highlight-symbol-mode +1))

(use-package symbol-overlay
  :defer 0.6
  ;; :hook (prog-mode text-mode conf-mode special-mode)
  :bind (;; move within buffer
         ("M-n" . symbol-overlay-jump-next)
         ("M-p" . symbol-overlay-jump-prev)
         ;; toggle highlight scope
         ("C-c h m" . symbol-overlay-toggle-in-scope)
         ;;
         ("C-c h s" . symbol-overlay-put)
         ("C-c h S" . symbol-overlay-mode)
         :map symbol-overlay-map
         ("]" . symbol-overlay-switch-forward)
         ("[" . symbol-overlay-switch-backward))
  :config
  ;; TODO: support region
  ;; (advice-add #'symbol-overlay-get-symbol :override
  ;;             (lambda (&optional noerror)
  ;;               (seq-some #'thing-at-point '(region symbol))))
  ;; (advice-add #'symbol-overlay-regexp :override
  ;;             (lambda (symbol)
  ;;               (regexp-quote symbol)))
  ;;
  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook special-mode-hook))
    (add-hook hook #'symbol-overlay-mode))
  (setq symbol-overlay-idle-time 0.2))

;; @problem Can't exclude current highlight when only one match
;; (use-package idle-highlight-mode
;;   :hook (prog-mode text-mode conf-mode special-mode)
;;   :config
;;   (setq idle-highlight-visible-buffers t))

;; TODO: navi map not compatible with evil-mode
(use-package region-occurrences-highlighter
  :defer 0.6
  :bind ( :map region-occurrences-highlighter-nav-mode-map
          ("M-n" . region-occurrences-highlighter-next)
          ("M-p" . region-occurrences-highlighter-prev))
  :config (global-region-occurrences-highlighter-mode +1))

;; ;; Uses built-in `thingatpt' and `hi-lock' functionality to identify the thing under point and highlight it.
;; ;; BUG: @my https://github.com/fgeller/highlight-thing.el/issues/28
;; ;; BUG: @my https://github.com/fgeller/highlight-thing.el/pull/29
;; (use-package highlight-thing
;;   :defer 0.6
;;   ;; :hook (prog-mode text-mode conf-mode special-mode)
;;   :bind ("C-c h h" . highlight-thing-mode)
;;   :custom-face
;;   ;; (highlight-thing ((t (:inherit mode-line))))
;;   ;; (highlight-thing ((t (:inherit minibuffer-depth-indicator))))
;;   :config
;;   (global-highlight-thing-mode +1)
;;   (setq highlight-thing-prefer-active-region t)
;;   ;; (setq highlight-thing-limit-to-defun t)
;;   (setq highlight-thing-all-visible-buffers-p t)
;;   (setq highlight-thing-limit-to-region-in-large-buffers-p t
;;         highlight-thing-narrow-region-lines 100))


;;; todo
(use-package hl-todo
  :defer 0.6
  :init
  (defvar-keymap xy/hl-todo-repeat-map
    :repeat t
    "p" #'hl-todo-previous
    "n" #'hl-todo-next)
  :bind (("C-c h t t" . hl-todo-mode)
         :map hl-todo-mode-map
         ("C-c h t p" . hl-todo-previous)
         ("C-c h t n" . hl-todo-next)
         ("C-c h t o" . hl-todo-occur)
         ("C-c h t i" . hl-todo-insert))
  :custom-face
  (hl-todo ((t (:inherit default :height 0.9 :width condensed :weight bold :underline nil :inverse-video t))))
  :config
  (global-hl-todo-mode +1)
  ;; To highlight TODO keywords in Magit
  (with-eval-after-load 'magit
    (add-hook 'magit-log-wash-summary-hook #'hl-todo-search-and-highlight t)
    (add-hook 'magit-revision-wash-message-hook #'hl-todo-search-and-highlight t)))

(use-package magit-todos
  :after magit-status :demand t
  :bind ("C-c g t" . magit-todos-list)
  :config
  ;; (magit-todos-mode +1)
  ;;
  (defun xy/toggle-magit-todos ()
    (interactive)
    (magit-todos-mode 'toggle)
    (magit-refresh))
  (transient-append-suffix 'magit-status-jump '(-1 -1 -1)
    '("/" "Toggle magit-todos" xy/toggle-magit-todos)))

(use-package consult-todo
  :bind
  ("C-c s t" . consult-todo)
  ("C-c s T" . consult-todo-all)
  ("C-x p t" . consult-todo-project)
  ("C-x p T" . consult-todo-dir))

;; (use-package flycheck-hl-todo
;;   :after flycheck :demand t
;;   :config
;;   (flycheck-hl-todo-setup)
;;   ;; Only enabled when hl-todo-mode is enabled
;;   (defun flycheck-hl-todo-follow-mode ()
;;     (setq flycheck-hl-todo-enabled hl-todo-mode)
;;     (flycheck-buffer))
;;   (add-hook 'hl-todo-mode-hook #'flycheck-hl-todo-follow-mode))


;;; buffer
;; Kill old buffers at midnight
(use-core midnight
  :defer 5
  :config
  (midnight-mode +1))

;; Useful to kill multiple buffers
(use-core ibuffer
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
                              ;; "\\`\\*scratch"        ; "C-x f s"
                              ;; "\\`\\*Messages"       ; "C-h e e"
                              "\\`\\*Bookmark List"  ; "C-x r l"
                              )
    "List of buffer names of buffers to hide on several occasions.")

  ;; (setq ibuffer-use-other-window t)
  (setq ibuffer-never-show-predicates xy/boring-buffers)

  ;; Use these keybindings to configure IBuffer to be consistent with keybindings used by Casual IBuffer
  (keymap-set ibuffer-mode-map "{" #'ibuffer-backwards-next-marked)
  (keymap-set ibuffer-mode-map "}" #'ibuffer-forward-next-marked)
  (keymap-set ibuffer-mode-map "[" #'ibuffer-backward-filter-group)
  (keymap-set ibuffer-mode-map "]" #'ibuffer-forward-filter-group)
  (keymap-set ibuffer-mode-map "$" #'ibuffer-toggle-filter-group)
  ;; Mouse click binding in IBuffer
  (keymap-set ibuffer-mode-map "<double-mouse-1>" #'ibuffer-visit-buffer)
  (keymap-set ibuffer-mode-map "M-<double-mouse-1>" #'ibuffer-visit-buffer-other-window))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode))


;;; window
(use-core winner
  :defer 0.4
  :bind
  ("C-x 4 u" . #'winner-undo)
  ("C-x 4 r" . #'winner-redo)
  :config
  (winner-mode +1))

(use-core windmove
  :defer 0.4
  :config
  ;; @tip shift and ctrl-shift is used by Org-Mode
  (windmove-default-keybindings 'ctrl)
  (windmove-swap-states-default-keybindings '(ctrl shift))
  ;; (windmove-display-default-keybindings '(ctrl meta))
  ;; @tip C-x shift-arrow to delete window
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

;; FIXME: not compatible with which-key and sidebar (imenu-list)
(use-package zoom
  :defer 0.5
  :bind
  ("C-x w z" . zoom)
  ("C-c t z" . zoom-mode)
  ("C-c t Z" . #'xy/toggle-zoom-size)
  :config
  (setq zoom-ignored-major-modes '(dired-mode))
  (setq zoom-ignored-buffer-names '(" *which-key*"))
  (setq zoom-ignored-buffer-name-regexps '("^*calc"))
  (setq zoom-ignore-predicates '((lambda () (< (count-lines (point-min) (point-max)) 20))))
  ;; (zoom-mode +1)
  (defun xy/toggle-zoom-size ()
    (interactive)
    (if (equal zoom-size xy/golden-ratio)
        (setq zoom-size xy/zoom-default)
      (setq zoom-size xy/golden-ratio)))
  (defconst xy/zoom-default zoom-size)
  (defconst xy/golden-ratio '(0.618 . 0.618))
  (setq zoom-size xy/golden-ratio))

;; Manage window configurations
;; @note `eyebrowse-keymap-prefix' is C-c C-w
;; (use-package eyebrowse
;;   :defer 1
;;   :config
;;   (eyebrowse-mode +1))

;; Persistent (saving and restoring) window configurations with several frames.
;; `desktop' is reliable only for single-frame use. When using multiple Emacs frames, it depends in what order the frames are closed, and only the last one is remembered.
;; (use-package eyebrowse-restore
;;   :after eyebrowse :demand t
;;   :config
;;   (eyebrowse-restore-mode +1)
;;   ;; For a better experience, I recommend naming your Emacs frames:
;;   (set-frame-parameter nil 'name "Main"))

;; (use-package perspective
;;   :defer 1
;;   :bind
;;   ( :map persp-mode-map
;;     ("C-c M-p" . perspective-map)
;;     :map perspective-map ; prefix command
;;     ("M-b" . persp-switch-to-buffer*)
;;     ("M-k" . persp-kill-buffer*)
;;     ("M-i" . persp-ibuffer))
;;   :config
;;   (setq persp-suppress-no-prefix-key-warning t)
;;   (persp-mode +1)
;;   ;; Let `previous-buffer' skip buffers not in current perspective
;;   (setq switch-to-prev-buffer-skip
;;         (lambda (_win buff _bury-or-kill)
;;           (not (persp-is-current-buffer buff))))
;;   ;; Group buffers by persp-name in ibuffer
;;   (add-hook 'ibuffer-hook #'persp-ibuffer-set-filter-groups)
;;   ;; Use narrow key `s' to list buffers in current perspective
;;   (with-eval-after-load 'consult
;;     ;; Use narrow key `b' to list all buffers in all perspectives
;;     (consult-customize consult--source-buffer :hidden t :default nil)
;;     (add-to-list 'consult-buffer-sources persp-consult-source))
;;   ;; Save sessions to disk
;;   (setq persp-state-default-file (expand-file-name ".perspective-state" user-emacs-directory))
;;   ;; (persp-state-load persp-state-default-file)
;;   (add-hook 'kill-emacs-hook #'persp-state-save))

;; Designate any buffer to “popup” status to disimss/summon/cycle them.
;; e.g. toggling display of help buffers, REPLs, grep and occur buffers, shell and compilation output, log buffers etc
(use-package popper
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type)) ; Turn any buffer into a popup (or vice-versa)
  :config
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*Warnings\\*"
          help-mode
          ;; emacs-lisp-compilation-mode ; byte/native compile log
          "\\*Compile-Log\\*"
          "\\*Async-native-compile-log\\*"
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))


;;; dired
(use-core dired
  :bind (("C-x d d" . dired)
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
  (setq dired-movement-style 'cycle)
  ;;
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

(use-package treemacs-nerd-icons
  :after treemacs :demand t
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package projtree
  :vc ( :url "https://github.com/petergardfjall/emacs-projtree"
        :rev :newest)
  :bind ("C-x d p" . projtree-mode))


;;; project
;; `project-prefix-map'
(use-core project
  :config
  (setq project-mode-line t))

(use-package disproject
  :bind (("C-x P" . disproject-dispatch)))

;; (use-package projection
;;   :defer 1
;;   :hook (compilation-mode . projection-customize-compilation-mode)
;;   :bind-keymap ("C-c P" . projection-map)
;;   :config
;;   (global-projection-hook-mode +1))

;; Find file/directory and review Diff/Patch/Commit under any VSC
(use-package find-file-in-project
  :init
  (define-prefix-command 'xy/ffip-map)
  :bind
  (("C-x M-p" . xy/ffip-map)
   :map xy/ffip-map
   ;; ("f" . find-file-in-project)
   ("f" . find-file-in-project-by-selected)
   ("." . find-file-in-project-at-point)
   ("d" . find-directory-in-project-by-selected)
   ("a" . find-file-with-similar-name)
   ("A" . ffip-fix-file-path-at-point)
   ("i" . ffip-insert-file)
   ("r" . ffip-find-files-resume)
   ("," . ffip-find-relative-path)
   ("d" . ffip-show-diff)))

;; @tip "s-p p" -> `projectile-switch-project'
;; https://docs.projectile.mx/projectile/index.html
(use-package projectile
  :bind-keymap ("s-p" . projectile-command-map)
  :config
  ;; 1. let project.el use it to locate project by `projectile-project-root-functions'
  ;;    @see https://docs.projectile.mx/projectile/projects.html#customizing-project-detection
  ;; 2. provide menu bar items by `projectile-mode-map'
  (projectile-mode +1)
  ;; (setq projectile-switch-project-action #'projectile-dired)
  (setq projectile-auto-discover t)
  (setq projectile-auto-cleanup-known-projects t)
  ;; I use it mainly for this
  (setq projectile-project-search-path
        '("~/dotter/" "~/notes/" "~/project/" "~/work/" ("~/src/" . 2))))


;;; theme
(use-core solar
  :defer 1
  :config
  (setq calendar-latitude 40)
  (setq calendar-longitude 116))

;; Switch themes depending on the time of the day
;; (use-package circadian
;;   :after solar :demand t
;;   :config
;;   (setq circadian-themes '(("8:00" . modus-operandi)
;;                            ("19:30" . modus-vivendi)
;;                            (:sunrise . modus-operandi)
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
          (5 variable-pitch 1.4)        ; absence of weight means `bold'
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

;; The theme that Tsoding is using
(use-package gruber-darker-theme)

;; https://emacs-china.org/t/emacs/29503/18
(use-package koishi-theme)

(provide 'init-ui)
