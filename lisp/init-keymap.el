;; -*- lexical-binding: t -*-

(require 'init-package)

(use-core repeat
  :defer 0.3
  :config
  (repeat-mode +1)
  ;; (setq repeat-exit-key "RET")
  (setq repeat-exit-key "q")
  :init
  (keymap-global-set "C-x U" #'undo-only)
  ;; @tip Modify the given :keymap and refer to it by a new name
  (defvar-keymap xy/undo-repeat-map
    :keymap undo-repeat-map
    :repeat t
    "U" #'undo-only
    "r" #'undo-redo) ; useful to shorten "undo records" by balancing out previous `undo'

  (defvar-keymap xy/page-navigation-repeat-map
    :keymap page-navigation-repeat-map
    :repeat t
    "{" #'backward-paragraph
    "}" #'forward-paragraph)

  (defvar-keymap xy/navi-repeat-map
    :repeat ( :enter (forward-word backward-word) ;; forward-page backward-page
              :exit (transpose-sexps kill-sexp backward-kill-sexp kill-backward-up-list raise-sexp mark-sexp eval-defun)
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

(use-core which-key
  :defer 0.3
  :bind (("C-h w d" . which-key-dump-bindings)
         ("C-h w w" . which-key-show-full-keymap)
         ("C-h w W" . which-key-show-keymap)
         ("C-h w m" . which-key-show-full-major-mode)
         ("C-h w M" . which-key-show-major-mode)
         ("C-h w n" . which-key-show-full-minor-mode-keymap)
         ("C-h w N" . which-key-show-minor-mode-keymap))
  :config
  (which-key-mode +1)
  ;;
  (setq which-key-compute-remaps t)
  (setq which-key-lighter nil
        which-key-separator " → "
        which-key-add-column-padding 1
        which-key-min-display-lines 6)
  (setq which-key-idle-delay .5
        which-key-idle-secondary-delay .0)
  ;; @tip Press h/C-h after which-key's paging will run `which-key-show-standard-help', which run `describe-prefix-bindings'
  (setq which-key-use-C-h-commands t)
  (setq which-key-sort-order 'which-key-key-order-alpha
        which-key-sort-uppercase-first nil))

;; Support :chords keyword for `key-chord-mode'
;; (use-package use-package-chords
;;   :demand t)

;; @note Key chord mode uses `input-method-function'. And so do internationalisation packages (mule, quail, etc). Do not expect them to work well together.
;; (use-package key-chord
;;   :defer 0.3
;;   :chords
;;   (",." . "<>\C-b")
;;   (",," . indent-for-comment)
;;   :bind
;;   ("C-h w c" . key-chord-describe)
;;   :config
;;   ;; When detect typing, disable chord detection to help prevent accidental chord triggering
;;   (setq key-chord-typing-detection t)
;;   (setq key-chord-typing-speed-threshold 0.1) ; Adjust how fast keystrokes need to be to be considered "typing"
;;   (setq key-chord-typing-reset-delay 0.5) ; How long to wait after typing stops before re-enabling chord detection
;;   (key-chord-mode +1))

(use-core ffap
  :defer 1
  :bind
  ("C-x M-f" . #'ffap-menu)
  :config
  ;; @tip
  ;; (keymap-global-set "S-<mouse-3>" 'ffap-at-mouse)
  ;; (keymap-global-set "C-S-<mouse-3>" 'ffap-menu)
  ;; (ffap-bindings)
  ;; Tells ffap to never try network lookups to prevent it from pinging hostnames when run `find-file-at-point'
  (setq ffap-machine-p-known 'reject))

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

;; https://magit.vc/manual/transient/
(use-package transient-showcase
  :vc ( :url "https://github.com/positron-solutions/transient-showcase"
        :rev :newest)
  :bind ("C-h t s" . tsc-showcase))

;; A collection of Transient menus for various built-in Emacs modes
(use-package casual
  :bind ("M-m" . casual-editkit-main-tmenu)
  :init
  (with-eval-after-load 'dired
    (keymap-set dired-mode-map "M-m" #'casual-dired-tmenu))
  (with-eval-after-load 'isearch
    (keymap-set isearch-mode-map "M-m" #'casual-isearch-tmenu))
  (with-eval-after-load 'ibuffer
    (keymap-set ibuffer-mode-map "M-m" #'casual-ibuffer-tmenu)
    (keymap-set ibuffer-mode-map "F" #'casual-ibuffer-filter-tmenu)
    (keymap-set ibuffer-mode-map "s" #'casual-ibuffer-sortby-tmenu))
  (with-eval-after-load 'info
    (keymap-set Info-mode-map "M-m" #'casual-info-tmenu))
  ;; (with-eval-after-load 'calc
  ;;   (keymap-set calc-mode-map "M-m" #'casual-calc-tmenu))
  ;; (with-eval-after-load 're-builder
  ;;   (keymap-set reb-mode-map "M-m" #'casual-re-builder-tmenu)
  ;;   (keymap-set reb-lisp-mode-map "M-m" #'casual-re-builder-tmenu))
  (with-eval-after-load 'bookmark
    (keymap-set bookmark-bmenu-mode-map "M-m" #'casual-bookmarks-tmenu))
  ;; (with-eval-after-load 'org-agenda
  ;;   (keymap-set org-agenda-mode-map "M-m" #'casual-agenda-tmenu))
  ;;
  ;; ;; Ediff
  (keymap-global-set "C-c d d" #'casual-ediff-revision)
  (with-eval-after-load 'ediff
    (casual-ediff-install) ; run this to enable Casual Ediff
    (add-hook 'ediff-keymap-setup-hook
              (lambda () (keymap-set ediff-mode-map "M-m" #'casual-ediff-tmenu))))
  :config
  (setq casual-lib-use-unicode t))

(use-package casual-avy
  :bind ("M-g SPC" . casual-avy-tmenu))

(use-package casual-symbol-overlay
  :after symbol-overlay
  :bind ( :map symbol-overlay-map
          ("RET" . casual-symbol-overlay-tmenu)))


;;; evil
;; @tip
;; `evil-toggle-key' is "C-z"
;; Use "\" to execute next command in Emacs state
(use-package evil
  :defer 2
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)

  (setq evil-disable-insert-state-bindings t)
  (setq evil-want-minibuffer t)

  (setq evil-want-Y-yank-to-eol t)

  (setq evil-shift-width 2)
  (setq evil-move-beyond-eol t)
  (setq evil-cross-lines t)
  (setq evil-respect-visual-line-mode t)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)

  ;; Enable text object match, e.g. kbd::gn
  (setq evil-search-module 'evil-search)

  ;; https://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word/20717
  ;; @tip An underscore _ is a word character in Vim, but not in emacs
  ;; so Evil use kbd::o as symbol object, making kbd::cio a good alternative to Vim’s kbd::ciw
  (setq evil-symbol-word-search t)

  ;; (setq evil-want-fine-undo t)
  (setopt evil-undo-system 'undo-redo)
  (with-eval-after-load 'undo-fu
    (setopt evil-undo-system 'undo-fu))

  (setq evil-mode-line-format '(before . mode-line-frame-identification))

  :config
  (evil-mode +1)

  ;; (setq evil-default-state 'emacs)
  (evil-set-initial-state 'special-mode 'emacs)
  ;; (evil-set-initial-state 'fundamental-mode 'emacs)
  ;; @see `evil-vars'
  (setq evil-emacs-state-modes (append evil-emacs-state-modes evil-motion-state-modes))
  (setq evil-motion-state-modes nil)
  (setq evil-insert-state-modes nil)
  (setq evil-emacs-state-modes (append evil-emacs-state-modes '(messages-buffer-mode minibuffer-mode dired-mode diff-mode difftastic-mode deadgrep-mode deadgrep-edit-mode shell-mode eshell-mode term-mode eat-mode)))

  ;; Show search match count in echo area. Replace package evil-anzu
  (defun xy/evil-ex-match-counter (&rest _)
    (let ((message-log-max nil)
          (search-upper-case (null (evil-ex-pattern-ignore-case evil-ex-search-pattern))))
      (when-let* ((regexp (evil-ex-pattern-regex evil-ex-search-pattern))
                  (total (how-many regexp (point-min) (point-max)))
                  (current (how-many regexp (point-min) (point))))
        (message (format "[%d/%d]: %s" (1+ current) total (car evil-ex-search-pattern))))))
  (advice-add #'evil-ex-start-search :after-until 'xy/evil-ex-match-counter)
  (advice-add #'evil-ex-search :after-while 'xy/evil-ex-match-counter)

  ;; ;; Rebind `universal-argument', since 'C-u' now scrolls the buffer
  ;; (global-set-key (kbd "M-u") 'universal-argument)
  ;; (define-key universal-argument-map (kbd "M-u") 'universal-argument-more)

  ;; @tip motion state bindings are visible in normal and visual state, and normal state bindings are also visible in visual state.
  (evil-set-leader 'motion (kbd "SPC"))
  (evil-set-leader 'motion (kbd "C-c SPC") t)

  (evil-define-operator evil-comment (beg end)
    "Toggle comment from BEG to END."
    (interactive "<r>")
    (comment-or-uncomment-region beg end))

  (evil-define-key 'normal 'global
    "gc" #'evil-comment
    ;; Use visual line motions even outside of visual-line-mode buffers
    "j" #'evil-next-visual-line "k" #'evil-previous-visual-line
    (kbd "DEL") #'evil-switch-to-windows-last-buffer
    (kbd "<tab>") #'evil-jump-item)

  (setq select-enable-clipboard nil)
  (add-hook 'evil-emacs-state-entry-hook (lambda () (setq-local select-enable-clipboard t)))
  (add-hook 'evil-emacs-state-exit-hook (lambda () (setq-local select-enable-clipboard nil)))

  (evil-define-key 'visual 'global
    "X" #'clipboard-kill-region
    "Y" #'clipboard-kill-ring-save
    "d" #'delete-region
    ">" (defun xy/evil-shift-right ()
          "vnoremap < <gv"
          (interactive)
          (call-interactively #'evil-shift-right)
          (evil-normal-state)
          (evil-visual-restore))
    "<" (defun xy/evil-shift-left ()
          "vnoremap > >gv"
          (interactive)
          (call-interactively #'evil-shift-left)
          (evil-normal-state)
          (evil-visual-restore)))

  (evil-define-key 'insert 'global
    (kbd "s-v") #'clipboard-yank)

  (with-eval-after-load 'elisp-def
    (evil-set-command-property 'elisp-def :jump t))

  (with-eval-after-load 'diff-hl
    (evil-define-key 'normal 'global
      (kbd "]c") #'diff-hl-next-hunk
      (kbd "[c") #'diff-hl-previous-hunk
      (kbd "]d") #'diff-hl-show-hunk-next
      (kbd "[d") #'diff-hl-show-hunk-previous))

  (with-eval-after-load 'goggles
    (goggles-define undo primitive-undo evil-undo)
    (goggles-define yank yank yank-pop evil-yank evil-yank-line)
    (goggles-define delete delete-region evil-delete evil-delete-line)))

;; (use-package evil-collection
;;   :after evil :demand t
;;   :init
;;   ;; (setq evil-collection-setup-minibuffer t)
;;   :config
;;   (evil-collection-init))

(use-package evil-visualstar
  :after evil :demand t
  :config
  (global-evil-visualstar-mode +1))

;; Show search match count in mode line
;; (use-package evil-anzu
;;   :after evil :demand t
;;   :config (global-anzu-mode +1))

;; Enhanced % to match delimiters, % as text-object to manipulate
;; (use-package evil-matchit
;;   :after evil :demand t
;;   :init
;;   (setq evilmi-shortcut (kbd "<tab>"))
;;   :config
;;   (global-evil-matchit-mode +1))

;; Use c/d/y s {motion}{delimiter} to change/delete/add delimiter around motion
;; 1. use S/gS in visual-state
;; 2. use yss to wrap the entire line
(use-package evil-surround
  :after evil :demand t
  :config
  ;; @see `evil-surround-pairs-alist'
  (global-evil-surround-mode +1)
  (add-hook 'emacs-lisp-mode-hook (lambda () (push '(?` . ("`" . "'")) evil-surround-pairs-alist)))
  (add-hook 'c++-mode-hook (lambda () (push '(?< . ("< " . " >")) evil-surround-pairs-alist))))

;; Make evil-surround better, enable custom surrouding pairs
;; (use-package evil-embrace
;;   :after evil :demand t
;;   :config
;;   (evil-embrace-enable-evil-surround-integration))

(use-package evil-snipe
  :after evil :demand t
  :config
  ;; Use s/S for 2-char search
  ;; Use z/Z in operator-state
  (evil-snipe-mode +1)
  ;; Use f/F/t/T for 1-char search
  (evil-snipe-override-mode +1)
  ;;
  (setq evil-snipe-scope 'whole-visible)
  ;; Enable multi-char search by pressing <tab>
  (setq evil-snipe-tab-increment t))

;; (use-package evil-easymotion
;;   :after evil :demand t
;;   :config
;;   (evilem-default-keybindings "SPC j"))

;; Use gx{motion} to exchange
(use-package evil-exchange
  :after evil :demand t
  :config
  (setq evil-exchange-key (kbd "gz"))
  (setq evil-exchange-cancel-key (kbd "gZ"))
  (evil-exchange-install))

;; Align left/right with gl/gL
;; `gl{motion}{char}' to align on char
;; [glip=]
;; one = 1
;; three = 3
;; fifteen = 15
;;
;; [gLip,]
;; one, two, three,
;; fifteen, sixteen, seventeen
;;
;; [1glip"]
;; (red "red")
;; (teal-green "#6fb593")
;; (wheat "#b9c791")
;; (blue "blue")
;; (cyan "#54b6b6")
;;
(use-package evil-lion
  :after evil :demand t
  :config
  ;; (setq evil-lion-left-align-key (kbd "g a"))
  ;; (setq evil-lion-right-align-key (kbd "g A"))
  (evil-lion-mode +1))

(use-package evil-visual-mark-mode
  :after evil :demand t
  :config
  (evil-visual-mark-mode +1))

;; Preview marks and registers before using them
(use-package evil-owl
  :after evil :demand t
  :config
  (setq evil-owl-display-method 'window
        evil-owl-max-string-length 200)
  (add-to-list 'display-buffer-alist
               '("*evil-owl*"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.3)))
  (evil-owl-mode +1))

;; Display visual hint on evil edit operations
;; (use-package evil-goggles
;;   :after evil :demand t
;;   :config
;;   (setq evil-goggles-duration 0.3)
;;   (evil-goggles-mode +1)
;;   (evil-goggles-use-diff-faces))

(provide 'init-keymap)
