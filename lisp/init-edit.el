;; -*- lexical-binding: t -*-

(require 'init-package)

;;; edit

(use-package vundo
  :defer 0.8
  :bind (("C-x C-u" . vundo))
  :config
  ;; (vundo-popup-mode +1)
  (setq vundo-glyph-alist vundo-unicode-symbols))

;; Linear undo with redo
(use-package undo-fu
  :init
  (defvar-keymap xy/undo-fu-repeat-map
    :keymap undo-repeat-map
    :repeat t
    "b" #'undo-fu-only-undo
    "f" #'undo-fu-only-redo)
  :bind
  ("s-z" . undo-fu-only-undo)
  ("s-Z" . undo-fu-only-redo))

;; Save & recover undo steps between Emacs sessions
(use-package undo-fu-session
  :defer 0.8
  :config
  (undo-fu-session-global-mode +1)
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

;; C-w and M-w act on the current line when the mark is not active
(use-package whole-line-or-region
  :defer 1
  :config
  (whole-line-or-region-global-mode +1))

;; super-c/x/v to copy/cut/paste without affecting the kill ring
;; NOTE good choice if I not use evil with s-v mapping
;; TODO enable for tty
;; (use-package simpleclip
;;   :defer 1
;;   :config
;;   (simpleclip-mode +1))

(use-package copy-as-format
  :bind ("C-c w w" . copy-as-format) ; @prefix prompt for the format
  :config
  (setq copy-as-format-default "org-mode"))

(use-package expreg
  :bind (("C-=" . expreg-expand)
         ("C-M-SPC" . prot/expreg-expand-dwim))
  :init
  (defvar-keymap xy/expreg-repeat-map
    :repeat t
    "=" #'expreg-expand
    "-" #'expreg-contract)
  :config
  (setq expreg-restore-point-on-quit t)
  ;; For markdown and org file
  (add-hook 'text-mode-hook
            (lambda ()
              (add-to-list 'expreg-functions #'expreg--sentence)))
  ;;
  (defun prot/expreg-expand (n)
    "Expand to N syntactic units, defaulting to 1 if none is provided interactively."
    (interactive "p")
    (dotimes (_ n)
      (expreg-expand)))
  (defun prot/expreg-expand-dwim ()
    "Do-What-I-Mean `expreg-expand' to start with symbol or word.
If over a real symbol, mark that directly, else start with a
word.  Fall back to regular `expreg-expand'."
    (interactive)
    (let ((symbol (bounds-of-thing-at-point 'symbol)))
      (cond
       ((equal (bounds-of-thing-at-point 'word) symbol)
        (prot/expreg-expand 1))
       (symbol (prot/expreg-expand 2))
       (t (expreg-expand))))))

;; (use-package expand-region
;;   :bind ("C-=" . er/expand-region))

;; To add pairs: select something, then M-' s (
;; https://emacsredux.com/blog/2026/03/17/surround-el-vim-style-pair-editing-comes-to-emacs/
(use-package surround
  :bind-keymap ("M-'" . surround-keymap))

;; Add/Change/Delete pairs based on `expand-region', similar to vim-surround
;; (use-package embrace
;;   :defer 0.8
;;   :bind ("C-c z" . embrace-commander)
;;   :init
;;   (defun embrace-markdown-mode-hook ()
;;     (dolist (lst '((?* "*" . "*")
;;                    (?\ "\\" . "\\")
;;                    (?$ "$" . "$")
;;                    (?/ "/" . "/")))
;;       (embrace-add-pair (car lst) (cadr lst) (cddr lst))))
;;   :hook
;;   (org-mode . embrace-org-mode-hook)
;;   (emacs-lisp-mode . embrace-emacs-lisp-mode-hook)
;;   (markdown-mode . embrace-markdown-mode-hook))

;; Preserve the scratch buffer across Emacs sessions
;; also save and restore the major mode, see `persistent-scratch-what-to-save'
(use-package persistent-scratch
  ;; :defer 2
  :bind ( :map persistent-scratch-mode-map
          ;; TODO: use `kill-buffer-query-functions'
          ;; ([remap kill-buffer] . (lambda (&rest _)
          ;;                          (interactive)
          ;;                          (user-error "[xy] scratch buffer cannot be killed")))
          ([remap save-buffer] . persistent-scratch-save) ; C-x C-s
          ([remap write-file] . #'xy/persistent-scratch-save-to-backup) ; C-x C-w
          ("C-x M-s" . #'xy/persistent-scratch-save) ; to make multiple backup under current session
          ([remap revert-buffer] . persistent-scratch-restore) ; s-u
          ([remap revert-buffer-quick] . persistent-scratch-restore) ; C-x x g
          ([remap find-alternate-file] . #'xy/persistent-scratch-restore-from-backup)) ; C-x C-v
  :hook (lisp-interaction-mode)
  :config
  ;; To protect the scratch buffer against accidental kill
  ;; https://www.emacswiki.org/emacs/ProtectingBuffers
  (with-current-buffer "*scratch*"
    (emacs-lock-mode 'kill))

  (defun xy/persistent-scratch-save ()
    (interactive)
    (persistent-scratch-new-backup)
    (persistent-scratch-save))

  (defun xy/persistent-scratch-save-to-backup ()
    (interactive)
    (let ((default-directory persistent-scratch-backup-directory))
      (call-interactively #'persistent-scratch-save-to-file)))

  (defun xy/persistent-scratch-restore-from-backup ()
    (interactive)
    (let ((default-directory persistent-scratch-backup-directory))
      (call-interactively #'persistent-scratch-restore-from-file)))

  (setq persistent-scratch-backup-directory (concat xy/var-dir "scratch-backup/"))

  (persistent-scratch-autosave-mode +1))

;; Edit regions in separate buffers, like `org-edit-special'
(use-package edit-indirect
  :bind ("C-x n e" . edit-indirect-region))

;; Edit comment/string/docstring or code block inside them in separate buffers
;; also work in minibuffer, help-mode
(use-package separedit
  :bind ("C-c '" . separedit) ; @prefix Select major mode
  :config
  (setq separedit-preserve-string-indentation t)
  (setq separedit-continue-fill-column t)
  (setq separedit-write-file-when-execute-save t)
  (setq separedit-remove-trailing-spaces-in-comment t)
  ;;
  (setq separedit-default-mode 'markdown-mode))


;;; indent
;; Keeps your code always indented. It reindents after every change
;; e.g. shifting blocks around, transposing lines, or slurping and barfing sexps
;; (use-package aggressive-indent
;;   :defer 1
;;   :config (global-aggressive-indent-mode +1))

;; Automatic indentation (and optional formatting) when yanking/pasting text
;; (use-package snap-indent
;;   :hook prog-mode
;;   :config
;;   (setq snap-indent-format '(untabify delete-trailing-whitespace))
;;   (setq snap-indent-on-save nil)
;;   (setq snap-indent-skip-on-prefix-arg t))

;; Automatic indentation mode
(use-package indentinator
  ;; :hook prog-mode
  :bind ("C-c t i" . indentinator-mode))


;;; motion

;; Move point through `buffer-undo-list' positions.
;; (use-package goto-last-change
;;   :bind ("M-g SPC" . goto-last-change)
;;   :config
;;   (defvar-keymap xy/goto-last-change-repeat-map
;;     :repeat t
;;     "SPC" #'goto-last-change))

(use-package goto-chg
  :bind
  ("M-g ;" . goto-last-change)
  ("M-g ," . goto-last-change-reverse)
  :config
  (defvar-keymap xy/goto-chg-repeat-map
    :repeat t
    ";" #'goto-last-change
    "," #'goto-last-change-reverse))

;; Jump to visible text using a char-based decision tree
(use-package avy
  ;; :chords
  ;; ("jk" . avy-goto-char-timer)
  ;; ("jl" . avy-goto-line)
  :bind (("M-g ." . avy-resume)
         ("M-g j" . avy-goto-char)
         ("M-g M-j" . avy-goto-word-1)
         ("M-g l" . avy-goto-char-2)
         ("M-g M-l" . avy-goto-line)
         ("M-g /" . avy-goto-char-timer)
         :map isearch-mode-map
         ("M-s j" . avy-isearch)))

(use-package ace-pinyin
  :after avy :demand t
  :config
  (ace-pinyin-global-mode +1))

(use-package binky
  :init
  (defvar-keymap xy/binky-repeat-map
    :repeat t
    "]" #'binky-next-in-buffer
    "[" #'binky-previous-in-buffer)
  :bind
  ("M-g '" . binky-binky)
  ("M-g ]" . binky-next-in-buffer)
  ("M-g [" . binky-previous-in-buffer)
  :config
  (binky-mode +1)
  (binky-margin-mode +1)
  (setq  binky-preview-delay 0.2)
  ;; (setq binky-indicator-side 'right)
  ;; Use mark itself as indicator
  (setq binky-margin-string nil)
  ;; Save and restore
  (binky-restore)
  (add-hook 'kill-emacs-hook #'binky-save))


;;; struct edit

;; (use-package paredit
;;   :hook (lisp-data-mode eval-expression-minibuffer-setup)
;;   :bind (("C-c t p" . paredit-mode)
;;          :map paredit-mode-map
;;          ("M-s" . nil)          ;; `paredit-splice-sexp'
;;          ("M-r" . nil)          ;; `paredit-raise-sexp'
;;          ("M-<up>" . nil)       ;; `paredit-splice-sexp-killing-backward'
;;          ("M-<down>" . nil)     ;; `paredit-splice-sexp-killing-forward'
;;          ("C-<right>" . nil)    ;; `paredit-forward-slurp-sexp'
;;          ("C-<left>" . nil)     ;; `paredit-forward-barf-sexp'
;;          ("C-M-<left>" . nil)   ;; `paredit-backward-slurp-sexp'
;;          ("C-M-<right>" . nil)  ;; `paredit-backward-barf-sexp'
;;          ;; ("M-;" . nil) ;; `paredit-comment-dwim'
;;          ;; ("C-j" . nil) ;; `paredit-C-j'
;;          ("M-L" . paredit-splice-sexp) ;; @prefix `paredit-splice-sexp-killing-backward', double @prefix `paredit-splice-sexp-killing-forward'
;;          ("M-R" . paredit-raise-sexp)
;;          ("M-N" . paredit-forward-slurp-sexp)
;;          ("M-P" . paredit-forward-barf-sexp)
;;          ("M-U" . paredit-backward-slurp-sexp)
;;          ("M-D" . paredit-backward-barf-sexp))
;;   :config
;;   ;; (electric-indent-mode -1)
;;   ;; ElDoc can safely print docstring after these commands
;;   (eldoc-add-command
;;    'paredit-backward-delete
;;    'paredit-close-round
;;    'paredit-close-square
;;    'paredit-close-curly))

;; Automatic insertion, wrapping and paredit-like navigation with user defined pairs
;; -- Handles anything that pairs, not only parentheses
;; -- Combination of autopair, textmate, wrap-region, electric-pair-mode, paredit
;; (use-package smartparens
;;   :defer 0.5
;;   :bind (("C-c t s" . smartparens-mode)
;;          ("C-c t S" . smartparens-strict-mode)
;;          ("C-h o S" . sp-cheat-sheet)
;;          :map smartparens-mode-map
;;          ;; NOTE: from `sp-paredit-bindings'
;;          ("C-M-f" . sp-forward-sexp) ;; navigation
;;          ("C-M-b" . sp-backward-sexp)
;;          ("C-M-u" . sp-backward-up-sexp)
;;          ("C-M-d" . sp-down-sexp)
;;          ("C-M-p" . sp-backward-down-sexp)
;;          ("C-M-n" . sp-up-sexp)
;;          ("M-L" . sp-splice-sexp) ;; depth-changing commands
;;          ("M-R" . sp-raise-sexp)
;;          ("M-(" . sp-wrap-round)
;;          ("M-N" . sp-forward-slurp-sexp) ;; barf/slurp
;;          ("M-P" . sp-forward-barf-sexp)
;;          ("M-U" . sp-backward-slurp-sexp)
;;          ("M-D" . sp-backward-barf-sexp)
;;          ("M-S" . sp-split-sexp) ;; misc
;;          ("M-J" . sp-join-sexp)
;;          ("M-?" . sp-convolute-sexp)
;;          ;; NOTE: from `sp-smartparens-bindings'
;;          ("M-F" . sp-forward-symbol)
;;          ("M-B" . sp-backward-symbol)
;;          ("M-I" . sp-change-inner)
;;          ("M-A" . sp-change-enclosing)
;;          ;; ("C-M-a" . sp-backward-down-sexp)
;;          ;; ("C-M-e" . sp-up-sexp)
;;          ("C-S-a" . sp-beginning-of-sexp)
;;          ("C-S-e" . sp-end-of-sexp)
;;          ("C-S-n" . sp-next-sexp)
;;          ("C-S-p" . sp-previous-sexp)
;;          ("C-M-k" . sp-kill-sexp)
;;          ("C-M-t" . sp-transpose-sexp)
;;          ;; ("C-M-w" . sp-copy-sexp)
;;          ;; ("M-<delete>" . sp-unwrap-sexp)
;;          ;; ("M-<backspace>" . sp-backward-unwrap-sexp)
;;          ;; ("C-M-SPC" . sp-mark-sexp)
;;          ("C-]" . sp-select-next-thing-exchange)
;;          ("C-M-]" . sp-select-next-thing))
;;   :config
;;   (electric-pair-mode -1)
;;   (show-paren-mode -1)
;;   ;;
;;   (require 'smartparens-config)
;;   (smartparens-global-mode +1)
;;   (add-hook 'term-mode-hook #'turn-off-smartparens-mode)
;;   (dolist (hook '(prog-mode-hook markdown-mode eval-expression-minibuffer-setup-hook))
;;     (add-hook hook #'smartparens-strict-mode))
;;   (show-smartparens-global-mode +1))

;; Structured editing (soft deletion, expression navigating & manipulating)
(use-package puni
  ;; :defer 0.5
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook eval-expression-minibuffer-setup-hook))
    (add-hook hook #'puni-mode))

  (defun xy/puni-splice (arg)
    (interactive "p")
    (pcase arg
      (4 (puni-splice-killing-backward))
      (16 (puni-splice-killing-forward))
      (_ (puni-splice))))

  (defvar-keymap xy/puni-repeat-map
    :repeat t
    "m" #'puni-expand-region
    "M" #'puni-contract-region)
  :bind (("C-c t p" . puni-mode)
         :map puni-mode-map
         ("C-w" . nil)
         ;; For deleting the char before point, no matter they are balanced or not
         ;; "C-c DEL" (`puni-force-delete') or "C-u DEL"
         ("M-(" . nil)
         ("M-)" . nil)
         ("C-M-a" . nil)
         ("C-M-e" . nil)
         ("C-M-u" . puni-syntactic-backward-punct)
         ("C-M-d" . puni-syntactic-forward-punct)
         ("C-M-p" . puni-beginning-of-sexp)
         ("C-M-n" . puni-end-of-sexp)
         ;;
         ("C-M-m" . puni-expand-region)
         ("C-M-z" . puni-squeeze)
         ("C-M-t" . puni-transpose)
         ;;
         ("M-N" . puni-slurp-forward)
         ("M-P" . puni-barf-forward)
         ("M-U" . puni-slurp-backward)
         ("M-D" . puni-barf-backward)
         ("M-L" . #'xy/puni-splice)
         ("M-R" . puni-raise)
         ("M-S" . puni-split)
         ;;
         ("M-(" . puni-wrap-round)
         ("C-(" . puni-wrap-square)
         ("C-M-(" . puni-wrap-curly))
  :config
  ;; NOTE: global enable will override DEL/M-DEL in minibuffer
  ;; (puni-global-mode +1)
  ;; (add-hook 'term-mode-hook #'puni-disable-puni-mode)
  (setq puni-blink-pulse-delay 0.1))

;; (use-package awesome-pair
;;   :vc ( :url "https://github.com/manateelazycat/awesome-pair"
;;         :rev :newest)
;;   :hook (lisp-data-mode eval-expression-minibuffer-setup)
;;   :bind ( :map awesome-pair-mode-map
;;           ("(" . awesome-pair-open-round)
;;           ("[" . awesome-pair-open-bracket)
;;           ("{" . awesome-pair-open-curly)
;;           (")" . awesome-pair-close-round)
;;           ("]" . awesome-pair-close-bracket)
;;           ("}" . awesome-pair-close-curly)
;;           ("=" . awesome-pair-equal)
;;           ;;
;;           ("<tab>" . awesome-pair-match-paren)
;;           ("\"" . awesome-pair-double-quote)
;;           ;;
;;           ("SPC" . awesome-pair-space)
;;           ("RET" . awesome-pair-newline)
;;           ;;
;;           ("C-d" . awesome-pair-forward-delete)
;;           ("C-k" . awesome-pair-kill)
;;           ;;
;;           ("M-\"" . awesome-pair-wrap-double-quote)
;;           ("M-[" . awesome-pair-wrap-bracket)
;;           ("M-{" . awesome-pair-wrap-curly)
;;           ("M-(" . awesome-pair-wrap-round)
;;           ("C-(" . awesome-pair-unwrap)
;;           ;;
;;           ("M-n" . awesome-pair-jump-right)
;;           ("M-p" . awesome-pair-jump-left)
;;           ("M-)" . awesome-pair-jump-out-pair-and-newline))
;;   :config
;;   (electric-indent-mode -1)
;;   (electric-pair-mode -1))

;; TODO: https://github.com/manateelazycat/fingertip


;;; util

;; TODO:
;; https://www.emacswiki.org/emacs/AbbrevMode
;; https://www.emacswiki.org/emacs/HippieExpand
;; @see (info "(emacs) Dynamic Abbrevs")
(use-core dabbrev
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

(use-core view
  :init
  ;; (setq view-read-only t)

  (defconst xy/elpa-lisp-dir package-user-dir)
  (defconst xy/emacs-lisp-dir (file-name-directory (directory-file-name doc-directory)))

  ;; (info "(emacs) Directory Variables")
  ;; https://www.emacswiki.org/emacs/DirectoryVariables
  ;; https://www.reddit.com/r/emacs/comments/cr91vw/tip_use_dirlocalsel_to_make_entire_projects_on/
  (dir-locals-set-class-variables
   :read-only
   '((nil . ((buffer-read-only . t)
             (mode . view)))))
  (dir-locals-set-directory-class xy/elpa-lisp-dir :read-only)
  (dir-locals-set-directory-class xy/emacs-lisp-dir :read-only)
  :bind
  (("C-x x v" . #'view-mode)
   :map view-mode-map
   ;; No exit view-mode or kill buffer
   ("q" . #'switch-to-prev-buffer))
  :config
  (keymap-set ctl-x-4-map "V" #'view-file-other-window)
  (keymap-set ctl-x-5-map "V" #'view-file-other-frame))

(use-core editorconfig
  :defer 0.4
  :config (editorconfig-mode +1))

;; https://github.com/rolandwalker/back-button
;; TODO: clean legacy code, and put it under site-lisp

;; https://www.emacswiki.org/emacs/VisibleMark
(use-package visible-mark
  :defer 0.5
  :config
  (global-visible-mark-mode +1)
  (setq visible-mark-max 2)
  (setq visible-mark-faces `(visible-mark-face1 visible-mark-face2)))

;; https://www.emacswiki.org/emacs/AutoMark
;; (use-package auto-mark
;;   :ensure nil ; site-lisp
;;   :defer 0.5
;;   :config
;;   (setq auto-mark-command-class-alist
;;         '((anything . anything)
;;           (goto-line . jump)
;;           (indent-for-tab-command . ignore)
;;           (undo . ignore)))
;;   (setq auto-mark-command-classifiers
;;         (list (lambda (command)
;;                 (if (and (eq command 'self-insert-command)
;;                          (eq last-command-event ? ))
;;                     'ignore))))
;;   (global-auto-mark-mode +1))

;; Make hypertext with active links in any buffer
(use-package linkd
  :vc ( :url "https://github.com/emacsorphanage/linkd"
        :rev :newest)
  :commands linkd-mode
  :config
  (setq linkd-use-icons t))

;; (use-package indent-hints
;;   :ensure nil ; site-lisp
;;   :hook prog-mode
;;   :config
;;   (setq indent-hints-profile-switching-enabled t))

(use-core pixel-scroll
  :defer 1
  :config
  (pixel-scroll-precision-mode +1)
  (setq pixel-scroll-precision-interpolate-page t)
  ;; @see `mouse-wheel-scroll-amount'
  ;; Disable `mouse-wheel-text-scale' by `mouse-wheel-mode' when use `pixel-scroll-precision-mode'
  (keymap-global-unset "C-<wheel-down>")
  (keymap-global-unset "C-<wheel-up>"))

;; Faster and can handle tall image scrolling
;; @tip `ultra-scroll-push-mark'
(use-package ultra-scroll
  :defer 1
  :init
  ;; NOTE: scroll-margin > 0 not yet supported
  (setq scroll-margin 0)
  :config
  (ultra-scroll-mode +1))

;; Wrapping visual-line-mode buffers at fill-column
(use-package visual-fill-column
  :bind (("C-x y c" . visual-fill-column-mode)
         ("C-x y C" . visual-fill-column-toggle-center-text))
  :hook markdown-mode
  :init
  (add-hook 'visual-line-mode-hook #'visual-fill-column-for-vline)
  :config
  ;; To not hide `display-fill-column-indicator'
  ;; @see https://codeberg.org/joostkremers/visual-fill-column/issues/14
  (add-hook 'visual-fill-column-mode-hook
            (lambda () (setq-local visual-fill-column-fringes-outside-margins nil)))
  (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)
  (setq visual-fill-column-enable-sensible-window-split t))

;; Show major mode heirarchy
(use-package mode-minder
  :vc ( :url "https://github.com/jdtsmith/mode-minder"
        :rev :newest)
  :commands mode-minder)

;; A word cloud of the current buffer
(use-package wordcloud
  :vc ( :url "https://github.com/davep/wordcloud.el"
        :rev :newest)
  :commands woldcloud)

;; Hide comments if code is obvious
(use-package obvious
  :vc ( :url "https://github.com/alphapapa/obvious.el"
        :rev :newest)
  :bind ("C-c t c" . obvious-mode))

(provide 'init-edit)
