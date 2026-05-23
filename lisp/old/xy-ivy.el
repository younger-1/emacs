;; @see https://github.com/abo-abo/swiper/blob/master/doc/ivy.org
;; Ivy, a generic completion mechanism for Emacs.
(use-package ivy
  :defer 0.2
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

;; Swiper, an Ivy-enhanced alternative to Isearch.
(use-package swiper
  :defer 0.2
  :config
  ;; (keymap-global-set "C-s" #'swiper)
  (keymap-global-set "C-s" #'swiper-isearch))

;; Counsel, a collection of Ivy-enhanced versions of common Emacs commands.
;; - Symbol completion for Elisp, Common Lisp, Python, Clojure, C, C++.
;; - Describe functions for Elisp: function, variable, library, command, bindings, theme.
;; - Navigation functions: imenu, ace-line, semantic, outline.
;; - Git utilities: git-files, git-grep, git-log, git-stash, git-checkout.
;; - Grep utilities: grep, ag, pt, recoll, ack, rg.
;; - System utilities: process list, rhythmbox, linux-app.
(use-package counsel
  :defer 0.2
  :config
  (keymap-global-set "C-x b" #'ivy-switch-buffer)
  (keymap-global-set "C-c v" #'ivy-push-view)
  (keymap-global-set "C-c V" #'ivy-pop-view)
  (keymap-global-set "C-c C-r" #'ivy-resume)

  ;; Remap some global key binding, see `counsel-mode-map'
  ;; (counsel-mode)
  ;;
  ;; Ivy-based interface to standard commands
  (keymap-global-set "M-x" #'counsel-M-x)
  (keymap-global-set "C-x C-f" #'counsel-find-file)
  (keymap-global-set "M-y" #'counsel-yank-pop)
  (keymap-global-set "<f1> f" #'counsel-describe-function)
  (keymap-global-set "<f1> v" #'counsel-describe-variable)
  (keymap-global-set "<f1> o" #'counsel-describe-symbol)
  (keymap-global-set "<f1> l" #'counsel-find-library)
  (keymap-global-set "<f2> i" #'counsel-info-lookup-symbol)
  (keymap-global-set "<f2> u" #'counsel-unicode-char)
  (keymap-global-set "<f2> j" #'counsel-set-variable)
  ;;
  (keymap-global-set "C-c b" #'counsel-bookmark)
  (keymap-global-set "C-c d" #'counsel-descbinds)
  (keymap-global-set "C-c o" #'counsel-outline)
  (keymap-global-set "C-c t" #'counsel-load-theme)
  (keymap-global-set "C-c F" #'counsel-org-file)

  ;; Ivy-based interface to shell and system tools
  (keymap-global-set "C-c c" #'counsel-compile)
  (keymap-global-set "C-c g" #'counsel-git)
  (keymap-global-set "C-c j" #'counsel-git-grep)
  (keymap-global-set "C-c L" #'counsel-git-log)
  (keymap-global-set "C-c k" #'counsel-rg)
  (keymap-global-set "C-c m" #'counsel-linux-app)
  (keymap-global-set "C-c n" #'counsel-fzf)
  (keymap-global-set "C-x l" #'counsel-locate)
  (keymap-global-set "C-c J" #'counsel-file-jump)
  (keymap-global-set "C-S-o" #'counsel-rhythmbox)
  (keymap-global-set "C-c w" #'counsel-wmctrl))
