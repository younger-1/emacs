;; -*- lexical-binding: t -*-

(require 'init-package)

;;; imenu
(use-core imenu
  :init
  ;; imenu support for `use-core', `use-feature', `with-eval-after-load'
  (with-eval-after-load 'lisp-mode
    (dolist (pattern '(("Options" "^\\s-*(defcustom\\s-+\\(.+\\)" 1)
                       ("Packages" "^\\s-*(\\(?:use-package\\|use-feature\\)\\s-+\\(.+\\)" 1)
                       ("Builtin Packages" "^(use-core \\(.+\\)$" 1)
                       ("Libraries" "^\\s-*(\\(?:require\\|load\\|\\(?:with-\\)?eval-after-load\\)\\s-+\\([^() ]+\\)" 1)
                       ("Sections" "^;;; \\(.+\\)$" 1)))
      (add-to-list 'lisp-imenu-generic-expression pattern)))
  :config
  ;; (setq imenu-flatten 'group)
  (setq imenu-flatten 'prefix
        imenu-level-separator "::")

  ;; Add an Imenu "Index" entry on the menu bar
  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook magit-status-mode-hook))
    (add-hook hook #'imenu-add-menubar-index)))

(use-package imenu-list
  :bind
  ("C-x d i" . imenu-list-smart-toggle)
  :config
  ;; (setq imenu-list-position 'left)
  (setq imenu-list-focus-after-activation t)
  (setq imenu-list-auto-resize t))


;;; eldoc
;; (info "(emacs) Programming Language Doc")
(use-core eldoc
  :init
  ;; (global-eldoc-mode +1) ;; @default
  ;; :hook
  ;; (prog-mode . eldoc-mode)
  :bind ("C-h ." . #'eldoc-doc-buffer)
  :config
  (setq eldoc-idle-delay 0.3)
  ;; (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  (setq eldoc-minor-mode-string nil)

  ;; https://emacs-china.org/t/elisp-eldoc/7571
  ;; Eldoc 对函数默认只显示参数列表，让其显示函数文档
  (define-advice elisp-get-fnsym-args-string (:around (orig-fun sym &rest r) docstring)
    "If SYM is a function, append its docstring."
    (concat
     (apply orig-fun sym r)
     (let* ((doc (and (fboundp sym) (documentation sym 'raw)))
            (oneline (and doc (substring doc 0 (string-match "\n" doc)))))
       (and oneline
            (concat "  |  " (propertize oneline 'face 'italic))))))
  )

(use-package eldoc-box
  :after eldoc
  :defer 0.5
  :bind (("C-h /" . eldoc-box-help-at-point)
         ("C-h ]" . eldoc-box-hover-mode)
         ("C-h [" . eldoc-box-hover-at-point-mode))
  :hook
  (eldoc-mode . eldoc-box-hover-mode)
  ;; (eglot-managed-mode . eldoc-box-hover-mode)
  ;;
  ;; (eldoc-mode . eldoc-box-hover-at-point-mode)
  (eglot-managed-mode . eldoc-box-hover-at-point-mode)
  :config
  (setq eldoc-box-clear-with-C-g t))

;; (use-package eldoc-mouse
;;   :hook (eglot-managed-mode)
;;   :bind ("C-h ;" . eldoc-mouse-pop-doc-at-cursor))


;;; xref
;; @tip
;; M-. / M-, -> `xref-find-definitions' / `xref-go-back'
;; C-M-. / C-M-, -> `xref-find-apropos' / `xref-go-forward'
;; M-? -> `xref-find-references'
(use-core xref
  :config
  (when (executable-find "rg")
    (setq xref-search-program 'ripgrep))
  ;; Use completion system instead of popup window.
  (setq xref-show-definitions-function 'xref-show-definitions-completing-read
        xref-show-xrefs-function 'xref-show-definitions-completing-read)
  (setq xref-history-storage 'xref-window-local-history))


;;; flymake
(use-core flymake
  :init
  ;; TODO as macro or use-package keyword
  (add-hook 'emacs-lisp-mode-hook
            (defun xy/defer-enable-flymake ()
              (run-with-idle-timer 2.0 nil #'flymake-mode)))
  :bind (("C-c j m" . flymake-mode)
         :map flymake-mode-map
         ("C-c j n" . flymake-goto-next-error)
         ("C-c j p" . flymake-goto-prev-error)
         ("C-c j e" . flymake-show-buffer-diagnostics)
         ("C-c j E" . flymake-show-project-diagnostics))
  :config
  ;; (remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake)
  ;; In-buffer display
  ;; (setq flymake-show-diagnostics-at-end-of-line 'fancy) ; TODO: emacs31
  (setq flymake-show-diagnostics-at-end-of-line 'short)
  ;; Indicator display
  ;; (setq flymake-indicator-type 'margins)
  (setq flymake-fringe-indicator-position 'right-fringe)
  (setq flymake-margin-indicator-position 'right-margin))

(use-package flymake-collection
  :after flymake :demand t
  :bind ("C-c j c" . flymake-collection-change-checker)
  :config
  (flymake-collection-hook-setup)
  ;; TODO: backend for all major-mode
  ;; (add-to-list 'flymake-collection-hook-config
  ;;              '(t . ((hl-todo-flymake :disabled t))))
  (add-to-list 'flymake-collection-hook-config
               '(emacs-lisp-mode . ((elisp-flymake-checkdoc :disabled t)))))

;; (use-package flymake-diagnostic-at-point
;;   :hook (flymake-mode . flymake-diagnostic-at-point-mode))

;; Display Flycheck and Flymake errors with overlays
;; (use-package flyover
;;   :hook (flymake-mode))

;; (use-package flycheck
;;   :defer 1
;;   :config
;;   (global-flycheck-mode +1))


;;; jump
(use-package dumb-jump
  :defer 1
  :config
  ;; @see `dumb-jump-find-rules'
  (setq dumb-jump-prefer-searcher 'rg)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))


;;; outline

;; Why code folding, @see https://www.jamescherti.com/emacs-the-definitive-guide-to-code-folding/
;; - Reading: manage cognitive load, preserve spatial memory, save screen real estate
;; - Navigating: reveal only a specific entry and its parents for seeing hierarchy without losing position
;; - Debugging: reduce visual noise, make hostile codebases readable
;; - Moving: delete, cut, copy, or move a massive function or block safely and cleanly
;; - Reviewing: fold previously examined functions or blocks

;; Emacs' oldest built-in folding method
;; C-x $ -> `set-selective-display'

;; Indentation-based folding -
;; https://www.reddit.com/r/emacs/comments/1stnc6q/the_definitive_guide_to_code_folding_in_emacs/
(defun xy/set-selective-display ()
  "Toggle fold all lines with indentation larger than the point column"
  (interactive)
  (if selective-display
      (set-selective-display nil)
    (set-selective-display (+ (current-column) 1))))
(keymap-global-set "C-x $" #'xy/set-selective-display)

(defun xy/toggle-fold ()
  "Toggle fold all lines with indentation larger than current line"
  (interactive)
  (set-selective-display (if selective-display
                             nil
                           (or (save-excursion (back-to-indentation) (+ 1 (current-column))) 1))))
(keymap-global-set "C-x %" #'xy/toggle-fold)

;; Reveal hidden text at point, and re-hiding them when you navigate away
(use-core reveal
  :hook (hs-minor-mode outline-minor-mode)
  :bind ("C-c t r" . reveal-mode))

;; Keymap and mouse support to fold(narrow) in subtree
;; C-z -> `foldout-zoom-subtree'
;; C-x -> `foldout-exit-fold'
(use-core foldout
  :after outline :demand t)

;; TAB(`outline-cycle'): cycles the current section
;; between “hide all”, “subheadings”, and “show all”
;; - `outline-hide-subtree'
;; - `outline-show-children'
;; - `outline-show-subtree'
;; S-TAB(`outline-cycle-buffer'): cycles the whole buffer
;; between “only top-level headings”, “all headings and subheadings”, and “show all”
;; - `outline-hide-sublevels' (top-level)
;; - `outline-hide-region-body'
;; - `outline-show-all'
;; C-q `outline-hide-sublevels': @prefix Only top n (default 1) headers visible
;; C-t `outline-hide-body': Hide all body lines in buffer, leaving all headings visible.
(use-core outline
  ;; :hook
  ;; (emacs-lisp-mode . outline-minor-mode)
  :bind (("C-c t o" . outline-minor-mode)
         :map outline-minor-mode-map
         ("C-c v SPC" . #'outline-back-to-head))
  :init
  (defun outline-back-to-head () (interactive) (outline-back-to-heading))
  :config
  ;; @tip TAB/S-TAB on the heading line
  (setq outline-minor-mode-cycle t)
  ;; @tip RET/S-RET at start of heading
  (keymap-set outline-overlay-button-map "S-<return>" #'outline-cycle-buffer)
  (setopt outline-minor-mode-prefix (kbd "C-c v")) ; v for view
  ;;; UI
  (setq outline-minor-mode-highlight 'append)
  ;; Click left margin with mouse-1/S-mouse-1. see `outline-minor-mode-cycle-map'
  (setq outline-minor-mode-use-buttons 'in-margins)
  (setq outline-blank-line t))

;; Comment-based outline folding, using `outline'
(use-package outli
  :hook (prog-mode text-mode conf-mode)
  :config
  ;; Remove overline on headings
  ;; (setf (alist-get 'emacs-lisp-mode outli-heading-config) '(";;" ?\; t t))
  )

;; Comment-based outline folding, using `outline'
;; (use-package outline-stars
;;   :vc ( :url "https://codeberg.org/phmcc/outline-stars"
;;         :rev :newest)
;;   :init (outline-stars-mode +1))

;; Syntax-aware folding, for C-style languages and others that use braces {}
(use-core hideshow
  :init (defalias 'hideshow-mode 'hs-minor-mode)
  :hook (prog-mode) ; TODO remove ts-based major mode
  :bind (("C-c t h" . hs-minor-mode)
         :map hs-minor-mode-map
         ("C-<return>" . hs-toggle-hiding)
         ("S-<return>" . hs-hide-all)
         ("C-S-<return>" . hs-show-all)))

;; Indentation-based folding
;; (use-package yafolding
;;   :hook prog-mode
;;   :bind (:map yafolding-mode-map))

;; Parser-based structural folding with indentation fallback
;; (use-package origami
;;   :hook prog-mode
;;   :bind ( :map origami-mode-map
;;           ("C-<return>" . origami-toggle-node)
;;           ("S-<return>" . origami-toggle-all-nodes)
;;           ("C-S-<return>" . origami-show-only-node)
;;           ("C-c v u" . origami-undo)
;;           ("C-c v r" . origami-redo)))

;; Indentation-based folding, for Python, Haskell, and YAML
(use-package outline-indent
  :init (defalias 'outline-indent-mode 'outline-indent-minor-mode)
  :hook (python-mode yaml-mode haskell-mode)
  ;; (outline-indent-minor-mode-hook . outline-indent-close-folds)
  :config
  (setq outline-indent-ellipsis " ▼"))

;; Tree-sitter-based folding, using `treesit'
(use-package treesit-fold
  ;; :defer 1
  :bind ( :map treesit-fold-mode-map
          ("C-<return>" . treesit-fold-toggle)
          ("S-<return>" . treesit-fold-close-all)
          ("C-S-<return>" . treesit-fold-open-all))
  :hook
  ;; Add support for non-ts modes
  (go-mode . (lambda () (treesit-parser-create 'go)))
  ;; (emacs-lisp-mode. (lambda () (treesit-parser-create 'elisp)))
  :config
  (setq treesit-fold-line-count-show t)
  ;; (setq treesit-fold-line-count-format " %d ▼")
  (global-treesit-fold-mode +1)
  (global-treesit-fold-indicators-mode +1))

;; Arbitrary region folding into a one-line summary in any buffer
;; for inspecting long log files, dealing with LLM output
(use-package occult
  :bind ; @tip Press e to open the fold at point in a narrowed indirect buffer for editing
  ("C-c z z" . occult-toggle)
  ("C-c z a" . occult-reveal-all))

;; A Unified Method to Fold and Unfold Text
;; `outline-minor-mode' relies on hierarchical headings
;; - emacs-lisp-mode conf-mode
;; - org-mode markdown-mode gfm-mode
;; `hs-minor-mode' parses buffer syntax, for legacy non-treesit mode
;; - c-mode c++-mode java-mode rust-mode go-mode ruby-mode lua-mode
;; - js-mode typescript-mode css-mode json-mode sh-mode
;; `outline-indent-minor-mode' for indent-sensitive lang
;; - python-mode yaml-mode haskell-mode
;; `treesit-fold-mode' for treesit mode
(use-package kirigami
  :init
  (global-set-key (kbd "C-c z o") 'kirigami-open-fold)
  (global-set-key (kbd "C-c z O") 'kirigami-open-fold-rec)
  (global-set-key (kbd "C-c z c") 'kirigami-close-fold)
  (global-set-key (kbd "C-c z a") 'kirigami-toggle-fold)
  (global-set-key (kbd "C-c z r") 'kirigami-open-folds)
  (global-set-key (kbd "C-c z m") 'kirigami-close-folds)
  ;;
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map "zo" 'kirigami-open-fold)
    (define-key evil-normal-state-map "zO" 'kirigami-open-fold-rec)
    (define-key evil-normal-state-map "zc" 'kirigami-close-fold)
    (define-key evil-normal-state-map "za" 'kirigami-toggle-fold)
    (define-key evil-normal-state-map "zr" 'kirigami-open-folds)
    (define-key evil-normal-state-map "zm" 'kirigami-close-folds))
  :config
  (setq kirigami-preserve-visual-position t))

;; (use-package savefold
;;   :defer 0.5
;;   :config
;;   (setq savefold-backends '(outline org hideshow markdown))
;;   (savefold-mode +1))


;;; lang

;; https://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do))
  :config
  ;; Markdown processor: not required for editing, for rendering HTML for preview and export.
  (setq markdown-command "pandoc"))

(use-core python
  :config
  ;; Remove guess indent python message
  (setq python-indent-guess-indent-offset-verbose nil))


;;; lisp
;; `lisp-data-mode' is the parent of `emacs-lisp-mode' and `lisp-mode'
;; `lisp-mode-shared-map' is the parent of `emacs-lisp-mode-map' and `lisp-mode-map'
(use-core lisp-mode
  :init
  (defun xy/check-parens-before-save ()
    (add-hook 'before-save-hook #'check-parens 0 :local))
  (add-hook 'lisp-mode-hook #'xy/check-parens-before-save)
  (add-hook 'emacs-lisp-mode-hook #'xy/check-parens-before-save)
  (defun xy/untabify-indent-buffer (beg end)
    "Untabify and Indent the entire buffer without affecting point or mark."
    (interactive "r")
    (unless (region-active-p)
      (setq beg (point-min))
      (setq end (point-max)))
    (let ((mark-even-if-inactive t))
      (unless indent-tabs-mode
        (untabify beg end))
      (save-excursion
        (save-restriction
          (indent-region beg end)))))
  (defun xy/backward-symbol (arg)
    (interactive "^p")
    (when  (numberp arg)
      (forward-symbol (- arg))))
  :bind (("C-c e e" . #'pp-eval-last-sexp)
         ("C-c e p" . #'pp-eval-expression)
         ("C-c e j" . #'eval-print-last-sexp)
         ("C-c e f" . #'eval-defun)
         ("C-c e b" . #'eval-buffer)
         ("C-c e r" . #'eval-region)
         ;;
         ("C-c e d" . #'debug-on-entry)
         ("C-c e u" . #'cancel-debug-on-entry)
         ("C-c e D" . #'edebug-on-entry)
         ;; ("C-c e D" . #'edebug-defun)
         ;;
         ("C-c e c" . #'check-parens)
         ("C-c e i" . #'xy/untabify-indent-buffer)
         ;; :map lisp-mode-shared-map
         ("M-F" . #'forward-symbol)
         ("M-B" . #'xy/backward-symbol))
  :config
  ;; 󰊾 󰕅 󰈍  󰉡 󰝖 󱡠 󰷐      󰓷     󰗁 󱗛  󰈸    󰍐 󰟙 󰍒  󰙑 󰘨 󰌕 󱍵 󰫍 󰕳    󰌱 󱇚 󰜅     
  ;; (setq lisp-prettify-symbols-alist nil)
  (add-to-list 'lisp-prettify-symbols-alist '("defun" . ?󰡱))
  (add-to-list 'lisp-prettify-symbols-alist '("defmacro" . ?))
  (add-to-list 'lisp-prettify-symbols-alist '("defvar" . ?󰓏))
  (add-to-list 'lisp-prettify-symbols-alist '("defconst" . ?󰀚))
  (add-to-list 'lisp-prettify-symbols-alist '("defcustom" . ?))
  (add-to-list 'lisp-prettify-symbols-alist '("defface" . ?))
  ;; (add-to-list 'lisp-prettify-symbols-alist '("setq" . ?))
  ;; (add-to-list 'lisp-prettify-symbols-alist '("setopt" . ?))
  ;; (add-to-list 'lisp-prettify-symbols-alist '("if" . ?󰞀))
  ;; (add-to-list 'lisp-prettify-symbols-alist '("when" . ? ))
  ;; (add-to-list 'lisp-prettify-symbols-alist '("unless" . ? ))
  ;; (add-to-list 'lisp-prettify-symbols-alist '("add-hook" . ?󰛢))
  ;; (add-to-list 'lisp-prettify-symbols-alist '("add-to-list" . ?󰾹))
  ;; (add-to-list 'lisp-prettify-symbols-alist '("push" . ?󰕕))
  ;; (add-to-list 'lisp-prettify-symbols-alist '("load" . ?))
  ;; (add-to-list 'lisp-prettify-symbols-alist '("require" . ?))
  (add-to-list 'lisp-prettify-symbols-alist '("use-core" . ?󰀘))
  (add-to-list 'lisp-prettify-symbols-alist '("use-feature" . ?))
  (add-to-list 'lisp-prettify-symbols-alist '("use-package" . ?)))

(use-core elisp-mode
  :bind ( :map emacs-lisp-mode-map
          ("C-c C-d" . #'byte-recompile-directory)))

(use-package macrostep
  :bind ( :map lisp-mode-shared-map
          ("C-c e m" . macrostep-expand)))

;; Better `xref-find-definitions', understands local bindings and parameters.
(use-package elisp-def
  :hook (emacs-lisp-mode ielm-mode))

;; Better `xref-find-references' (which based on a text search), understands comments and strings, and can distinguish between functions and variables.
;; @prefix Limit search results to specific directories
(use-package elisp-refs
  ;; :hook (emacs-lisp-mode ielm-mode)
  :bind
  ("C-h z f" . #'elisp-refs-function)
  ("C-h z m" . #'elisp-refs-macro)
  ("C-h z v" . #'elisp-refs-variable)
  ("C-h z o" . #'elisp-refs-symbol)
  ("C-h z s" . #'elisp-refs-special))

;; Evaluation Result OverlayS for Emacs Lisp.
;; (use-package eros
;;   ;; :bind
;;   ;; ("C-c e r" . #'eros-eval-last-sexp)
;;   ;; ("C-c e R" . #'eros-eval-defun)
;;   :defer 1
;;   :config
;;   (eros-mode +1))


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

;; (use-package go-mode
;;   ;; :bind (:map go-mode-map
;;   ;;             ("\C-c \C-c" . compile)
;;   ;;             ("\C-c \C-g" . go-goto-imports)
;;   ;;             ("\C-c \C-k" . godoc)
;;   ;;             ("M-j" . godef-jump))
;;   :config
;;   ;; goimports updates your Go import lines, adding missing ones and removing unreferenced ones
;;   ;; it also formats your code in the same style as gofmt so it can be used as a replacement for your editor's gofmt-on-save hook
;;   (unless (executable-find "goimports")
;;     (xy/install-go-tool "golang.org/x/tools/cmd/goimports"))
;;   (setq gofmt-command "goimports")
;;
;;   (add-hook 'go-mode-hook (lambda ()
;;                             (setq-local tab-width 4)
;;                             (add-hook 'before-save-hook #'gofmt-before-save nil t))))

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


;;; treesit
;; @see doc of `treesit-major-mode-setup'
(use-core treesit
  :bind (("C-h o i" . treesit-inspect-mode)
         ("C-h o e" . treesit-explore-mode))
  :config
  (setq treesit-font-lock-level 4))

;; TODO: Why not run go-mode-hook when it has (derived-mode-add-parents 'go-ts-mode '(go-mode))
;; @see https://magnus.therning.org/2023-11-16-using-the-golang-mode-shipped-with-emacs.html
;; (use-core go-ts-mode
;;   :init
;;   ;; Remapping major mode
;;   (add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))
;;   ;; (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
;;   ;; (add-to-list 'auto-mode-alist '("/go\\.mod\\'" . go-mod-ts-mode))
;;   ;; :mode (("\\.go\\'" . go-ts-mode)
;;   ;;        ("/go\\.mod\\'" . go-mod-ts-mode))
;;   :config
;;   ;; -- 1.
;;   (add-to-list 'treesit-language-source-alist '(go "https://github.com/tree-sitter/tree-sitter-go"))
;;   (add-to-list 'treesit-language-source-alist '(gomod "https://github.com/camdencheek/tree-sitter-go-mod"))
;;   ;; -- 2.
;;   ;; (dolist (lang '(go gomod)) (treesit-install-language-grammar lang))
;;   ;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
;;   )

(use-package treesit-auto
  ;; :defer 1
  :init
  (cl-defstruct xy/treesit-file-info size mtime ready)

  (defun xy/treesit-file-info-alist ()
    (mapcar (lambda (path)
              (let* ((attr (file-attributes path))
                     (size (file-attribute-size attr))
                     (mtime (file-attribute-modification-time attr))
                     (lang (and (string-match "libtree-sitter-\\([^./]+\\)" path)
                                (intern (match-string 1 path)))))
                (cons lang
                      (make-xy/treesit-file-info
                       :size (file-size-human-readable size 'decimal)
                       :mtime (format-time-string "%Y-%m-%d %H:%M" mtime)
                       :ready (treesit-ready-p lang)))))
            (directory-files (concat user-emacs-directory "tree-sitter") t "^libtree-sitter")))

  (defun xy/var-to-string (x &optional k)
    "根据变量的类型（nil/symbol/string/list）统一转化为字符串"
    (mapconcat (lambda (e) (format "%s" e)) (ensure-list x) (or k "")))

  (defun xy/treesit--read-langs-to-update ()
    (let* ((all-langs (mapcar #'symbol-name treesit-auto-langs))
           (buf-recipe (treesit-auto--get-mode-recipe))
           (buf-lang (and buf-recipe (symbol-name (treesit-auto-recipe-lang buf-recipe))))
           (info-alist (xy/treesit-file-info-alist))
           (recipe-alist (cl-loop for r in treesit-auto-recipe-list
                                  collect (cons (treesit-auto-recipe-lang r) r)))
           (completion-extra-properties
            `(:annotation-function
              ,(lambda (k)
                 (let* ((k (intern k))
                        (info    (alist-get k info-alist))
                        (recipe  (alist-get k recipe-alist))
                        (ext     (and recipe (treesit-auto-recipe-ext recipe)))
                        (ts-mode (and recipe (treesit-auto-recipe-ts-mode recipe)))
                        (remap   (and recipe (treesit-auto-recipe-remap recipe)))
                        (url     (and recipe (treesit-auto-recipe-url recipe)))
                        (size    (and info   (xy/treesit-file-info-size info)))
                        (mtime   (and info   (xy/treesit-file-info-mtime info)))
                        (ready   (and info   (xy/treesit-file-info-ready info)))
                        (column 0))
                   (cl-flet ((f-align (width x) ; Using let-over-lambda
                               (setq column (+ column width))
                               (concat
                                (thread-first x
                                              (xy/var-to-string "|")
                                              (thread-last (replace-regexp-in-string "\\\\" ""))
                                              (truncate-string-to-width (- width 1))
                                              (propertize 'face 'completions-annotations))
                                (propertize "-" 'display `(space :align-to ,column)))))
                     (concat (f-align 20 nil)
                             (f-align 20 ext)
                             (f-align 20 ts-mode)
                             (f-align 20 remap)
                             (f-align 60 url)
                             (f-align 2  (and info (if ready "o" "x")))
                             (f-align 18 mtime)
                             (f-align 8  size))))))))
      (completing-read-multiple "Update treesit grammars: " all-langs nil t nil nil buf-lang)))

  (defun xy/update-treesit-grammars (langs)
    (interactive
     (list (if current-prefix-arg
               (mapcar #'car (xy/treesit-file-info-alist))
             (xy/treesit--read-langs-to-update))))
    (with-output-to-temp-buffer "*Update Treesit Grammars*"
      (princ (format "The following tree-sitter grammars will be updated:\n%s\n"
                     (xy/var-to-string langs "\n"))))
    (when-let ((treesit-language-source-alist (treesit-auto--build-treesit-source-alist))
               (confirm (y-or-n-p "Update grammars? ")))
      (mapc #'treesit-install-language-grammar (mapcar #'intern langs))))

  :bind ("C-c u t" . #'xy/update-treesit-grammars)
  :config
  (setq treesit-auto-install 'prompt)
  ;; add ts-modes to `auto-mode-alist'
  (treesit-auto-add-to-auto-mode-alist 'all)
  ;; enable for `treesit-auto-langs', with their ts-modes and non-ts-mode
  (global-treesit-auto-mode +1))


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
          ("C-c s s" . consult-eglot-symbols)))

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
  :after eglot :demand t
  :hook (c-mode cpp-mode))

;; Speedier performance and less I/O blocking
(use-package eglot-booster
  :vc ( :url "https://github.com/jdtsmith/eglot-booster"
        :rev :newest)
  :after eglot :demand t
  :config
  ;; Or: cargo install emacs-lsp-booster
  (let ((emacs-lsp-booster-path (concat user-emacs-directory "emacs-lsp-booster")))
    (unless (file-exists-p emacs-lsp-booster-path)
      (make-directory emacs-lsp-booster-path))
    (push emacs-lsp-booster-path exec-path)
    (unless (executable-find "emacs-lsp-booster")
      (let ((temporary-zip-file (concat temporary-file-directory "emacs-lsp-booster.zip")))
        (shell-command (format "curl https://github.com/blahgeek/emacs-lsp-booster/releases/download/v0.2.1/emacs-lsp-booster_v0.2.1_x86_64-apple-darwin.zip -L -o %s" temporary-zip-file))
        (shell-command (format "unzip %s -d %s" temporary-zip-file emacs-lsp-booster-path))
        (shell-command (format "xattr -r -d com.apple.quarantine %s" (concat emacs-lsp-booster-path "/" "emacs-lsp-booster")))
        (delete-file temporary-zip-file))))

  (eglot-booster-mode +1)
  (setq eglot-booster-io-only t))

(provide 'init-prog)
