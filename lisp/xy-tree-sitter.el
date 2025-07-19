;; @see https://emacs-tree-sitter.github.io/syntax-highlighting/interface-for-modes/
;; Major modes that want to integrate with `tree-sitter-hl-mode' should set the variable `tree-sitter-hl-default-patterns'. It plays a similar role to `font-lock-defaults'.
;; Minor modes that want to customize syntax highlighting should call the function `tree-sitter-hl-add-patterns'. It plays a similar role to `font-lock-add-keywords'.
(use-package tree-sitter
  :init
  ;; Turn on `tree-sitter-mode' for each major mode.
  (global-tree-sitter-mode +1)
  ;; @see `tree-sitter-major-mode-language-alist' for the full list of supported major modes
  ;; (add-hook 'prog-mode-hook #'tree-sitter-mode)

  ;; Replace the regex-based highlighting provided by font-lock-mode.
  ;; If `tree-sitter-hl-default-patterns' is nil, turning on this mode does nothing, and does not interfere with `font-lock-mode'.
  ;; TODO (let ((major-mode 'go-mode)) (tree-sitter-mode) (font-lock-update))
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

  :config
  (dolist (mode-pair '((lisp-interaction-mode . elisp)
                       (lisp-data-mode . elisp)
                       (python-ts-mode . python)
                       (typescript-ts-mode . tsx)
                       (tsx-ts-mode . tsx)
                       (clojure-ts-mode . clojure)
                       (swift-ts-mode . swift)))
    (add-to-list 'tree-sitter-major-mode-language-alist mode-pair)))

;; The language bundle `tree-sitter-langs' provides highlighting queries for several languages.
;; These queries will be used when the corresponding major modes do not set `tree-sitter-hl-default-patterns'. eg. by calling (tree-sitter-langs--hl-default-patterns 'go)
(use-package tree-sitter-langs
  :demand t)

;; @see https://tony-zorman.com/posts/use-package-vc.html
;; (info "(use-package) Install package")
;; (info "(emacs) Fetching Package Sources")
;; `package-vc-selected-packages'
(use-package ts-fold
  :vc ( :url "https://github.com/emacs-tree-sitter/ts-fold"
        :rev :newest)
  :bind
  ("C-c z" . ts-fold-toggle)
  :init
  ;; (global-ts-fold-mode +1)
  ;; (global-ts-fold-indicators-mode +1)
  (add-hook 'tree-sitter-after-on-hook #'ts-fold-mode)
  (add-hook 'tree-sitter-after-on-hook #'ts-fold-indicators-mode))
