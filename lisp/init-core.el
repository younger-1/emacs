;; -*- lexical-binding: t -*-

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

(defun xy/load-theme (theme &optional _ _)
  "Load a single theme interactively. Without prefix argument, disable all other enabled themes."
  (interactive (eval (cadr (interactive-form 'load-theme))))
  (if (called-interactively-p)
      (message "[xy]: load theme: %s" theme))
  (unless current-prefix-arg
    (mapc #'disable-theme custom-enabled-themes))
  (funcall-interactively 'load-theme theme :no-confirm))

(keymap-global-set "C-c y l" #'xy/load-theme)
(keymap-global-set "C-c y u" #'disable-theme)

(defvar xy/after-enable-theme-hook nil
  "Normal hook run after enabling a theme.")
(defun xy/after-enable-theme (&rest _args)
  "Run `xy/after-enable-theme-hook'."
  (run-hooks 'xy/after-enable-theme-hook))
(advice-add #'enable-theme :after #'xy/after-enable-theme)


;;; font
(defconst xy/font-size (if xy/win-p 120 160))
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
;; @see http://xahlee.info/emacs/emacs/emacs_keybinding_functions.html
;; @see (info "(elisp) Key Binding Conventions") to know which keys are safe for users
(keymap-global-set "C-x C-C" #'restart-emacs)
(keymap-global-set "C-x C-a" #'rename-visited-file)
(keymap-global-set "C-x C-j" #'find-sibling-file)

(keymap-global-set "C-h j i" (defun xy/open-init-file ()
                               (interactive)
                               (find-file user-init-file)))
(keymap-global-set "C-h j I" (defun xy/open-init-dir ()
                               (interactive)
                               (dired xy/init-dir)))
(keymap-global-set "C-h j l" (defun xy/open-lisp-dir ()
                               (interactive)
                               (dired xy/lisp-dir)))
(keymap-global-set "C-h j L" (defun xy/open-site-lisp-dir ()
                               (interactive)
                               (dired xy/site-lisp-dir)))
(keymap-global-set "C-h j ," (defun xy/open-site-lisp-dir ()
                               (interactive)
                               (find-file custom-file)))

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
;; NOTE:
;; C-M-m is M-RET, not M-<return>
;; C-M-i is M-TAB, not M-<tab>
;;
;; C-x ESC ESC -> `repeat-complex-command'
;; M-SPC -> `cycle-spacing'
;; M-m -> `back-to-indentation'
(keymap-global-set "<backtab>" #'back-to-indentation) ; or "S-<tab>"

;; @tip from `newcomment', see (info "(emacs) Comment Commands")
;; https://emacsredux.com/blog/2026/02/25/so-many-ways-to-work-with-comments/
;; M-j -> `default-indent-new-line' (continue a comment on the next line)
;; M-; -> `comment-dwim'
(keymap-global-set "C-;" #'comment-line)
(keymap-global-set "C-x C-;" #'comment-box) ; @orig `comment-line'
;;
;; @tip from `indent'
;; -- 1. Basic: see (info "(emacs) Indentation Commands")
;; C-o / C-x C-o -> `open-line' / `delete-blank-lines'
;; C-M-o -> `split-line'
;; M-^ -> `delete-indentation' (inverse of `split-line')
;; M-i -> `tab-to-tab-stop'
;; C-M-\ -> `indent-region'
;; C-x TAB -> `indent-rigidly'
;;
;; -- 2. Programming: see (info "(emacs) Multi-line Indent")
;; M-q -> `prog-fill-reindent-defun'
;; C-M-q -> `indent-pp-sexp' ; from `emacs-lisp-mode-map'

;; @see https://www.reddit.com/r/emacs/comments/1ohr4uy/tip_use_deletepair_to_change_surroundings_similar/
;; To change surroundings/delimiters, e.g. (some text) to [some text]
;; - 1. C-M-SPC to mark
;; - 2. Type [ to add []. Note: require `electric-pair-mode'
;; - 3. M-_ to remove (). Note: point should at open delimiter
;; To only paste the inside of [foo bar]
;; - 1. C-M-SPC to mark, M-w to copy, C-y to yank
;; - 2. M-_ with negative-argument (M-- M-_). Note: point should at close delimiter
;; To change [foo] to [bar]
;; - 1. C-M-k to delete
;; - 2. Type [ to recreate it
(keymap-global-set "M-_" #'delete-pair)
(keymap-global-set "M-+" #'duplicate-dwim)
(keymap-global-set "M-=" #'copy-from-above-command) ; @orig `count-words-region'
(keymap-global-set "M-z" #'zap-up-to-char) ; @orig `zap-to-char'

;; M-( -> `insert-parentheses'
;; M-) -> `move-past-close-and-reindent'
;; @tip use prefix arguments to insert pairs without mark activating
;; To insert pairs of (), [], {} and ""
;; (define-key esc-map "("  #'insert-pair)
;; (define-key esc-map "["  #'insert-pair)
;; (define-key esc-map "{"  #'insert-pair)
;; (define-key esc-map "\"" #'insert-pair)

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

;; @tip from `term/ns-win'
;; s-? -> `info'
;; s-t -> `menu-set-font'
;; s-, -> `customize'
(keymap-global-set "M-s-," #'customize-group)
(keymap-global-set "s-<return>" #'toggle-frame-maximized) ; M-<f10>
(keymap-global-set "S-s-<return>" #'toggle-frame-fullscreen) ; <f11>
;; s-' -> `next-window-any-frame'
;; s-` -> `other-frame'
;; s-n -> `make-frame'
;; s-w -> `delete-frame'
;; s-u -> `revert-buffer'
;; s-k/s-& -> `kill-current-buffer'
;; s-^ -> `kill-some-buffer'
(keymap-global-set "s-b" #'switch-to-buffer)
(keymap-global-set "s-K" #'bury-buffer)
(keymap-global-set "s-z" #'undo-only) ; @orig `undo'
(keymap-global-set "s-Z" #'undo-redo)

;; C-/ -> undo
;; (keymap-global-set "C-M-/" #'undo-redo) ;; For gui; in tty "C-M-/" == "C-M-_"

;; @tip I should practice more by using `C-]' for `abort-recursive-edit'
;; @see (info "(emacs) Quitting")
;; (keymap-global-set "C-g" (defun xy/keyboard-quit-dwim ()
;;                            "Do-What-I-Mean behaviour for a general `keyboard-quit'."
;;                            (interactive)
;;                            (cond
;;                             ((region-active-p)
;;                              (keyboard-quit))
;;                             ((derived-mode-p 'completion-list-mode)
;;                              (delete-completion-window))
;;                             ((> (minibuffer-depth) 0)
;;                              (abort-recursive-edit))
;;                             (t
;;                              (keyboard-quit)))))

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


;;; env/path
;; https://github.com/purcell/exec-path-from-shell
;; support non-POSIX-standard shell: fish, nu

(defun xy/set-env-simple (env)
  "Set environment variable to value without seperator"
  (setenv env (shell-command-to-string (format "$SHELL --login -c 'echo -n $%s'" env))))
;; To put server file where emacsclient knows
(xy/set-env-simple "XDG_RUNTIME_DIR")

;; For fish shell
(when (string-suffix-p "fish" (getenv "SHELL"))
  (setq path-separator " "))

;; For mac gui
(when (and xy/mac-p (display-graphic-p) (not (getenv "EMACS_PLUS_PATH")))
  ;; @see https://www.emacswiki.org/emacs/ExecPath
  (defun xy/set-exec-path-from-shell-PATH ()
    "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.

This is particularly useful under macOS, where GUI apps are not started from a shell.
NOTE: PATH in emacs should always separated by `:'"
    (interactive)
    (let* ((path-str (if (string-suffix-p "fish" (getenv "SHELL"))
                         (shell-command-to-string "$SHELL --login -c 'string join : $PATH'")
                       (shell-command-to-string "$SHELL --login -c 'echo -n $PATH'")))
           (path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" path-str)))
      ;; For (shell-command-to-string "gls")
      (setenv "PATH" path-from-shell)
      ;; For (executable-find "gls")
      (setq exec-path (split-string path-from-shell path-separator))))
  (add-hook 'emacs-startup-hook #'xy/set-exec-path-from-shell-PATH))

(when xy/win-p
  (defconst xy/git-bin-dir (expand-file-name (file-name-concat (getenv "SCOOP") "apps/git/current/usr/bin")))
  (when (file-exists-p xy/git-bin-dir)
    ;; For (executable-find "ls")
    (add-to-list 'exec-path xy/git-bin-dir)
    ;; For (shell-command-to-string "ls")
    (setenv "PATH" (concat xy/git-bin-dir ";" (getenv "PATH"))))
  (setenv "LANG" "en_US")
  (cd "~/"))

(defconst xy/mason-bin-dir (expand-file-name "~/.local/share/nvim/mason/bin"))
(add-to-list 'exec-path xy/mason-bin-dir)

(provide 'init-core)
