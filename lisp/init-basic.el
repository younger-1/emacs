;; -*- lexical-binding: t -*-

(require 'init-package)

;;; perf
(use-package async
  :bind ( :map emacs-lisp-mode-map
          ("C-c C-b" . #'xy/async-byte-compile-file)
          ("C-c C-l" . #'xy/load-byte-compile-file))
  :init
  (defun xy/async-byte-compile-file ()
    (interactive)
    (async-byte-compile-file buffer-file-name))
  (defun xy/load-byte-compile-file ()
    "Load elc file manually after `async-byte-compile-file' to make native-compile happen automatically."
    (interactive)
    ;; No use absolute file name to elc: (load (byte-compile-dest-file buffer-file-name))
    (load (file-name-base buffer-file-name)))

  ;; 问题：在 Lisp 和 Emacs 社区的工程规范中，“在 Hook 里套 Hook，里面还带个匿名函数 (lambda)” 被称为反模式（Anti-pattern），主要有三大罪状：
  ;; - 1. 无法被轻易移除：因为 lambda 没有名字，你事后想用 remove-hook 把它干掉几乎不可能。
  ;; - 2. 调试极其恶心：当你用 C-h v after-save-hook 查看当前有哪些钩子时，你会看到一坨 (closure ...)，根本不知道它是干嘛的。
  ;; - 3. 滥用 eval：破坏了局部变量声明式（Declarative）的纯粹性。
  ;; 解法：要把它 “拍平”，最符合 Emacs 官方架构哲学的方法是：状态与行为分离（Separation of State and Behavior / Data-Driven Design）。
  ;;       也即：全局函数读取局部状态变量。其本质上是在做 控制流的数据化（Datafication of Control Flow）
  ;;       我们通过 “定义一个局部开关变量 + 一个全局具名函数”，就能把原来丑陋的嵌套彻底消灭。
  ;; 拓展：无论是 Emacs 的全局 Hook 读取局部变量、React 的 State 驱动视图、K8s 的 YAML 控制器，还是游戏引擎的 ECS，它们的核心思想如出一辙：
  ;;       把脆弱、复杂、容易抛出异常的 “行为代码（Code/Action）” 集中关进坚固的全局底座中；
  ;;       而暴露给业务层、目录层、用户层的，永远只有安全、透明、可复验的 “状态数据（Data/State）”
  ;; 1. 定义一个开关变量
  (defvar-local xy/async-compile-on-save-p nil)
  ;; 2. 告诉 Emacs 这个变量是安全的布尔值，防止在 dir-locals 触发安全弹窗警告
  ;; (info "(elisp) File Local Variables")
  (put 'xy/async-compile-on-save-p 'safe-local-variable #'booleanp)
  ;; 3. 定义一个全局动作
  (defun xy/async-compile-if-enabled ()
    "当开关打开时，执行异步编译。"
    (and xy/async-compile-on-save-p
         (eq major-mode 'emacs-lisp-mode)
         buffer-file-name
         (async-byte-compile-file buffer-file-name)))
  ;; 4. 挂载到全局保存钩子，放到最后面执行
  (add-hook 'after-save-hook #'xy/async-compile-if-enabled :last)
  ;; 5. 将 dir-locals 变成纯粹的数据声明
  (dir-locals-set-class-variables
   :byte-compile
   '((emacs-lisp-mode . ((xy/async-compile-on-save-p . t)))))
  (dir-locals-set-directory-class xy/lisp-dir :byte-compile)
  (dir-locals-set-directory-class xy/site-lisp-dir :byte-compile)

  :config
  ;; Copy, rename, and symlink operations in Dired now run in the background
  (dired-async-mode +1)
  ;; Compiles packages in a clean Emacs subprocess
  (async-bytecomp-package-mode +1)
  ;; Async email
  (setq message-send-mail-function 'async-smtpmail-send-it))

(use-core server
  ;; :if (dispay-graphic-p)
  ;; :after-call doom-first-input-hook doom-first-file-hook focus-out-hook
  :defer 1
  :config
  ;; (setq server-client-instructions nil)
  ;; Local socket file is `server-name' under `server-socket-dir'
  ;; TCP server file is `server-name' under `server-auth-dir'
  (when xy/win-p
    (setq server-use-tcp t)
    ;; 将目录换回为标准路径 ;; emacsclient does not read the init file
    (setq server-auth-dir (expand-file-name "server" xy/init-dir)))
  (unless (or (server-running-p) (daemonp))
    (server-start))
  ;; (add-hook 'server-switch-hook
  ;;    (lambda ()
  ;;      (let ((server-buf (current-buffer)))
  ;;        (bury-buffer)
  ;;        (switch-to-buffer-other-frame server-buf))))
  ;; (add-hook 'server-done-hook
  ;;    (lambda ()
  ;;      (kill-buffer nil)
  ;;      (delete-frame)))
  )

;; Automatically byte-compiles and native-compiles Emacs Lisp libraries
;; Ensure adding the following compile-angel code at the very beginning of init file, before all other packages.
;; (use-package compile-angel
;;   :demand t
;;   :config
;;   ;; (setq compile-angel-verbose t)
;;   ;; (setq compile-angel-debug t)
;;   ;; (setq compile-angel-byte-compile-report-issues t)
;;
;;   ;; (push "/init.el" compile-angel-excluded-files)
;;   (push "/early-init.el" compile-angel-excluded-files)
;;
;;   ;; A local mode that compiles .el files whenever the user saves them.
;;   (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode)
;;   ;; A global mode that compiles .el files before they are loaded.
;;   (compile-angel-on-load-mode +1))

;; Garbage Collector Magic Hack
;; To minimize GC interference with user activity
;; - 1. During normal use a high GC threshold is set.
;; - 2. When idling GC is triggered and a low threshold is set.
(use-package gcmh
  :hook
  (emacs-startup . gcmh-mode)
  (focus-out-hook . gcmh-idle-garbage-collect)
  :config
  ;; 1 GB -> 800 MB
  (setq gcmh-high-cons-threshold (* 800 1024 1024))
  (setq gcmh-idle-delay 'auto))

;; Profiling the startup time of Emacs
(use-package esup
  :bind ("C-c x p" . esup)
  :config
  ;; https://github.com/jschaf/esup/issues/85
  ;; This is a work around of a bug where esup tries to step into the byte-compiled version of `cl-lib’, and fails horribly:
  (setq esup-depth 0))

(use-package benchmark-init
  :bind
  ("C-c x m" . benchmark-init/show-durations-tree)
  ("C-c x M" . benchmark-init/show-durations-tabulated)
  ;; :init ;; only activate when doing benchmark
  ;; (benchmark-init/activate)
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'emacs-startup-hook #'benchmark-init/deactivate))

(use-package bug-hunter
  :bind ("C-c x b" . bug-hunter-init-file))

;; (use-package restart-emacs
;;   :bind ("C-c x r" . restart-emacs))


;;; help
(use-core help
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
  ;; Let . as punctuation instead of word
  (add-hook 'help-mode-hook (lambda () (modify-syntax-entry ?. ".")))
  ;; Add shortdoc examples to help buffer
  (add-hook 'help-fns-describe-function-functions
            #'shortdoc-help-fns-examples-function)

  (defun xy/loaded-feature ()
    "Find loaded features"
    (interactive)
    (require 'loadhist)
    (find-library
     (let* ((coll (mapcar #'symbol-name features))
            (completion-extra-properties
             '(:annotation-function
               (lambda (k) ; only accept string
                 (when-let ((path (feature-file (intern k)))
                            (path (abbreviate-file-name path)))
                   (concat (propertize "-" 'display '(space :align-to 40))
                           (propertize path 'face 'completions-annotations)))))))
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

  (defun xy/open-scratch-buffer ()
    "Jump to the *scratch* buffer. If it does not exist, create it."
    (interactive)
    (switch-to-buffer "*scratch*"))

  (defun xy/open-byte-compile-log-buffer ()
    (interactive)
    (switch-to-buffer "*Compile-Log*"))

  (defun xy/open-native-compile-log-buffer ()
    (interactive)
    (switch-to-buffer "*Async-native-compile-log*"))

  (defun xy/count-lines-pages ()
    "Combine `what-line', `what-page' and `count-lines-page'"
    (interactive)
    (apply #'message `("%s. Page %d, line %d. Page has %d line (%d + %d)." ,(what-line) ,@(page--what-page) ,@(page--count-lines-page))))

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
         ;; ("C-h P" . nil) ; `describe-package'
         ;; ("C-h L" . nil) ; `describe-language-environment'
         ;; ("C-h C" . nil) ; `describe-coding-system'
         ;; ("C-h I" . nil) ; `describe-input-method'
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
         ("C-h d p" . #'describe-text-properties)
         ("C-h d y" . #'cl-describe-type)
         ("C-h d L" . #'describe-language-environment)
         ("C-h d C" . #'describe-coding-system)
         ("C-h d I" . #'describe-input-method)
         ;; view
         ("C-h h" . nil) ; `view-hello-file'
         ("C-h h q" . #'view-emacs-FAQ)
         ("C-h h p" . #'view-emacs-problems)
         ("C-h h n" . #'view-emacs-news)
         ("C-h h t" . #'view-emacs-todo)
         ("C-h h d" . #'view-emacs-debugging)
         ;; where
         ("C-h w" . nil) ; `where-is'
         ("C-h w c" . #'where-is)
         ("C-h w k" . #'describe-key-briefly)
         ;; doc
         ("C-h o" . nil) ; `describe-symbol'
         ("C-h o s" . #'shortdoc)
         ;; echo
         ("C-h e" . nil) ; `view-echo-area-messages' or click echo area
         ("C-h e e" . #'view-echo-area-messages)
         ("C-h e l" . #'view-lossage)
         ("C-h e v" . #'getenv)
         ("C-h e w" . #'count-words)
         ("C-h e W" . #'count-words-region)
         ("C-h =" . #'xy/count-lines-pages)
         ;; library
         ("C-h l" . nil) ; `view-lossage'
         ("C-h l l" . #'load-library)
         ("C-h l L" . #'xy/loaded-feature)
         ("C-h l u" . #'unload-feature)
         ;;
         ;; NOTE: safe keys: j u y z
         ;;
         ;; jump
         ("C-h j s" . #'xy/open-scratch-buffer)
         ("C-h j c" . #'xy/open-byte-compile-log-buffer)
         ("C-h j C" . #'xy/open-native-compile-log-buffer)
         ;; user
         ("C-h u" . nil) ; `apropos-user-option`
         ("C-h u f" . #'add-file-local-variable-prop-line)
         ("C-h u F" . #'add-file-local-variable)
         ("C-h u d" . #'add-dir-local-variable)
         ("C-h u c" . #'xy/set-variable)
         ("C-h u p" . #'xy/help-show-plist)
         ;;
         ("C-h t" . nil) ; `help-with-tutorial'
         ("C-h t t" . #'help-with-tutorial)
         ;;
         ("C-h g" . nil) ; `describe-gnu-project'
         ("C-h q" . nil) ; `help-quit'
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
         ))

(use-core help-mode
  :bind ( :map help-mode-map
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

(use-core info
  :init
  (defun xy/info-elisp () (interactive) (info "elisp"))
  (defun xy/info-eintr () (interactive) (info "eintr"))
  (defun xy/info-org () (interactive) (info "org"))
  :bind (("C-h r" . nil) ; `info-emacs-manual'
         ("C-h r r" . #'info-emacs-manual)
         ("C-h r e" . #'xy/info-elisp)
         ("C-h r i" . #'xy/info-eintr)
         ("C-h r o" . #'xy/info-org)
         :map Info-mode-map
         ;; ("M-n" . nil) ; `clone-buffer'
         ("S-SPC" . nil) ; `Info-scroll-down', available as DEL(<backspace>)
         ("." . #'Info-search-next)
         ("a" . #'info-apropos)))

(use-core package
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
         ("C-h p a" . #'xy/open-elpa-d)
         ("C-h p Q" . #'xy/open-package-quickstart)
         ("C-h p j" . #'use-package-jump-to-package-form)
         ("C-h p k" . #'use-package-report)))

(use-core cus-edit
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

(use-core tooltip
  :config
  (tooltip-mode -1)
  (setq tooltip-resize-echo-area t))

;; Add button for *Help* buffer to remove Advice / Generic method.
(use-package help-remove-button
  :vc ( :url "https://github.com/twlz0ne/help-remove-button.el"
        :rev :newest)
  :after help :demand t)


;;; history
;; Pick recently visited files
(use-core recentf
  :hook emacs-startup
  :bind
  ("C-x f r" . recentf-open)
  ("C-x f R" . recentf-open-files)
  :config
  ;; TODO: https://vincent.demeester.fr/articles/emacs_keep_it_clean.html
  (setq recentf-auto-cleanup 60)
  ;; 禁止它在后台自动检查文件是否存在，防止 Tramp 远程网络卡死 Emacs
  ;; (setq recentf-auto-cleanup 'never)
  (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:")
  ;; (add-to-list 'recentf-exclude (regexp-quote (abbreviate-file-name xy/emacs-lisp-dir)))
  ;; (add-to-list 'recentf-exclude (regexp-quote (abbreviate-file-name xy/elpa-lisp-dir)))
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 25))

;; Goto the last location within a file upon reopening
(use-core saveplace
  ;; @perf Loading when open file
  ;; :hook (find-file . save-place-mode)
  :init
  (add-hook 'find-file-hook
            (defun xy/defer-load-saveplace ()
              (remove-hook 'find-file-hook #'xy/defer-load-saveplace)
              (save-place-mode +1)
              ;; for the first opened file
              (save-place-find-file-hook)))
  :config
  (setopt save-place-abbreviate-file-names t))

;; Save various kind of history between sessions
(use-core savehist
  ;; @perf Loading when open minibuffer
  :hook (minibuffer-setup . savehist-mode)
  :init
  ;; `completing-read' and `read-from-minibuffer'
  ;; -- The argument HISTORY specifies which history list variable to use for saving the input and for minibuffer history commands.
  ;; -- It defaults to ‘minibuffer-history’
  ;; `savehist-minibuffer-history-variables'
  ;; `savehist-ignored-variables'
  (setq savehist-additional-variables '(kill-ring      ; clipboard
                                        register-alist ; keyboard macro
                                        mark-ring global-mark-ring ; mark
                                        search-ring regexp-search-ring ; search
                                        log-edit-comment-ring ; vc commit msg
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
  :config
  ;; Strip all text properties (fonts, overlays, etc.) in kill ring before saving, not to bloat the savehist file.
  (add-hook 'savehist-save-hook
            (lambda ()
              (setq kill-ring
                    (mapcar #'substring-no-properties
                            (cl-remove-if-not #'stringp kill-ring)))))
  (savehist-mode +1))

(use-core autorevert
  :defer 0.4
  :config
  (global-auto-revert-mode +1)
  ;; @tip "C-x x g" is `revert-buffer-quick', "s-u" is `revert-buffer'
  (setq global-auto-revert-non-file-buffers t)
  ;; Set to nil if too slow
  (setq auto-revert-remote-files t))


;;; search

(keymap-global-set "M-s M-r" #'replace-regexp-as-diff)

(use-core isearch
  :config
  ;; @tip `isearch-mode-map'
  ;; [M-s M-.] -> `isearch-forward-thing-at-point' can use active region
  ;; To enable `minibuffer-local-isearch-map' which derived from `minibuffer-local-map'
  ;; -- 1.M-e -> `isearch-edit-string'
  ;; -- 2.M-p/M-n -> `isearch-ring-retreat' / `isearch-ring-advance'
  ;; -- 3.[C-s RET] -> `isearch-exit' do nonincremental search
  (setq isearch-lazy-count t)
  (setq isearch-lazy-highlight 'all-windows)
  (setq lazy-count-prefix-format nil)
  (setq lazy-count-suffix-format " [%s/%s]")
  ;;
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

(use-core grep
  :bind ( :map grep-mode-map
          ("H" . xy/toggle-grep-headings))
  :config
  (setq grep-use-headings t)
  (defun xy/toggle-grep-headings ()
    (interactive)
    (if grep-use-headings
        (setq grep-use-headings nil)
      (setq grep-use-headings t))
    (recompile))

  (when (executable-find "rg")
    ;; Populate defaults before change it
    (grep-compute-defaults)
    ;; 1. Use rg only in localhost, so modify `grep-host-defaults-alist' directly
    ;; (setcdr (assq 'localhost grep-host-defaults-alist)
    ;;         '((grep-command "rg --no-heading -Hn0 ")
    ;;           (grep-highlight-matches t)))
    ;; 2. Use rg in all host
    (setopt grep-command "rg -nS --no-heading ")))

;; Writable grep buffer and apply the changes to files
;; C-c C-c -> commit changes
;; C-c C-k -> drop changes
(use-package wgrep
  :config
  (setq wgrep-enable-key "e")
  ;; To save buffer automatically when `wgrep-finish-edit'.
  (setq wgrep-auto-save-buffer t))

;; @see `ripgrep--base-arguments'
;; (use-package ripgrep
;;   :bind (("M-s S" . ripgrep-regexp)
;;          :map ripgrep-search-mode-map
;;          ("e" . wgrep-change-to-wgrep-mode)))

;; 1. `rg-dwim':
;; -- @prefix Use current dir instead of project root
;; 2. `rg' or `rg-literal':
;; -- @prefix Show the full command line that will invoke the ripgrep binary.
;; -- This could e.g. search for multiple directories
;; 3. @tip `rg-mode-map':
;; r/t -> `rg-rerun-change-regexp' / `rg-rerun-change-literal'
;; d/f -> `rg-rerun-change-dir' / `rg-rerun-change-files'
;; c/i -> `rg-rerun-toggle-case' / `rg-rerun-toggle-ignore'
;; m -> `rg-menu'
;; [m b]/[m w] -> `rg-back-history' / `rg-forward-history'
(use-package rg
  :bind (("M-s M-s" . rg-menu)
         ("M-s s" . rg-isearch-menu)
         :map isearch-mode-map
         ("M-s s" . rg-isearch-menu))
  :bind-keymap
  ("M-s S" . rg-global-map)
  :config
  (defun xy/project-root (&optional buffer)
    "Return project root of BUFFER, or its `default-directory'."
    (abbreviate-file-name
     (with-current-buffer (or buffer (current-buffer))
       (if-let* ((p (project-current)))
           (project-root p)
         default-directory))))
  (setq rg-buffer-name (lambda () (format "rg %s" (xy/project-root)))))

(use-package deadgrep
  :init
  (defun xy/deadgrep-isearch ()
    (interactive)
    (deadgrep isearch-string))
  :bind (("M-s M-d" . deadgrep)
         ("M-s d" . #'xy/deadgrep-isearch)
         :map isearch-mode-map
         ("M-s d" . #'xy/deadgrep-isearch)
         :map deadgrep-mode-map
         ("e" . deadgrep-edit-mode))
  :config
  ;; TODO: use `project-find-functions', simple fix for now
  (setq deadgrep-project-root-overrides
        `(("/opt/homebrew/" . ,xy/emacs-lisp-dir))))


;;; minibuffer
(use-core minibuffer
  ;; (info "(emacs) Minibuffer History")
  ;; `minibuffer-local-map'  所有 minibuffer 输入。`'read-string' M-:
  ;; `minibuffer-local-completion-map'有补全的 minibuffer。`completing-read' `read-file-name'
  :config
  ;; completion
  (setq completions-detailed t)
  (setq completion-styles '(basic initials substring partial-completion flex)) ; @see `completion-styles-alist' for available style
  (setq completion-category-overrides ; @see `completion-category-defaults' for available category
        '((file (styles basic partial-completion)))) ; partial-completion enable open multiple files with `find-file' using wildcards
  (setq completion-ignore-case t
        read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t)
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
  ;; M-x only show commands which are applicable to major mode and active minor modes
  (setq read-extended-command-predicate #'command-completion-default-include-p)
  ;; Allow nested minibuffers.
  (setq enable-recursive-minibuffers t)
  (add-hook 'emacs-startup-hook #'minibuffer-depth-indicate-mode)
  ;; Keep the cursor out of the read-only portions of the minibuffer.
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  ;; Add prompt indicator to `completing-read-multiple', e.g. `describe-face'.
  ;; Display it as [CRM<separator>], e.g. [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; minibuffer UX
  (setq use-short-answers t)
  ;; Disable GUIs because they are inconsistent across systems, desktop environments, and themes, and they don't match the look of Emacs.
  ;; (setq use-dialog-box nil)
  (setq use-file-dialog nil)
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
  ;; Select the candidate number with M-<number>
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
          (consult-line buffer)
          ;; (consult-imenu reverse buffer)
          (execute-extended-command-for-buffer (:not indexed mouse))))
  (setq vertico-multiform-categories ; categories at `marginalia-annotators'
        '((file buffer)
          (project-file buffer)
          (buffer buffer)
          (symbol (vertico-sort-function . vertico-sort-alpha))
          (command (:not indexed))))
  (vertico-multiform-mode +1))

;; Consult provides search and navigation commands based on `completing-read'
(use-package consult
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c s f" . consult-fd)
         ("C-c s d" . consult-find)
         ("C-c s c" . consult-locate)
         ("C-c s r" . consult-ripgrep)
         ("C-c s g" . consult-grep)
         ("C-c s G" . consult-git-grep)
         ;;
         ("C-c s h" . consult-history)
         ("C-c s k" . consult-kmacro)
         ("C-c s m" . consult-man)
         ("C-c s i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; @orig repeat-complex-command
         ("C-x b" . consult-buffer)                ;; @orig switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; @orig switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; @orig switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; @orig switch-to-buffer-other-tab
         ("C-x f b" . consult-bookmark)
         ("C-x f f" . consult-recent-file)
         ("C-x p b" . consult-project-buffer)      ;; @orig project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; @orig abbrev-prefix-mark (unrelated)
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
         ("M-g g" . consult-goto-line)             ;; @orig goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ("M-g M-i" . #'xy/consult-imenu-lisp)
         ;; M-s bindings in `search-map'
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-focus-lines)
         ("M-s K" . consult-keep-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-s e" . consult-isearch-history)       ;; @orig isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; @orig next-matching-history-element
         ("M-r" . consult-history))                ;; @orig previous-matching-history-element

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

  :config
  ;; Configure the :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; TODO: https://arialdomartini.github.io/consult-line-at-point
  ;; @see https://www.reddit.com/r/emacs/comments/1jwk4dg/consultlinesymbolatpoint/
  (consult-customize
   consult-line consult-focus-lines
   :add-history (seq-some #'thing-at-point '(region symbol)))

  ;; Smart recenter: buffer is recentered only if you jump to match outside of current view
  ;; @see https://www.reddit.com/r/emacs/comments/14aglvm/highlight_multiple_lines_in_consultline/
  (defvar-local xy/prev-position nil)
  (defun xy/consult-maybe-recenter ()
    "Maybe recenter current window if point is outside of visible region."
    (when xy/prev-position
      (set-window-start (selected-window) xy/prev-position))
    (when (or (< (point) (window-start))
              (> (point) (window-end (selected-window) t)))
      (recenter))
    (setq xy/prev-position (window-start)))
  (setq consult-after-jump-hook '(xy/consult-maybe-recenter))

  (setq consult-narrow-key "<")
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'embark-prefix-help-command)

  (with-eval-after-load 'consult-imenu
    (add-to-list 'consult-imenu-config
                 '(emacs-lisp-mode :toplevel "Functions"
                                   :types ((?o "Options"   font-lock-doc-face)
                                           (?b "Builtin Packages" font-lock-builtin-face)
                                           (?l "Libraries" font-lock-string-face)
                                           (?s "Sections"  font-lock-comment-face)
                                           (?f "Functions" font-lock-function-name-face)
                                           (?m "Macros"    font-lock-keyword-face)
                                           (?p "Packages"  font-lock-constant-face)
                                           (?t "Types"     font-lock-type-face)
                                           (?v "Variables" font-lock-variable-name-face)))))
  (defun xy/consult-imenu-lisp ()
    "Imenu across all Elisp files in `xy/lisp-dir'"
    (interactive)
    (let ((dir xy/lisp-dir))
      (mapc #'find-file-noselect (directory-files dir t "\\.el\\'"))
      (consult-imenu-multi (list :sort 'alpha
                                 :mode 'emacs-lisp-mode
                                 :directory dir))))

  (consult-info-define "emacs" "efaq" "elisp" "eintr" "cl")
  (consult-info-define 'all "widget" "ediff" "eglot" "flymake" "eshell" "tramp" "org" "gnus" "calc" "eww")
  ;; "magit" "dash"
  (consult-info-define 'completion
                       "vertico" "consult" "marginalia" "orderless" "embark" "corfu" "cape")

  (add-to-list 'consult-mode-histories '(vc-git-log-edit-mode . log-edit-comment-ring))
  (add-to-list 'consult-mode-histories '(text-mode . log-edit-comment-ring)))

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

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

;; Enriches the completion display with annotation, e.g. docstring, value of variable
;; 1.provide classifiers for embark
;; -- `marginalia-classifiers'
;; 2.provide annotators for minibuffer
;; -- `marginalia-annotators'
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

  (add-to-list 'marginalia-annotators
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
   ("M-<return>" . embark-export) ; @see `embark-exporters-alist', falls back to the generic `embark-collect'
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
  :after embark
  :vc ( :url "https://github.com/kn66/embark-sidebar.el"
        :rev :newest)
  :bind ("C-x d e" . embark-sidebar-toggle)
  :config
  (embark-sidebar-mode +1))


;;; completion
;; (use-core completion-preview
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
  ;; :defer 2
  :hook (prog-mode text-mode)
  :bind ( :map corfu-map
          ;; ("RET" . nil) ; Free RET for newline etc.
          ;; ("TAB" . corfu-next) ; Use TAB for cycling
          ;; ("S-TAB" . corfu-previous)
          ("RET" . corfu-send)
          ("S-SPC" . corfu-insert-separator)
          ("M-q" . corfu-quick-complete))
  :config
  ;; (setq corfu-preview-current nil)
  (setq corfu-auto t)
  (setq corfu-auto-prefix 2)
  (setq corfu-cycle t)
  (setq corfu-scroll-margin (/ corfu-count 2))
  ;; Recommended enable globally since many modes provide Capfs and Dabbrev can be used globally (M-/).
  (global-corfu-mode +1)

  ;; Sort completions by history
  (corfu-history-mode +1)

  ;; Show documentation in echo area.
  ;; (corfu-echo-mode +1)

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

(provide 'init-basic)
