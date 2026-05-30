;; -*- lexical-binding: t -*-

;; For byte/native compiler to get `xy/var-dir'
(require 'init-util)

;;; package
(setq package-archives '(("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ;; ("gnu-dev". "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu-devel/")
                         ("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")))

;; 没有出现在此列表中的 archive 默认优先级为 0
;; - 默认行为，按版本号比较
;; 假设某个包被此列表的多个 archive 包含，安装时只在优先级最高的 archive 中选版本最高的
;; - 低优先级 archive 有更高版本（如 MELPA 的日期版本）不会被自动选中，用户必须手动指定（通过 :pin 或手动 package-install）
;; - :pin 是硬过滤，priorities 是软排序。过滤在排序之前执行
;; 已安装的包（本地版本）也被视为优先级 0。这意味着：
;; - 如果 archive 优先级 > 0，升级时即使本地已安装的版本更高，也会考虑从高优先级 archive "降级"
;; - 如果 archive 优先级 < 0，从该 archive 装的包不会被自动升级
;; 最佳实践
;; - 大多数 MELPA 包的开发版足够稳定，直接用即可。不需要设置此优先级列表（比较版本时，melpa的日期版本永远更新）
;; - 只对核心包（如 magit、org）pin 到 GNU/NonGNU ELPA 防止意外
;;
;; (setq package-archive-priorities '(("gnu"    . 90)
;;                                    ("nongnu" . 80)
;;                                    ("melpa"  . 10)))

;; Enable `package-quickstart-refresh'
(setq package-quickstart t)
;; (setq package-install-upgrade-built-in t)
;; (setq package-native-compile t)

(setq package-user-dir (concat xy/var-dir "elpa"))
(setq package-quickstart-file (concat xy/var-dir "package-quickstart.el"))

;; @perf Avoid `package-initialize' autoload `package'
(package-activate-all)
(unless (file-exists-p package-user-dir)
  (package-refresh-contents))

;; alist结构 ((pkg desc1 desc2...) ...)
;; `package-archive-contents' archive 中可用的包 alist
;; `package-alist' 已安装的包 alist
;; `package--builtins' 内置包 alist
;; `package--builtin-versions'
(defun xy/package-upgrade-info (pkg)
  "Return \"pkg: CUR -> NEW\" if archive has a newer version of PKG, else nil.
Works uniformly for installed and built-in packages."
  (package--archives-initialize)
  (let* ((new-desc (cadr (assq pkg package-archive-contents)))
         (cur-desc (cadr (assq pkg package-alist)))
         (new (package-desc-version new-desc))
         (cur (or (and cur-desc (package-desc-version cur-desc))
                  (alist-get pkg package--builtin-versions))))
    (when (version-list-< cur new)
      (format "%s: %s -> %s" pkg
              (package-version-join cur)
              (package-version-join new)))))

;; Delete upgraded builtin pakcages
;; (mapc #'package-delete
;;       (mapcar (lambda (p) (cadr (assoc p package-alist)))
;;               (seq-remove #'package--active-built-in-p
;;                           (mapcar #'car package--builtins))))
;; (compat jsonrpc let-alist project seq transient xref)

;; Delete all old versions of each installed package
;; (mapc #'package-delete
;;       (mapcan #'cddr (seq-filter #'cddr package-alist)))


;;; use-package
;; 编译期（Compile-time） 和 加载期 / 运行期（Load-time/Run-time）
;; - 编译后执行 (byte code) 和 解释执行 (source code)
;; - Compiled files load and run faster
;;                         1.直接加载源码   2.编译当前文件   3.加载编译后文件
;; normal top-level code     yes              no               yes
;; `eval-and-compile'        yes              yes              yes
;; `eval-when-compile'       yes              yes              no
;;
;; `eval-when-compile' 最常见用法是 “给编译器准备编译期依赖”。 实践里最常见的两类是：
;; 1. 编译期引入宏定义： require 宏库
;; 2. 编译期算好结果，运行时直接用：预计算常量

;; Load `use-package' macro definition when compiling THIS file
;; @see (info "(elisp) Compiling Macros")
(eval-when-compile
  (require 'use-package))

;; Load `use-package-ensure' to install missing packages when compiling OTHER files which require THIS file
(when (bound-and-true-p byte-compile-current-file)
  (require 'use-package-ensure))

;; Load `use-package-ensure' to install missing packages when eval `use-package' macro manually at runtime
(with-eval-after-load 'use-package-core
  (require 'use-package-ensure))

(setq use-package-always-ensure t)
(setq use-package-always-defer t)
(setq use-package-enable-imenu-support t)
(setq use-package-expand-minimally t)
(setq use-package-vc-prefer-newest t)
;; TODO
;; (setq use-package-hook-name-suffix nil)
;; (setq use-package-inject-hooks t)

(when init-file-debug
  (require 'use-package)
  (setq use-package-expand-minimally nil)
  (setq use-package-compute-statistics t) ; for `use-package-report'
  ;; (setq use-package-minimum-reported-time 0)
  (setq use-package-verbose t))

;; For builtin packages
(defmacro use-core (name &rest args)
  (declare (indent 1))
  `(use-package ,name
     :ensure nil
     ,@args))

;; For installed packages, refer to their features
(defmacro use-feature (name &rest args)
  (declare (indent 1))
  `(use-package ,name
     :ensure nil
     ,@args))

;; @perf Disable :ensure and :vc at startup
;; Only in interpreted run, :ensure and :vc may autoload `package', while in compiled run expanded code is clean. @see `use-package-handler/:ensure'
;; (unless after-init-time
;;   (advice-add #'use-package-ensure-elpa :around #'ignore)
;;   (advice-add #'use-package-vc-install :around #'ignore))
;; (add-hook 'emacs-startup-hook
;;           (defun xy/restore-use-package-ensure ()
;;             (advice-remove #'use-package-ensure-elpa #'ignore)
;;             (advice-remove #'use-package-vc-install #'ignore)))

;; normalize 负责把用户写的各种格式统一为内部表示 (列表而非单值)
;; `use-package-normalize/:ensure'
;;
;; |                          | args               | use-package-only-one    | use-package-normalize/:ensure |
;; |--------------------------+--------------------+-------------------------+-------------------------------|
;; | :ensure (无参数)         | nil                | 不调用，直接返回(t)     | (t)                           |
;; | :ensure t                | (t)                | arg = t                 | (t)                           |
;; | :ensure nil              | (nil)              | arg = nil               | (nil)                         |
;; | :ensure foo              | (foo)              | arg = foo               | (foo)                         |
;; | :ensure (foo :pin melpa) | ((foo :pin melpa)) | arg = (foo :pin melpa)  | ((foo . melpa))               |
;; | :ensure t nil            | (t nil)            | 报错：wants exactly one | 报错                          |

(defvar xy/ensured-pkgs nil
  "Packages explicitly declared via `use-package'.")

(with-eval-after-load 'use-package-ensure
  (advice-add #'use-package-handler/:ensure :around
              (lambda (fn name keyword ensure rest state)
                (let ((body (funcall fn name keyword ensure rest state)))
                  (when (car ensure)
                    ;; Or (cons `(add-to-list 'xy/ensured-pkgs ',name) body)
                    (push `(cl-pushnew ',name xy/ensured-pkgs) body))
                  body))))

(add-hook 'emacs-startup-hook
          (defun xy/ensured-pkgs ()
            "Get all the ensured/declared pkgs and set the `package-selected-packages', so `package-autoremove' can do what I want"
            (let ((declared xy/ensured-pkgs)
                  (installed (eval (car (get 'package-selected-packages 'saved-value)))))
              (unless (seq-set-equal-p declared installed)
                (with-output-to-temp-buffer "*xy/ensured-pkgs*"
                  (princ (format "Maybe install (only declared): %s\n\nMaybe delete (not declared): %s"
                                 (seq-difference declared installed)
                                 (seq-difference installed declared))))
                (customize-set-variable 'package-selected-packages xy/ensured-pkgs)))))

(provide 'init-package)
