;; -*- lexical-binding: t -*-

;; For byte/native compiler to get `xy/var-dir'
(require 'init-util)

;;; package
(setq package-archives '(("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("gnu-dev". "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu-devel/")
                         ("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")))

;; (setq package-archive-priorities '(("gnu"    . 90)
;;                                    ("nongnu" . 80)
;;                                    ("melpa"  . 10)))

;; Enable `package-quickstart-refresh'
(setq package-quickstart t)
(setq package-install-upgrade-built-in t)
;; (setq package-native-compile t)

(setq package-user-dir (concat xy/var-dir "elpa"))
(setq package-quickstart-file (concat xy/var-dir "package-quickstart.el"))

;; @perf Avoid `package-initialize' autoload `package'
(package-activate-all)
(unless (file-exists-p package-user-dir)
  (package-refresh-contents))


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
  (require 'use-package))

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

(defmacro use-core (name &rest args)
  (declare (indent 1))
  `(use-package ,name
     :ensure nil
     ,@args))

;; Use this instead of (use-package <feature> :ensure <package>)
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

(provide 'init-package)
