;; -*- lexical-binding: t -*-

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

;; (package-initialize)
(package-activate-all)
(unless (file-exists-p package-user-dir)
  (package-refresh-contents))


;;; use-package
;; load time (or interpreted run) vs compile time
;;                         1.直接加载源码   2.编译当前文件   3.加载编译后文件
;; normal top-level code     yes              no               yes
;; `eval-and-compile'        yes              yes              yes
;; `eval-when-compile'       yes              yes              no
;;
;; `eval-when-compile' 最常见用法是 “给编译器准备编译期依赖”。 实践里最常见的两类是：
;; 1. 编译期引入宏定义： require 宏库
;; 2. 编译期算好结果，运行时直接用：预计算常量

;; Load `use-package' macro definition when compiling
;; @see (info "(elisp) Compiling Macros")
(eval-when-compile
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

;; `package-activate-all' will not autoload `package', but use-package's :ensure and :vc will require it
;; @perf Not require `package' by disabling :ensure and :vc at startup
(advice-add #'use-package-ensure-elpa :around #'ignore)
(advice-add #'use-package-vc-install :around #'ignore)
(add-hook 'emacs-startup-hook
          (defun xy/restore-use-package-ensure ()
            (advice-remove #'use-package-ensure-elpa #'ignore)
            (advice-remove #'use-package-vc-install #'ignore)))

(provide 'init-package)
