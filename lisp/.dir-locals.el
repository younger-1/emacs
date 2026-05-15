;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((emacs-lisp-mode
  . ((eval . (add-hook 'after-save-hook
                       (lambda ()
                         (async-byte-compile-file buffer-file-name))
                       nil t)))))
