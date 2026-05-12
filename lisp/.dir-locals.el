((emacs-lisp-mode
  . ((eval . (add-hook 'after-save-hook
                       (lambda ()
                         (byte-compile-file buffer-file-name))
                       nil t)))))
