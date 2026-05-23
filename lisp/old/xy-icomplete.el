;; @see (info "(emacs) Icomplete")
;; http://xahlee.info/emacs/emacs/emacs_icomplete_mode.html
;; http://xahlee.info/emacs/emacs/emacs_fido_mode.html
(use-package icomplete
  :ensure nil
  :defer 0.2
  :config
  ;; -- Enable Icomplete’s in-buffer display for M-TAB (`completion-at-point'), and disable *Completions* buffer
  ;; -- 1.must set before `icomplete-mode' / `fido-mode'
  ;; -- 2.only for non-vertical version
  (progn
    (setq icomplete-in-buffer t)
    (advice-add 'completion-at-point :after #'minibuffer-hide-completions))
  ;;
  ;; (icomplete-mode +1)
  ;; (icomplete-vertical-mode +1)
  ;;
  ;; (fido-mode +1)
  (fido-vertical-mode +1)
  ;;
  ;; Do not delay displaying completion candidates
  (setq icomplete-compute-delay 0.01))
