(use-package god-mode
  :defer 0.5
  :bind (("<escape>" . god-local-mode)
         :map god-local-mode-map
         ("." . #'repeat))
  :init
  (setq god-mode-enable-function-key-translation nil)
  :config
  (god-mode)
  ;; Visual indicators for God mode
  ;; (set-face-attribute 'god-mode-lighter nil :inherit 'match)
  ;;
  (defun xy/god-mode-update-cursor-type ()
    (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))
  (add-hook 'post-command-hook #'xy/god-mode-update-cursor-type)
  ;;
  ;; (defun xy/god-mode-update-mode-line ()
  ;;   (cond
  ;;    (god-local-mode
  ;;     (set-face-attribute 'mode-line nil
  ;;                         :foreground "#604000"
  ;;                         :background "#fff29a")
  ;;     (set-face-attribute 'mode-line-inactive nil
  ;;                         :foreground "#3f3000"
  ;;                         :background "#fff3da"))
  ;;    (t
  ;;     (set-face-attribute 'mode-line nil
  ;;       		  :foreground "#0a0a0a"
  ;;       		  :background "#d7d7d7")
  ;;     (set-face-attribute 'mode-line-inactive nil
  ;;       		  :foreground "#404148"
  ;;       		  :background "#efefef"))))
  (defun xy/god-mode-update-mode-line ()
    (cond (god-local-mode
           (progn
             (set-face-background 'mode-line "#e9e2cb")
             (set-face-foreground 'mode-line "black")
             (set-face-background 'mode-line-inactive "#e9e2cb")
             (set-face-foreground 'mode-line-inactive "black")))
          (t (progn
               (set-face-background 'mode-line "#0a2832")
               (set-face-foreground 'mode-line "white")
               (set-face-background 'mode-line-inactive "#0a2832")
               (set-face-foreground 'mode-line-inactive "white")))))
  (add-hook 'post-command-hook #'xy/god-mode-update-mode-line)
  ;; isearch integration
  (require 'god-mode-isearch)
  (define-key isearch-mode-map (kbd "<escape>") #'god-mode-isearch-activate)
  (define-key god-mode-isearch-map (kbd "<escape>") #'god-mode-isearch-disable))
