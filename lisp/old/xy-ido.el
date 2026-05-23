;; Interactively do things with buffers and files
;;
;; (info "(ido) Top")
;; https://www.masteringemacs.org/article/introduction-to-ido-mode
;; http://xahlee.info/emacs/emacs/emacs_ido_mode.html
;; https://www.emacswiki.org/emacs/InteractivelyDoThings
;; `ido-common-completion-map'
;; -- C-./C-,
;; -- C-s/C-r Moves to the next and previous match
;; -- C-@ Restricts the completion list to anything that matches your current input
;; -- C-p Toggles prefix matching (default is substring matching)
;; -- C-t Toggles matching by Emacs regular expression
;; `ido-file-dir-completion-map'
;; -- M-n/M-p Cycles through the next/previous work directories.
;; `ido-buffer-completion-map'
;; -- C-k ido-kill-buffer-at-head
(use-package ido
  :ensure nil
  :defer 0.2
  :config
  (ido-mode +1)
  (ido-everywhere +1)
  (setq ido-enable-flex-matching t) ;; after substring matching
  (setq ido-use-filename-at-point 'guess)
  (setq ido-create-new-buffer 'always)
  (setq ido-ignore-extensions t))

;; Imenu with ido-style completion
(use-package idomenu
  :after ido
  :bind ([remap imenu] . idomenu))

;; Real ido-everywhere
(use-package ido-completing-read+
  :after ido
  :config
  (ido-ubiquitous-mode +1))
