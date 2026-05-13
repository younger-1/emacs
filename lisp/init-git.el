;; -*- lexical-binding: t -*-

;;; vc
(use-core vc
  :config
  (setq vc-git-diff-switches '("--histogram")))


;;; git
(use-package magit
  ;; :defer 10
  :bind (;; ("C-x g"   . magit-status)
         ;; ("C-x C-g" . magit-dispatch)
         ;; ("C-x M-g" . magit-file-dispatch)
         ;; ("C-x 5 g" . xy/magit-status-other-frame)
         ("C-x g g" . magit-status)
         ("C-x g a" . magit-log-all)
         ("C-x g b" . magit-branch-checkout)
         ("C-x g c" . magit-log-current)
         ("C-x g C" . magit-log-buffer-file)
         ("C-x g '" . magit-blame-addition)
         ("C-x g d" . magit-diff-dwim)
         ("C-x g D" . magit-diff-buffer-file)
         ;;
         ("C-x m" . nil)
         ("C-x m m" . magit-dispatch)
         ("C-x m f" . magit-file-dispatch)
         ("C-x p m" . magit-project-status)
         ("C-x m l" . magit-log)
         ("C-x m b" . magit-branch)
         ("C-x m '" . magit-blame)
         ("C-x m d" . magit-diff)
         ("C-x m e" . magit-ediff-dwim)
         ("C-x m E" . magit-ediff)
         ("C-x m z" . magit-stash)
         ("C-x m p" . magit-pull)
         ("C-x m P" . magit-push)
         ("C-x m r" . magit-rebase)
         ("C-x m M" . magit-merge)
         :map magit-section-mode-map
         ("]" . magit-section-forward-sibling)
         ("[" . magit-section-backward-sibling))
  :config
  (setq magit-status-goto-file-position t)
  ;; (setq magit-status-margin '(t age magit-log-margin-width t 18))
  ;; (add-to-list 'magit-blame-styles
  ;;              '(margin
  ;;                (margin-format    . (" %s%f" " %C %a" " %H"))
  ;;                (margin-width     . 42)
  ;;                (margin-face      . magit-blame-margin)
  ;;                (margin-body-face . (magit-blame-dimmed))))

  ;; (setq magit-log-auto-more t)
  (setq magit-diff-refine-hunk t)
  ;; Enable gravatars when viewing commits. The service used by default is [Libgravatar](https://www.libravatar.org/).
  (setq magit-revision-show-gravatars t)
  ;; Order for branch checkout: objectsize, authordate, committerdate, creatordate, taggerdate
  (setq magit-list-refs-sortby "-creatordate")
  (add-to-list 'savehist-additional-variables 'magit-revision-history)
  (setq magit-repository-directories '(("~/notes" . 0)
                                       ("~/dotter" . 0)
                                       ("~/work" . 1)))
  (setq git-commit-use-local-message-ring t))

(use-package git-timemachine
  :bind ("C-c g h" . git-timemachine))

(use-package git-link
  :bind (("C-c g l l" . git-link) ; double @prefix to reverse `git-link-use-commit'
         ("C-c g l h" . git-link-homepage) ; double @prefix to enable `git-link-open-in-browser'
         ("C-c g l c" . git-link-commit)
         ;; Transient menu
         ("C-c g L" . git-link-dispatch))
  :config
  (setq git-link-use-commit t)
  ;; (setq git-link-open-in-browser t)

  (defun xy/git-link-byted (hostname dirname filename branch commit start end)
    (format "%s/%s/blob/%s/%s"
            hostname
            dirname
            (or branch commit)
            (concat filename
                    (when start
                      (concat "#"
                              (if end
                                  (format "L%s-%s" start end)
                                (format "L%s" start)))))))
  ;; (add-to-list 'git-link-web-host-alist
  ;;              '("byted" . "bits.bytedance.net/code"))
  (add-to-list 'git-link-remote-alist
               '("byted" xy/git-link-byted))
  (add-to-list 'git-link-commit-remote-alist
               '("byted" git-link-commit-github))
  (add-to-list 'git-link-homepage-remote-alist
               '("byted" git-link-homepage-github)))

(use-package git-messenger
  :bind ("C-c g m" . git-messenger:popup-message)
  :config
  ;; Enable `magit-show-commit' instead of `pop-to-buffer'
  (setq git-messenger:use-magit-popup t)
  (setq git-messenger:show-detail t))

(use-package remoto
  :vc ( :url "https://github.com/agzam/remoto.el"
        :rev :newest)
  :bind ("C-c g r" . remoto-browse))


;;; diff
(use-core diff
  :bind
  ("C-c d b" . #'diff-buffer-with-file)
  ("C-c d B" . #'diff-buffers)
  ("C-c d k" . xy/diff-last-two-kills)
  :config
  ;; @see https://irreal.org/blog/?p=12704
  (defun xy/diff-last-two-kills (&optional ediff?)
    "Diff last couple of things in the kill-ring. With prefix open ediff."
    (interactive "P")
    (require 'ediff)
    (let* ((old "/tmp/old-kill")
           (new "/tmp/new-kill")
           (prev-ediff-quit-hook ediff-quit-hook))
      (cl-flet ((kill-temps ()
                  (dolist (f (list old new))
                    (kill-buffer (find-buffer-visiting f)))
                  (setq ediff-quit-hook prev-ediff-quit-hook)))
        (with-temp-file new
          (insert (current-kill 0 t)))
        (with-temp-file old
          (insert (current-kill 1 t)))
        (if ediff?
            (progn
              (add-hook 'ediff-quit-hook #'kill-temps)
              (ediff old new))
          (diff old new "-u" t)))))
  (setq diff-refine 'font-lock)
  (setq diff-font-lock-prettify nil)
  (setq diff-font-lock-syntax t))

;; @see https://irreal.org/blog/?p=11780
;; @see https://emacs.stackexchange.com/questions/51424/how-can-i-diff-two-long-lines-from-the-same-buffer
(use-core ediff
  :bind
  ("C-c d c" . compare-windows)         ; @tip Use C-x z z z ... to repeat it
  ("C-c d w" . ediff-windows-wordwise)
  ("C-c d w" . ediff-windows-linewise)
  ("C-c d r" . ediff-regions-wordwise)
  ("C-c d R" . ediff-regions-linewise)
  :config
  ;; Use a single frame
  (setq ediff-window-setup-function #'ediff-setup-windows-plain)
  ;; Split windows horizontally
  (setq ediff-split-window-function #'split-window-horizontally))

;; Highlight uncommitted changes using VC
(use-package diff-hl
  ;; :defer 0.5
  :hook
  (find-file . diff-hl-mode)
  (dired-mode . diff-hl-dired-mode)
  (vc-dir-mode . diff-hl-dir-mode)
  :bind ( :map diff-hl-command-map
          ("." . diff-hl-amend-mode)
          ("-" . diff-hl-set-reference-rev)
          ("_" . diff-hl-reset-reference-rev)
          ("RET" . diff-hl-show-hunk)
          ("SPC" . diff-hl-mark-hunk)
          ("n" . diff-hl-next-hunk)
          ("p" . diff-hl-previous-hunk)
          ("M-s" . #'xy/toggle-diff-hl-show-staged-changes))
  :custom-face
  ;; (diff-hl-change ((t (:inherit custom-changed :foreground unspecified :background unspecified))))
  ;; (diff-hl-insert ((t (:inherit diff-added :background unspecified))))
  ;; (diff-hl-delete ((t (:inherit diff-removed :background unspecified))))
  :config
  ;; (global-diff-hl-mode +1)
  ;; Makes fringe and margin react to mouse clicks
  ;; (global-diff-hl-show-hunk-mouse-mode +1)
  ;; Diffing on-the-fly (i.e. without saving the buffer first)
  (diff-hl-flydiff-mode +1)

  (setq diff-hl-update-async t)
  (setq diff-hl-show-staged-changes nil)

  (defun xy/toggle-diff-hl-show-staged-changes ()
    (interactive)
    (if diff-hl-show-staged-changes
        (setq diff-hl-show-staged-changes nil)
      (setq diff-hl-show-staged-changes t))
    (diff-hl-magit-post-refresh))

  (unless (display-graphic-p)
    ;; Fall back to the display margin since the fringe is unavailable in tty
    (diff-hl-margin-mode +1)
    ;; Avoid restoring `diff-hl-margin-mode'
    (with-eval-after-load 'desktop
      (add-to-list 'desktop-minor-mode-table '(diff-hl-margin-mode nil))))

  (defun xy/diff-hl-fringe-bmp-function (_type _pos)
    (define-fringe-bitmap 'my-diff-hl-bmp
      (vector (if xy/linux-p #b11111100 #b11100000)) 1 8 '(center t)))
  ;; (setq diff-hl-fringe-bmp-function #'xy/diff-hl-fringe-bmp-function)

  ;; Integration with magit
  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

;; Adapted from Tassilo Horn's blog post:
;; https://www.tsdh.org/posts/2022-07-20-using-eldoc-with-magit-async.html
(use-package eldoc-diffstat
  :after (eldoc magit) :demand t
  :config
  (global-eldoc-diffstat-mode +1)
  (eldoc-add-command
   'magit-next-line 'magit-previous-line
   'magit-section-forward 'magit-section-backward
   'magit-section-forward-sibling 'magit-section-backward-sibling))

;; Enhanced diff of all Magit buffers
;; Make magit's diff have syntax highlight, like `vc-diff'
;; TODO: https://github.com/dandavison/magit-delta/issues/9
(use-package magit-delta
  ;; :if (executable-find "delta")
  :hook (magit-mode . magit-delta-mode)
  :config
  ;; @see https://github.com/dandavison/magit-delta/issues/13#issuecomment-690534938
  ;; --line-numbers/--side-by-side cannot be used with magit-delta since it creates invalid patches
  ;; (setq magit-delta-delta-args '("--max-line-distance" "0.6" "--true-color" "always" "--color-only"))
  ;; (add-to-list 'magit-delta-delta-args "--diff-highlight")
  ;; (add-to-list 'magit-delta-delta-args "--diff-so-fancy")
  (add-to-list 'magit-delta-delta-args "--no-gitconfig")
  ;;
  (defun xy/toggle-magit-delta ()
    (interactive)
    (magit-delta-mode 'toggle)
    (magit-refresh))
  (transient-append-suffix 'magit-diff '(-1 -1 -1)
    '("l" "Toggle magit-delta" xy/toggle-magit-delta)))

;; Adapted from Tassilo Horn's blog post:
;; https://tsdh.org/posts/2022-08-01-difftastic-diffing-with-magit.html
;; https://shivjm.blog/better-magit-diffs/
(use-package difftastic
  :after magit-status :demand t
  :config
  ;; @see `difftastic-bindings-alist'
  (difftastic-bindings-mode +1))

;; Enhanced diff of Magit's revision buffers
;; Enable side-by-side diff display
;; (use-package diff-ansi
;;   ;; :if (executable-find "delta")
;;   :hook (magit-mode . diff-ansi-mode)
;;   :commands diff-ansi-buffer)

;; (use-package diffview
;;   :commands (diffview-current diffview-region))

(provide 'init-git)
