;; -*- lexical-binding: t -*-

(require 'init-package)

;;; tool

(use-package elfeed
  :bind ("C-c j f" . elfeed)
  :config
  (setq elfeed-feeds
        '("https://nullprogram.com/feed" ; Chris Wellons 大量高质量的 Emacs Lisp 深度文章
          "https://coolshell.cn/feed" ; 左耳朵耗子
          "https://www.ruanyifeng.com/blog/atom.xml" ; 阮一峰 科技爱好者周刊
          "https://matklad.github.io/feed.xml" ; rust zig
          "https://manateelazycat.github.io/feed.xml" ; 懒猫
          "https://jvns.ca/atom.xml" ; Julia Evans（漫画式讲系统）
          "https://blog.codingnow.com/" ; 云风
          "https://rsshub.app/paulgraham/articles" ; Paul Graham（YC 创始人，创业 / 思维随笔）
          "https://drewdevault.com/blog/index.xml" ; sr.ht 创建者，开源治理、系统编程、Wayland
          "https://eli.thegreenplace.net/feeds/all.atom.xml" ; 编译器、Go、Python 内部机制，和 Chris Wellons 类似的 "从底层讲" 风格
          "https://tech.meituan.com/feed" ; 美团技术团队
          "https://hellogithub.com/rss"
          "https://tw93.fun/feed.xml" ; Kaku · Pake · MiaoYan · Waza · Kami · Mole
          "https://weekly.tw93.fun/rss.xml" ; https://github.com/tw93
          "https://newzone.top/atom.xml" ; LearnData 开源笔记
          "https://rss.aishort.top/?type=guokr" ; 果壳网
          "https://rss.aishort.top/?type=sspai" ; 少数派
          "https://rss.aishort.top/?type=zhihu" ; 知乎想法
          "https://rsshub.app/3dmgame/news"
          "https://rsshub.app/bilibili/weekly"
          "https://rsshub.app/infoq/recommend"
          "https://zed.dev/blog.rss"
          "https://neovim.io/news.xml"
          "https://ziglang.cc/learn/index.xml" "https://ziglang.cc/monthly/index.xml" "https://ziglang.cc/post/index.xml"
          "https://emacs.liujiacai.net/index.xml" "https://rusttalk.github.io/index.xml" ; "https://liujiacai.net/index.xml" "https://en.liujiacai.net/index.xml"
          "https://redguardtoo.github.io/categories/emacs.xml"
          "https://xenodium.com/feed"
          "https://karthinks.com/tags/emacs/index.xml"
          "https://emacsredux.com/atom.xml"
          "https://sachachua.com/blog/category/emacs-news/feed/"
          "https://planet.emacslife.com/atom.xml")))

;; (use-package elfeed-org)
;; (use-package elfeed-autotag)

(use-package wakatime-mode
  :defer 1
  :config
  (global-wakatime-mode +1))

;; (use-package pyim
;;   :defer 1
;;   :bind (("C-x C-\\" . pyim-convert-string-at-point)
;;          ("M-f" . pyim-forward-word)
;;          ("M-b" . pyim-backward-word))
;;   :config
;;   (setq default-input-method "pyim")
;;   ;; 拼音词库设置，五笔用户 *不需要* 此行设置
;;   (use-package pyim-basedict
;;     :config
;;     (pyim-basedict-enable))
;;   ;; 小鹤双拼
;;   (pyim-default-scheme 'xiaohe-shuangpin)
;;   ;; 使用云拼音(搜索引擎提供的云输入法服务)
;;   (setq pyim-cloudim 'baidu)
;;   ;; 设置 pyim 探针，可以实现 *无痛* 中英文切换 :-)
;;   ;; 1. 中英文动态切换规则：
;;   (setq-default pyim-english-input-switch-functions '(pyim-probe-dynamic-english
;;                                                       pyim-probe-isearch-mode
;;                                                       pyim-probe-program-mode
;;                                                       pyim-probe-org-structure-template))
;;   ;; 2. 半角标点动态切换规则：
;;   (setq-default pyim-punctuation-half-width-functions '(pyim-probe-punctuation-line-beginning
;;                                                         pyim-probe-punctuation-after-punctuation))
;;
;;   ;; 使用拼音搜索中文
;;   (pyim-isearch-mode +1))

;; Smart Input Source minimize manual switching input source (input method) in Emacs
;; (use-package sis
;;   :defer 1
;;   :config
;;   ;; Debug: (sis-get) (sis-switch)
;;   ;; (sis-log-mode +1)
;;   ;;
;;   (cond ;; see `sis-ism-lazyman-config'
;;    (xy/mac-p ;; brew install laishulu/homebrew/macism
;;     (unless (sis-get)
;;       (setq sis-english-source "com.apple.keylayout.UnicodeHexInput"))
;;     (setq sis-other-source "com.apple.inputmethod.SCIM.Shuangpin")))
;;   ;; 启用 /光标颜色/ 模式
;;   (sis-global-cursor-color-mode +1)
;;   ;; 启用 /respect/ 模式
;;   (sis-global-respect-mode +1)
;;   ;; 为所有缓冲区启用 /context/ 模式
;;   (sis-global-context-mode +1)
;;   ;; 为所有缓冲区启用 /inline english/ 模式
;;   (sis-global-inline-mode +1))

;; https://unclex.net/projects/launcher/
;; Combined with M-x.app to get a system-wide launcher that never leaves your editor
;; brew install --cask xiaoxinghu/tools/m-x
;; In M-x.app, settings:
;; - path: /opt/homebrew/bin/emacsclient -s ~/.xdg/emacs/server
;; - mapping: C-s-1 -> (select-frame-set-input-focus (selected-frame)) (launcher)
(use-package launcher
  :vc ( :url "https://github.com/xiaoxinghu/launcher.el"
        :rev :newest)
  :bind ("C-s-1" .  launcher))

(use-package quake-frame
  :vc ( :url "https://codeberg.org/ctietze/quake-frame.el"
        :rev :newest)
  :bind ("C-s-`" . quake-frame-toggle)
  :config
  (setq quake-frame-position 'top)
  ;; Size options accept three formats:
  ;; float (0.8 = 80% of screen), integer (1500 = pixels), or cons cell ((columns . 140) or (rows . 40)).
  (setq quake-frame-width 0.9)
  (setq quake-frame-height 0.4)
  ;; (setq quake-frame-width 40)
  ;; (setq quake-frame-height 20)
  ;; (setq quake-frame-width '(columns . 20))
  ;; (setq quake-frame-height '(rows . 20))
  (setq quake-frame-margin 0))

(use-package browser-hist
  :bind ("C-c b h" . browser-hist-search))

;; Merriam-Webster Thesaurus in Emacs, in `org-mode'
;; - RET or "C-c C-o" -> lookup for the word at the cursor (lets you “drill” into definition further)
;; - q -> kill mw-thesaurus buffer and close the window
(use-package mw-thesaurus
  :bind (("C-h y m" . mw-thesaurus-lookup-dwim)
         :map mw-thesaurus-mode-map
         ([remap evil-ret] . mw-thesaurus-lookup-at-point)
         ([remap evil-record-macro] . mw-thesaurus--quit))
  :hook (mw-thesaurus-mode . variable-pitch-mode)
  :config
  ;; Window on the right side
  (add-to-list 'display-buffer-alist
               `(,mw-thesaurus-buffer-name
                 (display-buffer-reuse-window
                  display-buffer-in-direction)
                 (direction . right)
                 (window . root)
                 (window-width . 0.3))))

;; Wiktionary browser in Emacs, in `org-mode'
;; 维基词典 - 词源查找 (Etymology Lookup)
;; - Wiktionary is an amazing dictionary, a comprehensive toolkit with main focus on word etymology
;; - C-c C-l -> to change the language and re-render the entry
;; @dep @cli Pronunciation gets played via ffplay (typically bundled in ffmpeg)
(use-package wiktionary-bro
  :bind ("C-h y w" . wiktionary-bro-dwim)
  :config
  (add-hook 'wiktionary-bro-mode-hook
            (defun xy/wiktionary-use-system-browser ()
              (setq-local browse-url-browser-function #'browse-url-default-browser))))

(use-core eww
  :config
  ;; https://emacs-china.org/t/eww-readable/22956
  ;; (setq eww-retrieve-command '("readable"))
  )

;; https://www.reddit.com/r/emacs/comments/1su4ips/getting_emacs_procedel_to_show_cpu_and_memory_on/
(use-core proced
  :config
  (setq-default proced-auto-update-flag 'visible)
  (setq proced-auto-update-interval 1)
  (setq proced-enable-color-flag t)
  (setq proced-tree-flag t)
  (setq proced-descend t)
  (setq proced-format 'medium) ;; can be changed interactively with `F'
  (setq proced-filter 'user)   ;; can be changed interactively with `f'
  )


;;; org
(use-core org
  ;; :defer 15
  :bind
  (("C-c o o" . #'xy/open-org-notes)
   ("C-c o d" . #'xy/open-org-dir)
   ("C-c o a" . #'org-agenda)
   ("C-c o c" . #'org-capture)
   ("C-c o l" . #'org-store-link)
   ("C-c o ;" . #'org-toggle-link-display)
   ("C-c o p" . #'org-publish)
   :map org-mode-map
   ;; @tip
   ;; "C-c C-o" ->
   ("C-c o t i" . org-indent-mode)
   ("C-c o t n" . org-num-mode))
  :init
  (defun xy/open-org-dir ()
    (interactive)
    (dired org-directory))
  (defun xy/open-org-notes ()
    "Visit the Org notes file."
    (interactive)
    (find-file org-default-notes-file))
  :config
  ;; @perf https://emacs-china.org/t/org/31278
  (setq org-modules nil)
  ;; (setq org-startup-folded 'content)
  ;; (setq org-startup-indented t)
  ;; (setq org-startup-numerated t)

  ;; (setq org-hide-emphasis-markers t)
  ;; (add-hook 'org-mode-hook #'visual-line-mode)

  (setq org-use-speed-commands t)
  (setq org-special-ctrl-a/e t
        org-special-ctrl-k t)

  ;; Let `org-goto' use completion
  (setq org-goto-interface 'outline-path-completion)
  ;; Flatten subheadings in `org-goto' completion
  (setq org-outline-path-complete-in-steps nil)

  ;; Alignment of tags at the end of headlines
  (setq org-auto-align-tags t
        org-tags-column 0)
  (setq org-reverse-note-order t) ; Put newer notes on top of the file
  (setq org-directory "~/org/"
        org-default-notes-file (concat org-directory "notes.org"))
  (setq org-todo-keywords ; Set some sensible default states for todo-items
        '((sequence "TODO(t)" "PROJ(p)" "LOOP(r)" "STRT(s)" "WAIT(w)" "HOLD(h)" "IDEA(i)" "|" "DONE(d)" "KILL(k)")
          (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
          (sequence "|" "OKAY(o)" "YES(y)" "NO(n)")))
  ;; (setq org-log-done 'note)
  (setq org-publish-timestamp-directory ; Where to place the directory containing the timestamps about changed files
        (concat user-emacs-directory "org-timestamps/"))
  (setq org-html-checkbox-type 'unicode
        org-html-prefer-user-labels t
        org-html-self-link-headlines t))

;; https://www.reddit.com/r/emacs/comments/18y85l9/orgmargin_mode/
(use-package org-margin
  :vc ( :url "https://github.com/rougier/org-margin"
        :rev :newest)
  ;; :hook org-mode
  :bind
  ("C-c o t m" . org-margin-mode))

(use-package denote
  :hook (dired-mode . denote-dired-mode)
  :bind (("C-c n n" . denote)
         ("C-c n o" . denote-open-or-create)
         ;; ("C-c n c" . denote-link-or-create)
         ("C-c n c" . denote-link-after-creating)
         ;;
         ("C-c n l" . denote-link)
         ("C-c n L" . denote-add-links)
         ;; ("C-c n b" . denote-backlinks)
         ("C-c n b" . denote-find-backlink-with-location)
         ;;
         ("C-c n r" . denote-rename-file)
         ("C-c n R" . denote-rename-file-using-front-matter)
         ("C-c n d" . denote-dired)
         ("C-c n g" . denote-grep)
         :map dired-mode-map
         ("C-c n i" . denote-dired-link-marked-notes)
         ("C-c n r" . denote-dired-rename-files)
         ("C-c n k" . denote-dired-rename-marked-files-with-keywords)
         ("C-c n R" . denote-dired-rename-marked-files-using-front-matter))
  :config
  (setq denote-directory (expand-file-name "~/denotes/"))
  (setq denote-file-type 'markdown-yaml)
  (setq denote-sort-keywords nil)
  (setq denote-date-prompt-use-org-read-date t)
  ;; Automatically rename Denote buffers instead of their long file name
  (denote-rename-buffer-mode +1)

  (defun xy/denote-always-rename-on-save-based-on-front-matter ()
    "Rename the current Denote file, if needed, upon saving the file.
Rename the file based on its front matter, checking for changes in the
title or keywords fields."
    (let ((denote-rename-confirmations nil)
          (denote-save-buffers t))      ; to save again post-rename
      (when (and buffer-file-name (denote-file-is-note-p buffer-file-name))
        (ignore-errors (denote-rename-file-using-front-matter buffer-file-name)))))

  (add-hook 'after-save-hook #'xy/denote-always-rename-on-save-based-on-front-matter))


;;; tty
(unless (display-graphic-p)
  (keymap-global-set "<mouse-4>" #'scroll-down-line)
  (keymap-global-set "<mouse-5>" #'scroll-up-line)
  (keymap-global-set "S-<mouse-4>" (defun xy/scroll-right () (interactive) (scroll-right 2)))
  (keymap-global-set "S-<mouse-5>" (defun xy/scroll-left () (interactive) (scroll-left 2)))
  (keymap-global-set "M-<mouse-4>" (defun xy/scroll-down++ () (interactive) (scroll-down-line 5)))
  (keymap-global-set "M-<mouse-5>" (defun xy/scroll-up++ () (interactive) (scroll-up-line 5)))

  (use-package xt-mouse
    :defer 0.5
    :config
    (xterm-mouse-mode +1))

  ;; NOTE: need xclip at linux
  ;; Allow Emacs to copy to and paste from the GUI clipboard when running in a text terminal
  (when (and xy/linux-p (executable-find "xclip"))
    (use-package xclip
      :defer 0.5
      :config
      (xclip-mode +1))))


;;; terminal

;; https://www.reddit.com/r/emacs/comments/17nl7cw/shout_out_to_the_eat_terminal_emulator_package/
;; 1. Input Mode (C-c C-e) = similar to vterm's copy mode the buffer becomes "frozen" for you to copy the text and scroll back and basically use all of emacs's nifty search features.
;; 2. Char Mode (C-c M-d) = One of my favorite modes where basically every input you make short of the keys C-M-m or M-RET will be sent to the terminal. This means I can open vim/nano/emacs -nw all within the terminal buffer (which I do a lot as I ssh into machines regulary) and it works absolutely perfectly.
;; 3. Semi-Char Mode: The default mode where most inputs will be sent to the terminal. This mode does 90% of the job but if sometimes you have a weird mix of alt and control input combinations to send then the Char Mode is there for you.
;;
;; @see https://abode.karthinks.com/share/eat-modes.png
(use-package eat
  :bind ("C-x c c" . eat)
  :init
  ;; Use Eat to handle term codes in program output
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  ;; Use Eat to handle `eshell-visual-commands'
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode))

(use-package ghostel
  :bind
  ("C-x c g" . ghostel))


;;; shell
;; @see https://www.masteringemacs.org/article/running-shells-in-emacs-overview
;; TODO https://emacsredux.com/blog/2026/03/17/tree-sitter-font-lock-and-indentation-in-comint-buffers/
(use-core comint
  :bind (("C-x c s" . shell)
         ("C-x c t" . ansi-term)
         :map comint-mode-map
         ;; @tip
         ;; "C-c C-p/C-n/C-a": Jump to the prev/next/last prompt
         ;; "C-c C-l": `comint-dynamic-list-input-ring'
         ;; "C-c C-x": `comint-get-next-from-history'
         ;; Auto subsitution: !! expands to the last command; ^a^b replaces a with b
         ("SPC" . #'comint-magic-space))
  :config
  (setq shell-command-prompt-show-cwd t)
  ;;
  (setq comint-input-ignoredups t
        comint-prompt-read-only t
        comint-scroll-to-bottom-on-input 'this
        comint-buffer-maximum-size (* 2 1024))
  (setq comint-history-isearch 'dwim)
  ;; (setq comint-input-autoexpand 'input)
  ;; Process the escape codes, e.g. "ls --hyperlink" will be made into clickable buttons
  (add-hook 'comint-output-filter-functions #'comint-osc-process-output))

;; @see https://www.masteringemacs.org/article/complete-guide-mastering-eshell
(use-core eshell
  :bind
  ("C-x c e" . eshell)
  :config
  (with-eval-after-load 'em-term
    (add-to-list 'eshell-visual-subcommands '("git" "log" "diff" "show" "ls"))
    (add-to-list 'eshell-visual-options '("git" "--help" "--paginate")))
  ;; Show help for quickly filtering files or elisp lists
  (with-eval-after-load 'em-alias
    (eshell/alias "ep" #'eshell-display-predicate-help)
    (eshell/alias "ef" #'eshell-display-modifier-help))
  ;; Enable bash keys (C-r, C-s, C-w, C-u)
  ;; (require 'em-rebind)
  ;; (eshell-rebind-initialize)
  ;; Plan 9 Smart Shell: improve the write-run-revise
  ;; (require 'em-smart)
  ;; (eshell-smart-initialize)

  ;; https://lambdaland.org/posts/2024-08-19_fancy_eshell_prompt/
  ;; (setq eshell-prompt-function 'xy/eshell-prompt)
  (defun xy/eshell-prompt ()
    "A pretty shell with git status"
    (require 'magit-git)
    (require 'magit-process)
    (let* ((cwd (abbreviate-file-name (eshell/pwd)))
           (ref (magit-get-shortname "HEAD"))
           (stat (magit-file-status))
           (x-stat eshell-last-command-status)
           (git-chunk
            (if ref
                (format "%s%s%s "
                        (propertize (if stat "[" "(") 'font-lock-face (list :foreground (if stat "red" "green")))
                        (propertize ref 'font-lock-face '(:foreground "yellow"))
                        (propertize (if stat "]" ")") 'font-lock-face (list :foreground (if stat "red" "green"))))
              "")))
      (propertize
       (format "%s %s %s$ "
               (if (< 0 x-stat) (format (propertize "!%s" 'font-lock-face '(:foreground "red")) x-stat)
                 (propertize "➤" 'font-lock-face (list :foreground (if (< 0 x-stat) "red" "green"))))
               (propertize cwd 'font-lock-face '(:foreground "#45babf"))
               git-chunk)
       'read-only t
       'front-sticky   '(font-lock-face read-only)
       'rear-nonsticky '(font-lock-face read-only)))))

;; @see https://www.masteringemacs.org/article/pcomplete-context-sensitive-completion-emacs
;; (use-package pcomplete)

(provide 'init-tool)
