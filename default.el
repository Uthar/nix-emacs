(eval-when-compile
  (require 'use-package)
  (setf use-package-expand-minimally t
        use-package-use-theme nil))

(rplaca mouse-wheel-scroll-amount 2)
(setf mouse-wheel-progressive-speed nil)
(put 'if 'lisp-indent-function 4)
(put 'if-let 'lisp-indent-function 4)

(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

(use-package tetris
  :commands tetris
  :config
  (define-key tetris-mode-map "z" 'tetris-rotate-next)
  (define-key tetris-mode-map "x" 'tetris-rotate-prev))

(use-package diminish)

(diminish 'auto-revert-mode)
(diminish 'eldoc-mode)

(use-package browse-kill-ring)

(use-package evil-matchit
  :after evil
  :config (global-evil-matchit-mode))

(use-package page-break-lines
  :diminish
  :config (global-page-break-lines-mode))

(use-package direnv
  :custom (direnv-always-show-summary nil)
  :config (direnv-mode))

(use-package rg)

(use-package wgrep)

(use-package hl-todo
  :custom
  (hl-todo-highlight-punctuation ":")
  :config (global-hl-todo-mode))

(use-package projectile
  :custom
  (projectile-enable-caching t)
  (projectile-completion-system 'ivy)
  (projectile-track-known-projects-automatically nil)
  :bind-keymap ("C-c p" . projectile-command-map)
  :config (projectile-mode))

(use-package efsl)

(use-package magit
  :custom
  (magit-completing-read-function 'ivy-completing-read)
  (magit-define-global-key-bindings nil)
  :hook
  (after-save . magit-after-save-refresh-status)
  :config
  (dotimes (i 4)
    (let ((n (1+ i)))
      (define-key magit-section-mode-map (kbd (format "M-%i" n)) nil)
      (define-key magit-section-mode-map (kbd (format "C-%i" n))
        (intern (format "magit-section-show-level-%i-all" n))))))

(defun toggle-hook (hook function)
  (if (and (consp (symbol-value hook))
           (memq function (symbol-value hook)))
      (remove-hook hook function)
      (add-hook hook function)))

(defun vc-annotate-toggle-annotation-visibility* ()
  (toggle-hook 'vc-annotate-mode-hook 'vc-annotate-toggle-annotation-visibility))

(use-package vc-annotate
  :commands vc-annotate
  :config
  (define-key vc-annotate-mode-map (kbd "v")
    (lambda ()
      (interactive)
      (vc-annotate-toggle-annotation-visibility)
      (vc-annotate-toggle-annotation-visibility*))))

(use-package winum
  :config
  (require 'term)
  (dotimes (i 9)
    (let* ((n (1+ i))
           (key (kbd (format "M-%i" n)))
           (command (intern (format "winum-select-window-%i" n))))
      (define-key diff-mode-map key nil)
      (define-key term-raw-map key command)
      (global-set-key key command)))
  (winum-mode))

(defun dired-toggle-hidden ()
  (interactive)
  (if (string-match-p "a" dired-actual-switches)
      (dired "." (remove ?a dired-listing-switches))
      (dired "." (concat dired-listing-switches "a")))
  (setf dired-listing-switches dired-actual-switches))

(use-package dired
  :custom
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-listing-switches "--group-directories-first -lh")
  :config
  (define-key dired-mode-map (kbd "M-h") 'dired-toggle-hidden)
  (define-key dired-mode-map "N" nil)
  (define-key dired-mode-map "n" nil)
  (define-key dired-mode-map [mouse-1] 'dired-find-file)
  :hook
  (dired-mode . dired-hide-details-mode)
  (dired-mode
   . (lambda ()
       (setq-local mouse-1-click-follows-link nil)
       (evil-local-set-key 'normal "l" 'dired-find-file)
       (evil-local-set-key 'normal "h" 'dired-up-directory))))

(use-package diff
  :custom (diff-font-lock-syntax nil)
  :hook
  (diff-mode
   . (lambda ()
       (setq-local require-final-newline nil)
       (setq-local before-save-hook nil))))

(use-package ivy
  :diminish
  :config (ivy-mode t))

(use-package counsel
  :diminish
  :config (counsel-mode))

(defun state-dir (dir)
  (expand-file-name (concat user-emacs-directory dir "/")))

(defmacro with-inhibit-message (&rest body)
  `(let ((inhibit-message t))
     ,@body))

(use-package emacs
  :custom
  (completions-detailed t)
  (read-minibuffer-restore-windows nil)
  (mode-line-compact 'long)
  (enable-dir-local-variables nil)
  (enable-local-variables nil)
  (server-client-instructions nil)
  (use-short-answers t)
  (create-lockfiles nil)
  (enable-recursive-minibuffers t)
  (custom-file null-device)
  (use-dialog-box nil)
  (require-final-newline t)
  (display-line-numbers-type 'relative)
  (scroll-margin 4)
  (scroll-conservatively 1000)
  (initial-scratch-message "")
  (initial-buffer-choice t)
  (version-control t)
  (delete-old-versions t)
  (backup-directory-alist `((".*" . ,(state-dir "backups"))))
  (auto-save-file-name-transforms `((".*" ,(state-dir "auto-save") t)))
  (auto-save-list-file-prefix (state-dir "auto-save"))
  (indent-tabs-mode nil)
  (tab-width 4)
  (column-number-mode t)
  (line-number-mode t)
  :config
  (minibuffer-depth-indicate-mode t)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (tooltip-mode -1)
  (save-place-mode t)
  (show-paren-mode t)
  (winner-mode t)
  (recentf-mode)
  (savehist-mode)
  (context-menu-mode)
  (advice-add 'display-startup-echo-area-message :around 'identity)
  (set-language-environment "UTF-8")
  :hook
  (before-save . delete-trailing-whitespace)
  (after-save . executable-make-buffer-file-executable-if-script-p)
  (prog-mode . display-line-numbers-mode))

;;;; recentf

(defun recentf-save-file-p (file)
  (string= file (expand-file-name recentf-save-file)))

(add-to-list 'recentf-exclude 'recentf-save-file-p)
(add-to-list 'recentf-exclude (regexp-opt '("ci-comment-")))

(defun recentf-save-current-buffer ()
  (let ((file-name (buffer-file-name (current-buffer))))
    (when file-name
      (recentf-add-file file-name)
      (with-inhibit-message (recentf-save-list)))))

(add-hook 'buffer-list-update-hook 'recentf-save-current-buffer)

;;;;

(use-package evil
  :custom
  (evil-want-C-u-scroll t)
  (evil-kill-on-visual-paste nil)
  (evil-undo-system 'undo-redo)
  (evil-want-keybinding nil)
  :config
  (evil-set-initial-state 'help-mode 'emacs)
  (evil-mode 1)
  (evil-global-set-key 'insert (kbd "C-r") nil)
  (dolist (state '(motion insert))
    (evil-global-set-key state (kbd "C-e") nil))
  (evil-global-set-key 'insert (kbd "C-a") nil)
  (evil-global-set-key 'motion (kbd "C-i") nil)
  (evil-global-set-key 'insert (kbd "C-k") nil)
  (evil-global-set-key 'normal (kbd "M-.") nil))

(use-package xref
  :hook (xref-after-update . evil-emacs-state))

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-company-use-tng nil)
  :config
  (evil-collection-magit-setup)
  (evil-collection-dired-setup)
  (evil-collection-help-setup)
  (evil-collection-company-setup)
  (evil-collection-term-setup))


(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode 1))

(use-package anzu
  :diminish
  :config (global-anzu-mode t))

(use-package evil-anzu
  :after evil)

(use-package company
  :custom
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case t)
  (company-minimum-prefix-length 1)
  (company-show-numbers 'left)
  :diminish
  :hook (after-init . global-company-mode))

(use-package which-key
  :diminish
  :custom (which-key-dont-use-unicode t)
  :config (which-key-mode))

(use-package editorconfig
  :diminish
  :config (editorconfig-mode t))

;; remove ?
(use-package flycheck
  :custom (flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :diminish
  :config (global-flycheck-mode))


;;;; programming language support

(use-package glsl-mode)

(use-package terraform-mode
  :mode "\\.tf\\'")

(use-package company-terraform
  :after (company terraform-mode)
  :config (company-terraform-init))

(use-package yaml-mode
  :hook (yaml-mode . display-line-numbers-mode))

(use-package nix-mode
  :mode "\\.nix\\'"
  :config

  (defun nix-prefetch-tarball-at-point ()
    (interactive)
    (let ((hash (shell-command-to-string
                 (concat "nix-prefetch-url --unpack "
                         (ffap-string-at-point)
                         " 2> /dev/null"))))
      (kill-new (string-trim hash))
      (message "Copied %s to kill ring" (string-trim hash))))

  (define-key nix-mode-map (kbd "C-x n h") 'nix-prefetch-tarball-at-point))

(use-package groovy-mode
  :mode "\\.groovy\\'")

(use-package go-mode)

(use-package cider
  :custom
  (cider-show-error-buffer 'except-in-repl)
  :config
  (define-key cider-repl-mode-map (kbd "C-c M-o") 'cider-repl-clear-buffer)
  (evil-set-initial-state 'cider-stacktrace-mode 'emacs)
  :hook
  ((clojure-mode clojurescript-mode)
   . (lambda ()
       ;; should fix slime-company itself
       (setq-local company-backends (remove 'company-slime company-backends)))))

(use-package lsp-mode
  :commands lsp
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-restart 'ignore)
  :hook
  (((c-mode c++-mode python-mode go-mode) . lsp)
   (lsp-mode . lsp-enable-which-key-integration))
  :config
  (defun direnv-update-advice (&rest r)
    (direnv-update-directory-environment))
  (advice-add 'lsp :before 'direnv-update-advice))

(use-package lsp-python-ms
  :after lsp-mode
  :config
  (defun set-python-ms-executable (&rest r)
    (setf lsp-python-ms-executable (executable-find "python-language-server")))
  (advice-add 'lsp :before 'set-python-ms-executable '((depth . 100))))

(use-package python
  :commands python-mode
  :config
  (advice-add 'python-shell-make-comint :around 'call-with-repl-window)
  (advice-add 'python-shell-switch-to-shell :around 'call-with-repl-window))

(use-package modus-themes
  :config (modus-themes-load-themes)
  :hook (after-init . modus-themes-load-operandi)
  :bind ("C-c t" . modus-themes-toggle))

(use-package slime
  :commands slime
  :custom
  (slime-truncate-lines nil)
  (slime-net-coding-system 'utf-8-unix)
  (inferior-lisp-program "sbcl --disable-ldb --dynamic-space-size 4096")
  (slime-contribs '(slime-asdf slime-company slime-quicklisp slime-fancy))
  (slime-company-completion 'fuzzy)
  (slime-repl-auto-right-margin t)
  (slime-repl-history-size 10000)
  (common-lisp-hyperspec-root "@clhs@/HyperSpec/")
  (common-lisp-hyperspec-symbol-table "@clhs@/HyperSpec/Data/Map_Sym.txt")
  :bind ("C-c s" . 'slime-selector)
  :config
  (add-to-list 'evil-buffer-regexps
               (cons (regexp-opt '("*slime-description*")) 'emacs))
  (advice-add 'slime :around 'call-with-repl-window)
  (advice-add 'slime-repl :around 'call-with-repl-window)
  (bind-key (kbd "C-c C-z") 'slime-repl 'slime-mode-map)
  (bind-key (kbd "C-c h") 'slime-hyperspec-lookup 'slime-mode-map)
  :hook
  ;; Disable annoying tab completion buffers.
  ;; Careful: both slime-repl and inferior-slime set this.
  ;; With M-x slime this is enough because only slime-repl is loaded
  ;; Probably wouldn't work if using comint (but who would want to?)
  (slime-repl-mode . (lambda () (setq-local tab-always-indent t))))

(use-package slime-repl
  :after slime
  :config
  (defslime-repl-shortcut nil ("delete-package" "dp")
    (:handler (lambda ()
                (interactive)
                (let ((package (slime-read-package-name "Package: ")))
                  (slime-repl-shortcut-eval `(cl:delete-package ,package)))))
    (:one-liner "Delete a package.")))

(use-package slime-presentations
  :after slime
  :config
  (define-key slime-presentation-map [mouse-1] 'slime-inspect-presentation-at-mouse))

(use-package slime-cl-indent
  :after slime
  :config
  (setf
   common-lisp-style-default
   (define-common-lisp-style "kpg"
     "Fix the indentation of some Clojure-like macros."
     (:inherit "modern")
     (:indentation
      (-> (as if))
      (->> (as if))
      (hash-keys-bind (as destructuring-bind))
      (dovec (as dolist))))))

(use-package lisp-mode
  :config
  (modify-syntax-entry ?\[ "(]" lisp-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" lisp-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}" lisp-mode-syntax-table)
  (modify-syntax-entry ?\} "){" lisp-mode-syntax-table))

(use-package paredit
  :bind (("M-(" . paredit-wrap-round)
         ("M-{" . paredit-wrap-curly)
         ("M-[" . paredit-wrap-square)
         ("M-\"" . paredit-meta-doublequote)
         ("M-s" . paredit-splice-sexp)
         ("M-r" . paredit-raise-sexp)
         ("M-q" . paredit-reindent-defun)
         ("C-<right>" . paredit-forward-slurp-sexp)
         ("C-<left>" . paredit-forward-barf-sexp)))

;; repl window

(defvar repl-window nil)
(defvar repl-window-height 15)
(defvar last-repl-buffer nil)

(defun repl-window-selected? ()
  (and repl-window (eq repl-window (selected-window))))

(defun save-last-repl-buffer (frame)
  (when (repl-window-selected?)
    (setf last-repl-buffer (window-buffer repl-window))))

(add-to-list 'window-buffer-change-functions 'save-last-repl-buffer)

(defun save-repl-window-height (frame)
  (when repl-window
    (setf repl-window-height (window-height repl-window))))

(add-to-list 'window-size-change-functions 'save-repl-window-height)

(defun default-repl-buffer ()
  (ansi-term "bash"))

(defun open-repl-window ()
  (setf repl-window
        (split-window
         (frame-root-window)
         (- (min repl-window-height
                 (- (window-height (frame-root-window)) window-min-height)))))
  (select-window repl-window)
  (switch-to-buffer (or last-repl-buffer (default-repl-buffer))))

(defun close-repl-window ()
  (ignore-errors (delete-window repl-window))
  (setf repl-window nil))

(defun toggle-repl-window ()
  (interactive)
  (if repl-window
      (close-repl-window)
      (open-repl-window)))

(defun ensure-repl-window ()
  (if (window-live-p repl-window)
      (select-window repl-window)
      (open-repl-window)))

(defun open-buffer-in-repl-window (buffer)
  (ensure-repl-window)
  (switch-to-buffer buffer))

(defun call-with-repl-window (fn &rest args)
  (open-buffer-in-repl-window (save-window-excursion (apply fn args))))

(advice-add 'ansi-term :around 'call-with-repl-window)
(advice-add 'eshell :around 'call-with-repl-window)
(advice-add 'cider-repl-create :around 'call-with-repl-window)
(advice-add 'cider-switch-to-repl-buffer :around 'call-with-repl-window)

;; search

(setenv "FZF_DEFAULT_COMMAND" "fd -LH")

(defun universal-argument-provided? ()
  (>= (prefix-numeric-value current-prefix-arg) 4))

(defun guess-directory (cmd-name)
  (if (universal-argument-provided?)
      (counsel-read-directory-name (concat cmd-name " in directory: "))
      (or (if (featurep 'projectile) (projectile-project-root))
          default-directory)))

(defun counsel-fzf-in-project ()
  (interactive)
  (let ((dir (guess-directory "fzf")))
    (counsel-fzf "" dir (concat "fzf in " dir ": "))))

(defun counsel-ag-in-project ()
  (interactive)
  (let ((dir (guess-directory "ag")))
    (counsel-ag "" dir " --hidden --follow " (concat "ag in " dir ": "))))


;;;; generic utilities

(defun my/comment-or-uncomment ()
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
  (setq deactivate-mark nil))

(global-set-key (kbd "C-;") 'my/comment-or-uncomment)

(defun x-copy ()
  (interactive)
  (when (region-active-p)
    (shell-command-on-region (region-beginning) (region-end) "xsel -i -b")
    (message "Yanked region to clipboard")
    (deactivate-mark)))

(defun x-paste ()
  (interactive)
  (insert (shell-command-to-string "xsel -o -b")))

(defun select-or-exit-minibuffer ()
  (interactive)
  (if (universal-argument-provided?)
      (exit-minibuffer))
      (select-window (minibuffer-window)))

(defun bash ()
  (interactive)
  (ansi-term "bash"))

(defun xdg-open ()
  (interactive)
  (start-process "xdg-open" nil "xdg-open" (ffap-string-at-point)))

;; keys

(bind-keys* ("<f1>" . toggle-repl-window)
            ("<f2>" . dired-jump)
            ("<f3>" . counsel-fzf-in-project)
            ("<f4>" . counsel-ag-in-project)
            ("<f5>" . previous-buffer)
            ("<f6>" . next-buffer)
            ("<f7>" . recentf-open-files)
            ("<f8>" . select-or-exit-minibuffer)
            ("<f9>" . kill-current-buffer)
            ("<f10>" . delete-window)
            ("<f11>" . kill-buffer-and-window)
            ("<f12>" . universal-argument))

(require 'notifications)
