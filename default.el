;; -*- lexical-binding: t -*-

;; Make startup faster by inhibiting GC
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
  (lambda ()
    (message "booted in %s" (emacs-init-time))
    (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value)))))

;; TODO delete
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

(use-package evil-matchit
  :after evil
  :config (global-evil-matchit-mode))

(use-package direnv
  :custom (direnv-always-show-summary nil)
  :hook (after-init . direnv-mode))

(add-hook 'prog-mode-hook
  (lambda ()
    (font-lock-add-keywords nil
	  '(("TODO" 0 '(:foreground "red" :weight bold) t)
        ("NOTE" 0 '(:foreground "dark green" :weight bold) t)))))

(use-package efsl
  :commands efsl)

(use-package magit
  :commands magit
  :custom
  (magit-completing-read-function 'ivy-completing-read)
  (magit-define-global-key-bindings nil)
  :hook
  (magit-mode
   . (lambda ()
       (dotimes (i 4)
         (let ((n (1+ i)))
           (define-key magit-section-mode-map (kbd (format "M-%i" n)) nil)
           (define-key magit-section-mode-map (kbd (format "C-%i" n))
             (intern (format "magit-section-show-level-%i-all" n))))))))

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
  :defer
  :config
  (require 'term)
  (dotimes (i 9)
    (let* ((n (1+ i))
           (key (kbd (format "M-%i" n)))
           (command (intern (format "winum-select-window-%i" n))))
      (define-key diff-mode-map key nil)
      (define-key term-raw-map key command)
      (global-set-key key command)))
  :hook
  (after-init . winum-mode))

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
       (local-set-key "j" 'dired-next-line)
       (local-set-key "k" 'dired-previous-line)
       (local-set-key "l" 'dired-find-file)
       (local-set-key "h" 'dired-up-directory))))

(use-package diff
  :defer
  :custom (diff-font-lock-syntax nil)
  :hook
  (diff-mode
   . (lambda ()
       (setq-local require-final-newline nil)
       (setq-local before-save-hook nil))))

(use-package ivy
  :diminish
  :hook (after-init . ivy-mode))

(use-package counsel
  :diminish
  :hook (after-init . counsel-mode))

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
  (after-init . (lambda () (load-theme 'wombat)))
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
  (evil-move-beyond-eol t)
  :hook
  (prog-mode . evil-local-mode)
  :config
  (evil-global-set-key 'insert (kbd "C-[") 'evil-force-normal-state)
  (dolist (state '(motion insert))
    (evil-global-set-key state (kbd "C-e") nil))
  (evil-global-set-key 'insert (kbd "C-a") nil)
  (evil-global-set-key 'insert (kbd "C-k") nil)
  (dolist (state '(normal motion insert))
    (evil-global-set-key state (kbd "C-f") nil)
    (evil-global-set-key state (kbd "C-b") nil)
    (evil-global-set-key state [remap cua-paste-pop] nil)
    (evil-global-set-key state [remap yank-pop] nil)
    (evil-global-set-key state (kbd "C-n") nil)
    (evil-global-set-key state (kbd "C-p") nil))
  (evil-global-set-key 'normal (kbd "M-.") nil))

(use-package org
  :defer
  :hook
  (org-mode
   . (lambda ()
       (evil-local-mode)
       (evil-local-set-key 'motion (kbd "<tab>") 'org-cycle))))

(use-package anzu
  :diminish
  :after evil
  :config (global-anzu-mode t))

(use-package evil-anzu
  :after anzu)

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
  :hook (after-init . which-key-mode))

(use-package editorconfig
  :diminish
  :hook (after-init . editorconfig-mode))

;; remove ?
(use-package flycheck
  :custom (flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :diminish
  :hook (prog-mode . flycheck-mode))


;;;; programming language support

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

(use-package cider
  :commands (cider-jack-in-clj cider-jack-in-cljs
             cider-connect-clj cider-connect-cljs)
  :custom
  (cider-show-error-buffer 'except-in-repl)
  :config
  (define-key cider-repl-mode-map (kbd "C-c M-o") 'cider-repl-clear-buffer)
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
  :defer
  :config
  (defun set-python-ms-executable (&rest r)
    (setf lsp-python-ms-executable (executable-find "python-language-server")))
  (advice-add 'lsp :before 'set-python-ms-executable '((depth . 100))))

(use-package python
  :commands python-mode
  :config
  (advice-add 'python-shell-make-comint :around 'call-with-repl-window)
  (advice-add 'python-shell-switch-to-shell :around 'call-with-repl-window))

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
  (advice-add 'slime :around 'call-with-repl-window)
  (advice-add 'slime-repl :around 'call-with-repl-window)
  (define-key slime-mode-map (kbd "C-c C-z") 'slime-repl)
  (define-key slime-mode-map (kbd "C-c h") 'slime-hyperspec-lookup)
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
  :defer
  :config
  (modify-syntax-entry ?\[ "(]" lisp-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" lisp-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}" lisp-mode-syntax-table)
  (modify-syntax-entry ?\} "){" lisp-mode-syntax-table))

(use-package paredit
  :commands (paredit-wrap-round
             paredit-wrap-curly
             paredit-wrap-square
             paredit-meta-doublequote))

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
      (or (ignore-errors (project-root (project-current)))
          default-directory)))

;; TODO: rename to dwim
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

(global-set-key (kbd "<f1>") 'toggle-repl-window)
(global-set-key (kbd "<f2>") 'dired-jump)
(global-set-key (kbd "<f3>") 'counsel-fzf-in-project)
(global-set-key (kbd "<f4>") 'counsel-ag-in-project)
(global-set-key (kbd "<f5>") 'previous-buffer)
(global-set-key (kbd "<f6>") 'next-buffer)
(global-set-key (kbd "<f7>") 'recentf-open-files)
(global-set-key (kbd "<f8>") 'select-or-exit-minibuffer)
(global-set-key (kbd "<f9>") 'kill-current-buffer)
(global-set-key (kbd "<f10>") 'delete-window)
(global-set-key (kbd "<f11>") 'kill-buffer-and-window)
(global-set-key (kbd "<f12>") 'universal-argument)

(global-unset-key (kbd "C-z"))

(setq echo-keystrokes 0.05)
(global-so-long-mode)
(setq read-process-output-max (* 1024 1024))
