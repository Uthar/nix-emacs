;; -*- lexical-binding: t -*-

;; Startup
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
  (lambda ()
    (message "booted in %s" (emacs-init-time))
    (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value)))))

;; Mouse
(rplaca mouse-wheel-scroll-amount 2)
(setq mouse-wheel-progressive-speed nil)

;; Tetris
(with-eval-after-load 'tetris
  (define-key tetris-mode-map "z" 'tetris-rotate-next)
  (define-key tetris-mode-map "x" 'tetris-rotate-prev))

;; Direnv
(setq direnv-always-show-summary nil)
(add-hook 'after-init-hook 'direnv-mode)

;; Efsl
(autoload 'efsl "efsl")

;; Magit
(setq magit-completing-read-function 'ivy-completing-read)
(setq magit-define-global-key-bindings nil)

;; VC
(defun toggle-hook (hook function)
  (if (and (consp (symbol-value hook))
           (memq function (symbol-value hook)))
      (remove-hook hook function)
      (add-hook hook function)))

(defun vc-annotate-toggle-annotation-visibility* ()
  (toggle-hook 'vc-annotate-mode-hook 'vc-annotate-toggle-annotation-visibility))

(with-eval-after-load 'vc-annotate
  (define-key vc-annotate-mode-map (kbd "v")
    (lambda ()
      (interactive)
      (vc-annotate-toggle-annotation-visibility)
      (vc-annotate-toggle-annotation-visibility*))))

;; Windmove
(add-hook 'after-init-hook
  (lambda ()
    (windmove-default-keybindings)
    (windmove-swap-states-default-keybindings)))

;; Dired
(defun dired-toggle-hidden ()
  (interactive)
  (if (string-match-p "a" dired-actual-switches)
      (dired "." (remove ?a dired-listing-switches))
      (dired "." (concat dired-listing-switches "a")))
  (setf dired-listing-switches dired-actual-switches))

(setq dired-kill-when-opening-new-dired-buffer t)
(setq dired-listing-switches "--group-directories-first -lh")

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "M-h") 'dired-toggle-hidden)
  (define-key dired-mode-map "N" nil)
  (define-key dired-mode-map "n" nil)
  (define-key dired-mode-map [mouse-1] 'dired-find-file))

(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(add-hook 'dired-mode-hook
  (lambda ()
    (setq-local mouse-1-click-follows-link nil)
    (local-set-key "j" 'dired-next-line)
    (local-set-key "k" 'dired-previous-line)
    (local-set-key "l" 'dired-find-file)
    (local-set-key "h" 'dired-up-directory)))

;; Diff
(setq diff-font-lock-syntax nil)

(add-hook 'diff-mode-hook
  (lambda ()
    (setq-local require-final-newline nil)
    (setq-local before-save-hook nil)))

;; Ivy
(add-hook 'after-init-hook 'ivy-mode)
(add-hook 'after-init-hook 'counsel-mode)

;; Emacs
(defun state-dir (dir)
  (expand-file-name (concat user-emacs-directory dir "/")))

(setq completions-detailed t)
(setq read-minibuffer-restore-windows nil)
(setq mode-line-compact 'long)
(setq enable-dir-local-variables nil)
(setq enable-local-variables nil)
(setq server-client-instructions nil)
(setq use-short-answers t)
(setq create-lockfiles nil)
(setq enable-recursive-minibuffers t)
(setq custom-file null-device)
(setq use-dialog-box nil)
(setq require-final-newline t)
(setq display-line-numbers-type 'relative)
(setq scroll-margin 4)
(setq scroll-conservatively 1000)
(setq initial-scratch-message "")
(setq initial-buffer-choice t)
(setq version-control t)
(setq delete-old-versions t)
(cl-flet ((dir (dir)
            (expand-file-name (concat user-emacs-directory dir "/"))))
  (setq backup-directory-alist `((".*" . (dir "backups"))))
  (setq auto-save-file-name-transforms `((".*" ,(dir "auto-save") t)))
  (setq auto-save-list-file-prefix (dir "auto-save")))
(setq indent-tabs-mode nil)
(setq tab-width 4)
(setq column-number-mode t)
(setq line-number-mode t)
(setq echo-keystrokes 0.05)
(setq read-process-output-max (* 1024 1024))

(with-eval-after-load 'emacs
  (put 'narrow-to-region 'disabled nil)
  (put 'erase-buffer 'disabled nil)
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
  (global-so-long-mode)
  (set-language-environment "UTF-8"))

(add-hook 'after-init-hook (lambda () (load-theme 'wombat)))
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;;;; Recentf
(with-eval-after-load 'recentf

  (defun recentf-save-file-p (file)
    (string= file (expand-file-name recentf-save-file)))

  (add-to-list 'recentf-exclude 'recentf-save-file-p)
  (add-to-list 'recentf-exclude (regexp-opt '("ci-comment-")))

  (defun recentf-save-current-buffer ()
    (let ((file-name (buffer-file-name (current-buffer)))
          (inhibit-message t))
      (when file-name
        (recentf-add-file file-name)
        (recentf-save-list))))

  (add-hook 'buffer-list-update-hook 'recentf-save-current-buffer))

;; Evil
(setq evil-want-C-u-scroll t)
(setq evil-kill-on-visual-paste nil)
(setq evil-undo-system 'undo-redo)
(setq evil-want-keybinding nil)
(setq evil-move-beyond-eol t)

(with-eval-after-load 'evil
  (evil-global-set-key 'insert (kbd "C-[") 'evil-force-normal-state)
  (evil-global-set-key 'motion (kbd "C-e") nil)
  (evil-global-set-key 'insert (kbd "C-e") nil)
  (evil-global-set-key 'insert (kbd "C-a") nil)
  (evil-global-set-key 'insert (kbd "C-k") nil)
  (dolist (state '(normal motion insert))
    (evil-global-set-key state (kbd "C-f") nil)
    (evil-global-set-key state (kbd "C-b") nil)
    (evil-global-set-key state [remap cua-paste-pop] nil)
    (evil-global-set-key state [remap yank-pop] nil)
    (evil-global-set-key state (kbd "C-n") nil)
    (evil-global-set-key state (kbd "C-p") nil))
  (evil-global-set-key 'normal (kbd "M-.") nil)
  (global-anzu-mode)
  (global-evil-matchit-mode))

(autoload 'evil-local-mode "evil")

(add-hook 'prog-mode-hook 'evil-local-mode)
(add-hook 'yaml-mode-hook 'evil-local-mode)
(add-hook 'org-mode-hook
  (lambda ()
    (evil-local-mode)
    (evil-local-set-key 'motion (kbd "<tab>") 'org-cycle)))

;; Company
(setq company-dabbrev-downcase nil)
(setq company-dabbrev-ignore-case t)
(setq company-minimum-prefix-length 1)
(setq company-show-numbers 'left)
(add-hook 'after-init-hook 'global-company-mode)

;; Which key
(setq which-key-dont-use-unicode t)
(add-hook 'after-init-hook 'which-key-mode)

;; Editorconfig
(add-hook 'after-init-hook 'editorconfig-mode)

;; Flycheck
(add-hook 'prog-mode-hook 'flycheck-mode)

;; Prog
(add-hook 'prog-mode-hook
  (lambda ()
    (font-lock-add-keywords nil
	  '(("TODO" 0 '(:foreground "red" :weight bold) t)
        ("NOTE" 0 '(:foreground "dark green" :weight bold) t)))))

;; Elisp
(put 'if 'lisp-indent-function 4)
(put 'if-let 'lisp-indent-function 4)

;; Yaml
(add-hook 'yaml-mode-hook 'display-line-numbers-mode)

;; Nix
(with-eval-after-load 'nix-mode
  (defun nix-prefetch-tarball-at-point ()
    (interactive)
    (let ((hash (shell-command-to-string
                 (concat "nix-prefetch-url --unpack "
                         (ffap-string-at-point)
                         " 2> /dev/null"))))
      (kill-new (string-trim hash))
      (message "Copied %s to kill ring" (string-trim hash))))
  (define-key nix-mode-map (kbd "C-x n h") 'nix-prefetch-tarball-at-point))

(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))

;; Cider
(setq cider-show-error-buffer 'except-in-repl)

(with-eval-after-load 'cider
  (define-key cider-repl-mode-map (kbd "C-c M-o") 'cider-repl-clear-buffer))

(add-hook 'clojure-mode-hook
  (lambda ()
    (setq-local company-backends (remove 'company-slime company-backends))))

(add-hook 'clojurescript-mode-hook
  (lambda ()
    (setq-local company-backends (remove 'company-slime company-backends))))

;; LSP
(setq lsp-keymap-prefix "C-c l")
(setq lsp-restart 'ignore)

(with-eval-after-load 'lsp-mode
  (defun direnv-update-advice (&rest r)
    (direnv-update-directory-environment))
  (advice-add 'lsp :before 'direnv-update-advice))

(add-hook 'c-mode-hook 'lsp-deferred)
(add-hook 'c++-mode-hook 'lsp-deferred)
(add-hook 'python-mode-hook 'lsp-deferred)
(add-hook 'go-mode-hook 'lsp-deferred)
(add-hook 'lsp-mode-hook 'lsp-enable-which-key-integration)

;; Python
(with-eval-after-load 'python
  (advice-add 'python-shell-make-comint :around 'call-with-repl-window)
  (advice-add 'python-shell-switch-to-shell :around 'call-with-repl-window))

;; Slime
(setq slime-truncate-lines nil)
(setq slime-net-coding-system 'utf-8-unix)
(setq inferior-lisp-program "sbcl --disable-ldb --dynamic-space-size 4096")
(setq slime-contribs '(slime-asdf slime-company slime-quicklisp slime-fancy))
(setq slime-company-completion 'fuzzy)
(setq slime-repl-auto-right-margin t)
(setq slime-repl-history-size 10000)
(setq common-lisp-hyperspec-root "@clhs@/HyperSpec/")
(setq common-lisp-hyperspec-symbol-table "@clhs@/HyperSpec/Data/Map_Sym.txt")

(with-eval-after-load 'slime
  (advice-add 'slime :around 'call-with-repl-window)
  (advice-add 'slime-repl :around 'call-with-repl-window)
  (define-key slime-mode-map (kbd "C-c C-z") 'slime-repl)
  (define-key slime-mode-map (kbd "C-c h") 'slime-hyperspec-lookup)
  (define-key slime-mode-map (kbd "C-c s") 'slime-selector))

(add-hook 'slime-repl-mode-hook
  (lambda ()
    (setq-local tab-always-indent t)))

(with-eval-after-load 'slime-repl
  (defslime-repl-shortcut nil
    ("delete-package" "dp")
    (:handler
     (lambda ()
       (interactive)
       (let
           ((package
             (slime-read-package-name "Package: ")))
         (slime-repl-shortcut-eval
          `(cl:delete-package ,package)))))
    (:one-liner "Delete a package.")))

(with-eval-after-load 'slime-presentations
  (define-key slime-presentation-map [mouse-1] 'slime-inspect-presentation-at-mouse))

(with-eval-after-load 'slime-cl-indent
  (setq common-lisp-style-default
    (define-common-lisp-style "kpg" 
      "Indentation fixes"
      (:inherit "modern")
      (:indentation
       (->
        (as if))
       (->>
        (as if))
       (hash-keys-bind
        (as destructuring-bind))
       (dovec
        (as dolist))))))

;; Lisp
(with-eval-after-load 'lisp-mode
  (modify-syntax-entry ?\[ "(]" lisp-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" lisp-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}" lisp-mode-syntax-table)
  (modify-syntax-entry ?\} "){" lisp-mode-syntax-table))

;; Repl window
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

;; Search
(setenv "FZF_DEFAULT_COMMAND" "fd -LH")

(defun universal-argument-provided? ()
  (>= (prefix-numeric-value current-prefix-arg) 4))

(defun guess-directory (cmd-name)
  (if (universal-argument-provided?)
      (counsel-read-directory-name (concat cmd-name " in directory: "))
      (or (ignore-errors (project-root (project-current)))
          default-directory)))

(defun kaspi/fzf-dwim ()
  (interactive)
  (let ((dir (guess-directory "fzf")))
    (counsel-fzf "" dir (concat "fzf in " dir ": "))))

(defun kaspi/rg-dwim ()
  (interactive)
  (let ((dir (guess-directory "rg")))
    (counsel-rg "" dir " --hidden --follow " (concat "rg in " dir ": "))))

;; Utils
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

;; Keys
(global-set-key (kbd "<f1>") 'toggle-repl-window)
(global-set-key (kbd "<f2>") 'dired-jump)
(global-set-key (kbd "<f3>") 'kaspi/fzf-dwim)
(global-set-key (kbd "<f4>") 'kaspi/rg-dwim)
(global-set-key (kbd "<f5>") 'previous-buffer)
(global-set-key (kbd "<f6>") 'next-buffer)
(global-set-key (kbd "<f7>") 'recentf-open-files)
(global-set-key (kbd "<f8>") 'select-or-exit-minibuffer)
(global-set-key (kbd "<f9>") 'kill-current-buffer)
(global-set-key (kbd "<f10>") 'delete-window)
(global-set-key (kbd "<f11>") 'kill-buffer-and-window)
(global-set-key (kbd "<f12>") 'universal-argument)

(global-unset-key (kbd "C-z"))

