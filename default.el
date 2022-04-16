;; -*- lexical-binding: t -*-

;; Minimize garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)

;; (add-hook 'emacs-startup-hook
;;   (lambda ()
;;     (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value)))))

;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
  (lambda ()
    (message "Emacs ready in %s with %d garbage collections."
             (emacs-init-time)
             gcs-done)
    (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value)))))

;; Disable impure packages.
;; The load path is instead built with Nix from emacs.nix.
(setq package-archives nil)

(rplaca mouse-wheel-scroll-amount 2)
(setf mouse-wheel-progressive-speed nil)
(put 'if 'lisp-indent-function 4)
(put 'if-let 'lisp-indent-function 4)

(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

(add-hook 'tetris-mode-hook
  (lambda ()
    (define-key tetris-mode-map "z" 'tetris-rotate-next)
    (define-key tetris-mode-map "x" 'tetris-rotate-prev)))

(add-hook 'after-init-hook
  (lambda ()
    (diminish 'auto-revert-mode)
    (diminish 'eldoc-mode)))

(add-hook 'evil-mode-hook
  (lambda ()
    (global-evil-matchit-mode)))

;; (add-hook 'after-init-hook
;;   (lambda ()
;;     (global-page-break-lines-mode)))

(setq direnv-always-show-summary nil)
(add-hook 'after-init-hook
  (lambda ()
    (direnv-mode)))

(add-hook 'prog-mode-hook
  (lambda ()
    (font-lock-add-keywords nil
	  '(("TODO" 0 '(:foreground "red" :weight bold) t)
        ("NOTE" 0 '(:foreground "dark green" :weight bold) t)))))

;; projectile
(setq projectile-enable-caching t
      projectile-completion-system 'ivy
      projectile-track-known-projects-automatically nil)

(add-hook 'after-init-hook
  (lambda ()
    (projectile-mode)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)))

;; magit
(setq magit-completing-read-function 'ivy-completing-read
      magit-define-global-key-bindings nil)

(add-hook 'magit-mode-hook
  (lambda ()
    (dotimes (i 4)
      (let* ((n (1+ i))
             (command (intern (format "magit-section-show-level-%i-all" n))))
        (define-key magit-section-mode-map (kbd (format "M-%i" n)) nil)
        (define-key magit-section-mode-map (kbd (format "C-%i" n)) command)))))



;; vc-annotate
(with-eval-after-load "vc-annotate"
  (defun toggle-hook (hook function)
    (if (and (consp (symbol-value hook))
             (memq function (symbol-value hook)))
        (remove-hook hook function)
        (add-hook hook function)))
  (defun vc-annotate-toggle-annotation-visibility* ()
    (toggle-hook 'vc-annotate-mode-hook 'vc-annotate-toggle-annotation-visibility))
  (define-key vc-annotate-mode-map (kbd "v")
      (lambda ()
        (interactive)
        (vc-annotate-toggle-annotation-visibility)
        (vc-annotate-toggle-annotation-visibility*))))

;; winum
(add-hook 'after-init-hook
  (lambda ()
    (winum-mode)
    (dotimes (i 9)
      (let* ((n (1+ i))
             (key (kbd (format "M-%i" n)))
             (command (intern (format "winum-select-window-%i" n))))
        (with-eval-after-load "diff"
          (define-key diff-mode-map key nil))
        (with-eval-after-load "term"
          (define-key term-raw-map key command))
        (global-set-key key command)))))

;; dired
(setq
  dired-kill-when-opening-new-dired-buffer t
  dired-listing-switches "--group-directories-first -lh")

(with-eval-after-load "dired"
  (defun dired-toggle-hidden ()
    (interactive)
    (if (string-match-p "a" dired-actual-switches)
        (dired "." (remove ?a dired-listing-switches))
        (dired "." (concat dired-listing-switches "a")))
    (setf dired-listing-switches dired-actual-switches))
  (define-key dired-mode-map (kbd "M-h") 'dired-toggle-hidden)
  (define-key dired-mode-map "N" nil)
  (define-key dired-mode-map "n" nil)
  (define-key dired-mode-map [mouse-1] 'dired-find-file))

(add-hook 'dired-mode-hook
  (lambda ()
  (dired-hide-details-mode)
  (setq-local mouse-1-click-follows-link nil)
  (evil-local-set-key 'normal "l" 'dired-find-file)
  (evil-local-set-key 'normal "h" 'dired-up-directory)))

;; diff
(setq diff-font-lock-syntax nil)

(add-hook 'diff-mode-hook
  (lambda ()
    (setq-local require-final-newline nil)
    (setq-local before-save-hook nil)))

;; ivy
(add-hook 'after-init-hook
  (lambda ()
    (ivy-mode t)
    (counsel-mode)))

;; emacs
(defun state-dir (dir)
  (expand-file-name (concat user-emacs-directory dir "/")))

(defmacro with-inhibit-message (&rest body)
  `(let ((inhibit-message t))
     ,@body))

(setq
 completions-detailed t
 read-minibuffer-restore-windows nil
 mode-line-compact 'long
 enable-dir-local-variables nil
 enable-local-variables nil
 server-client-instructions nil
 use-short-answers t
 create-lockfiles nil
 enable-recursive-minibuffers t
 custom-file null-device
 use-dialog-box nil
 require-final-newline t
 display-line-numbers-type 'relative
 scroll-margin 4
 scroll-conservatively 1000
 initial-scratch-message ""
 initial-buffer-choice t
 version-control t
 delete-old-versions t
 backup-directory-alist `((".*" . ,(state-dir "backups")))
 auto-save-file-name-transforms `((".*" ,(state-dir "auto-save") t))
 auto-save-list-file-prefix (state-dir "auto-save")
 indent-tabs-mode nil
 tab-width 4
 column-number-mode t
 line-number-mode t)

(add-hook 'after-init-hook
  (lambda ()
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
    (set-language-environment "UTF-8")))

(add-hook 'before-save-hook
  (lambda ()
    (delete-trailing-whitespace)))

(add-hook 'before-save-hook
  (lambda ()
    (executable-make-buffer-file-executable-if-script-p)))

(add-hook 'prog-mode-hook
  (lambda ()
    (display-line-numbers-mode)))

;; recentf
(with-eval-after-load "recentf"
  (defun recentf-save-file-p (file)
    (string= file (expand-file-name recentf-save-file)))
  (defun recentf-save-current-buffer ()
    (let ((file-name (buffer-file-name (current-buffer))))
      (when file-name
        (recentf-add-file file-name)
        (with-inhibit-message (recentf-save-list)))))
  (add-to-list 'recentf-exclude 'recentf-save-file-p)
  (add-to-list 'recentf-exclude (regexp-opt '("ci-comment-")))
  (add-hook 'buffer-list-update-hook 'recentf-save-current-buffer))

;; evil
(setq
 evil-want-C-u-scroll t
 evil-kill-on-visual-paste nil
 evil-undo-system 'undo-redo
 evil-want-keybinding nil)

(with-eval-after-load "evil"
  (evil-set-initial-state 'help-mode 'emacs)
  (evil-global-set-key 'insert (kbd "C-r") nil)
  (dolist (state '(motion insert))
    (evil-global-set-key state (kbd "C-e") nil))
  (evil-global-set-key 'insert (kbd "C-a") nil)
  (evil-global-set-key 'motion (kbd "C-i") nil)
  (evil-global-set-key 'insert (kbd "C-k") nil)
  (evil-global-set-key 'normal (kbd "M-.") nil)
  (add-to-list 'evil-buffer-regexps (cons (regexp-opt '("*Warnings*")) 'emacs)))

;; xref
(add-hook 'xref-after-update-hook 'evil-emacs-state)

;; This packages' code is unreasonably slow. I should rewrite it to be faster.
;; evil-collection
;; (setq evil-collection-company-use-tng nil)

;; (with-eval-after-load "evil"
;;   (require 'evil-collection)
;;   (evil-collection-magit-setup)
;;   (evil-collection-dired-setup)
;;   (evil-collection-help-setup)
;;   (evil-collection-company-setup)
;;   (evil-collection-term-setup))

;; anzu
(cl-labels ((setup-anzu (&rest args)
              (require 'anzu)
              (require 'evil-anzu)
              (global-anzu-mode t)
              (advice-remove 'evil-search #'setup-anzu)))
  (advice-add 'evil-search :before #'setup-anzu))

;; company
(setq
 company-dabbrev-downcase nil
 company-dabbrev-ignore-case t
 company-minimum-prefix-length 1
 company-show-numbers 'left)

(add-hook 'after-init-hook
  (lambda ()
    (global-company-mode)))

;; which-key
(setq which-key-dont-use-unicode t)

(let ((done nil))
  (add-hook 'pre-command-hook
    (lambda ()
      (unless done
        (which-key-mode)
        (setq done t)))))

;; editorconfig
(let ((done nil))
  (add-hook 'find-file-hook
    (lambda ()
      (unless done
        (editorconfig-mode t)
        (setq done t)))))

;; ;; remove ?
;; (use-package flycheck
;;   :custom (flycheck-disabled-checkers '(emacs-lisp-checkdoc))
;;   :diminish
;;   :config (global-flycheck-mode))

;; flycheck
(add-hook 'after-init-hook 'global-flycheck-mode)

;; yaml
(with-eval-after-load "yaml-mode"
  (add-hook 'yaml-mode-hook 'display-line-numbers-mode))

;; nix
(with-eval-after-load "nix-mode"
  (defun nix-prefetch-tarball-at-point ()
    (interactive)
    (let ((hash (shell-command-to-string
                 (concat "nix-prefetch-url --unpack "
                         (ffap-string-at-point)
                         " 2> /dev/null"))))
      (kill-new (string-trim hash))
      (message "Copied %s to kill ring" (string-trim hash))))

  (define-key nix-mode-map (kbd "C-x n h") 'nix-prefetch-tarball-at-point))


;; cider
(setq cider-show-error-buffer 'except-in-repl)

(with-eval-after-load "cider"
  (define-key cider-repl-mode-map (kbd "C-c M-o") 'cider-repl-clear-buffer)
  (evil-set-initial-state 'cider-stacktrace-mode 'emacs)
  (cl-flet
      ((fix-slime-conflict ()
         ;; should fix slime-company itself
         (setq-local company-backends (remove 'company-slime company-backends))))
    (add-hook 'clojure-mode-hook #'fix-slime-conflict)
    (add-hook 'clojurescript-mode-hook #'fix-slime-conflict)))

;; (use-package lsp-mode
;;   :commands lsp
;;   :custom
;;   (lsp-keymap-prefix "C-c l")
;;   (lsp-restart 'ignore)
;;   :hook
;;   (((c-mode c++-mode python-mode go-mode) . lsp)
;;    (lsp-mode . lsp-enable-which-key-integration))
;;   :config
;;   (defun direnv-update-advice (&rest r)
;;     (direnv-update-directory-environment))
;;   (advice-add 'lsp :before 'direnv-update-advice))

;; (use-package lsp-python-ms
;;   :after lsp-mode
;;   :config
;;   (defun set-python-ms-executable (&rest r)
;;     (setf lsp-python-ms-executable (executable-find "python-language-server")))
;;   (advice-add 'lsp :before 'set-python-ms-executable '((depth . 100))))

;; (use-package python
;;   :commands python-mode
;;   :config
;;   (advice-add 'python-shell-make-comint :around 'call-with-repl-window)
;;   (advice-add 'python-shell-switch-to-shell :around 'call-with-repl-window))

;; (use-package modus-themes
;;   :config (modus-themes-load-themes)
;;   :hook (after-init . modus-themes-load-operandi)
;;   :bind ("C-c t" . modus-themes-toggle))

;; slime
(setq
 slime-truncate-lines nil
 slime-net-coding-system 'utf-8-unix
 inferior-lisp-program "sbcl --disable-ldb --dynamic-space-size 4096"
 slime-contribs '(slime-asdf slime-company slime-quicklisp slime-fancy)
 slime-company-completion 'fuzzy
 slime-repl-auto-right-margin t
 slime-repl-history-size 10000
 common-lisp-hyperspec-root "@clhs@/HyperSpec/"
 common-lisp-hyperspec-symbol-table "@clhs@/HyperSpec/Data/Map_Sym.txt")

(with-eval-after-load "slime"
  (define-key slime-mode-map (kbd "C-c s") 'slime-selector)
  (add-to-list 'evil-buffer-regexps
               (cons (regexp-opt '("*slime-description*")) 'emacs))
  ;; (advice-add 'slime :around 'call-with-repl-window)
  ;; (advice-add 'slime-repl :around 'call-with-repl-window)
  (define-key slime-mode-map (kbd "C-c C-z") 'slime-repl)
  (define-key slime-mode-map (kbd "C-c h") 'slime-hyperspec-lookup)
  ;; Disable annoying tab completion buffers.
  ;; Careful: both slime-repl and inferior-slime set this.
  ;; With M-x slime this is enough because only slime-repl is loaded
  ;; Probably wouldn't work if using comint (but who would want to?)
  (add-hook 'slime-repl-mode-hook
    (lambda ()
      (setq-local tab-always-indent t))))

(with-eval-after-load "slime-repl"
  (defslime-repl-shortcut nil ("delete-package" "dp")
    (:handler (lambda ()
                (interactive)
                (let ((package (slime-read-package-name "Package: ")))
                  (slime-repl-shortcut-eval `(cl:delete-package ,package)))))
    (:one-liner "Delete a package.")))

(with-eval-after-load "slime-presentations"
  (define-key slime-presentation-map [mouse-1]
    'slime-inspect-presentation-at-mouse))

(with-eval-after-load "lisp-mode"
  (modify-syntax-entry ?\[ "(]" lisp-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" lisp-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}" lisp-mode-syntax-table)
  (modify-syntax-entry ?\} "){" lisp-mode-syntax-table))

;; (use-package paredit
;;   :bind (("M-(" . paredit-wrap-round)
;;          ("M-{" . paredit-wrap-curly)
;;          ("M-[" . paredit-wrap-square)
;;          ("M-\"" . paredit-meta-doublequote)
;;          ("M-s" . paredit-splice-sexp)
;;          ("M-r" . paredit-raise-sexp)
;;          ("M-q" . paredit-reindent-defun)
;;          ("C-<right>" . paredit-forward-slurp-sexp)
;;          ("C-<left>" . paredit-forward-barf-sexp)))

;; ;; repl window

;; (defvar repl-window nil)
;; (defvar repl-window-height 15)
;; (defvar last-repl-buffer nil)

;; (defun repl-window-selected? ()
;;   (and repl-window (eq repl-window (selected-window))))

;; (defun save-last-repl-buffer (frame)
;;   (when (repl-window-selected?)
;;     (setf last-repl-buffer (window-buffer repl-window))))

;; (add-to-list 'window-buffer-change-functions 'save-last-repl-buffer)

;; (defun save-repl-window-height (frame)
;;   (when repl-window
;;     (setf repl-window-height (window-height repl-window))))

;; (add-to-list 'window-size-change-functions 'save-repl-window-height)

;; (defun default-repl-buffer ()
;;   (ansi-term "bash"))

;; (defun open-repl-window ()
;;   (setf repl-window
;;         (split-window
;;          (frame-root-window)
;;          (- (min repl-window-height
;;                  (- (window-height (frame-root-window)) window-min-height)))))
;;   (select-window repl-window)
;;   (switch-to-buffer (or last-repl-buffer (default-repl-buffer))))

;; (defun close-repl-window ()
;;   (ignore-errors (delete-window repl-window))
;;   (setf repl-window nil))

;; (defun toggle-repl-window ()
;;   (interactive)
;;   (if repl-window
;;       (close-repl-window)
;;       (open-repl-window)))

;; (defun ensure-repl-window ()
;;   (if (window-live-p repl-window)
;;       (select-window repl-window)
;;       (open-repl-window)))

;; (defun open-buffer-in-repl-window (buffer)
;;   (ensure-repl-window)
;;   (switch-to-buffer buffer))

;; (defun call-with-repl-window (fn &rest args)
;;   (open-buffer-in-repl-window (save-window-excursion (apply fn args))))

;; (advice-add 'ansi-term :around 'call-with-repl-window)
;; (advice-add 'eshell :around 'call-with-repl-window)
;; (advice-add 'cider-repl-create :around 'call-with-repl-window)
;; (advice-add 'cider-switch-to-repl-buffer :around 'call-with-repl-window)

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


;; generic utilities
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
(global-set-key (kbd "<f1>")  'toggle-repl-window)
(global-set-key (kbd "<f2>")  'dired-jump)
(global-set-key (kbd "<f3>")  'counsel-fzf-in-project)
(global-set-key (kbd "<f4>")  'counsel-ag-in-project)
(global-set-key (kbd "<f5>")  'previous-buffer)
(global-set-key (kbd "<f6>")  'next-buffer)
(global-set-key (kbd "<f7>")  'recentf-open-files)
(global-set-key (kbd "<f8>")  'select-or-exit-minibuffer)
(global-set-key (kbd "<f9>")  'kill-current-buffer)
(global-set-key (kbd "<f10>") 'delete-window)
(global-set-key (kbd "<f11>") 'kill-buffer-and-window)
(global-set-key (kbd "<f12>") 'universal-argument)

;; (require 'notifications)
