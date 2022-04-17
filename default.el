;; -*- lexical-binding: t -*-

(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
  (lambda ()
    (message "Emacs ready in %s with %d garbage collections."
             (emacs-init-time)
             gcs-done)
    (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value)))
    (load-theme 'wombat)))

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

(setq direnv-always-show-summary nil)

(add-hook 'after-init-hook
  (lambda ()
    (direnv-mode)))

(add-hook 'prog-mode-hook
  (lambda ()
    (font-lock-add-keywords nil
	  '(("TODO" 0 '(:foreground "red" :weight bold) t)
        ("NOTE" 0 '(:foreground "dark green" :weight bold) t)))))

(setq magit-define-global-key-bindings nil)

(with-eval-after-load "magit"
  (dotimes (i 4)
    (let* ((n (1+ i))
           (command (intern (format "magit-section-show-level-%i-all" n))))
      (define-key magit-section-mode-map (kbd (format "M-%i" n)) nil)
      (define-key magit-section-mode-map (kbd (format "C-%i" n)) command))))

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
  (define-key dired-mode-map [mouse-1] 'dired-find-file))

(add-hook 'dired-mode-hook
  (lambda ()
  (dired-hide-details-mode)
  (setq-local mouse-1-click-follows-link nil)))

(setq diff-font-lock-syntax nil)

(add-hook 'diff-mode-hook
  (lambda ()
    (setq-local require-final-newline nil)
    (setq-local before-save-hook nil)))

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
 display-line-numbers-type t
 scroll-margin 4
 scroll-conservatively 1000
 initial-scratch-message ""
 initial-buffer-choice t
 version-control t
 delete-old-versions t
 backup-directory-alist `((".*" . ,(state-dir "backups")))
 auto-save-file-name-transforms `((".*" ,(state-dir "auto-save") t))
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
    (set-language-environment "UTF-8")
    (add-to-list 'permanently-enabled-local-variables 'mode)))

(add-hook 'before-save-hook
  (lambda ()
    (executable-make-buffer-file-executable-if-script-p)))

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

(setq cider-show-error-buffer 'except-in-repl)

(with-eval-after-load "cider"
  (define-key cider-repl-mode-map (kbd "C-c M-o") 'cider-repl-clear-buffer)
  (cl-flet
      ((fix-slime-conflict ()
         (setq-local company-backends (remove 'company-slime company-backends))))
    (add-hook 'clojure-mode-hook #'fix-slime-conflict)
    (add-hook 'clojurescript-mode-hook #'fix-slime-conflict)))

(setq
 slime-truncate-lines nil
 slime-net-coding-system 'utf-8-unix
 inferior-lisp-program "sbcl --disable-ldb --dynamic-space-size 4096"
 slime-contribs '(slime-asdf slime-quicklisp slime-fancy)
 slime-repl-auto-right-margin t
 slime-repl-history-size 10000
 common-lisp-hyperspec-root "@clhs@/HyperSpec/"
 common-lisp-hyperspec-symbol-table "@clhs@/HyperSpec/Data/Map_Sym.txt")

(with-eval-after-load "slime"
  (define-key slime-mode-map (kbd "C-c s") 'slime-selector)
  (define-key slime-mode-map (kbd "C-c C-z") 'slime-repl)
  (define-key slime-mode-map (kbd "C-c h") 'slime-hyperspec-lookup)
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

(defun universal-argument-provided-p ()
  (>= (prefix-numeric-value current-prefix-arg) 4))

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
  (if (universal-argument-provided-p)
      (exit-minibuffer))
      (select-window (minibuffer-window)))

(defun xdg-open ()
  (interactive)
  (start-process "xdg-open" nil "xdg-open" (ffap-string-at-point)))

(global-set-key (kbd "M-o") 'other-window)

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
