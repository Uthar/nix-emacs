(setf use-package-expand-minimally t
      use-package-use-theme nil)

(eval-when-compile
  (require 'use-package))

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

(use-package direnv
  :custom (direnv-always-show-summary nil)
  :hook (prog-mode . direnv-mode))

(use-package evil-matchit
  :after evil
  :config (global-evil-matchit-mode))

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
  (magit-define-global-key-bindings nil))

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

(use-package windmove
  :hook
  (after-init . windmove-default-keybindings)
  (after-init . windmove-swap-states-default-keybindings))

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
  :custom (diff-font-lock-syntax nil)
  :hook
  (diff-mode
   . (lambda ()
       (setq-local require-final-newline nil)
       (setq-local before-save-hook nil))))

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
  (set-language-environment "UTF-8")
  :hook
  (after-init . fido-vertical-mode)
  (after-init . (lambda () (load-theme 'wombat)))
  (after-save . executable-make-buffer-file-executable-if-script-p))

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
  (dolist (state '(motion replace operator visual insert normal))
    (evil-global-set-key state (kbd "C-r") nil)
    (evil-global-set-key state (kbd "C-e") nil)
    (evil-global-set-key state (kbd "C-a") nil)
    (evil-global-set-key state (kbd "C-i") nil)
    (evil-global-set-key state (kbd "C-k") nil)
    (evil-global-set-key state (kbd "M-.") nil)))

(use-package which-key
  :diminish
  :custom (which-key-dont-use-unicode t)
  :config (which-key-mode))

(use-package editorconfig
  :diminish
  :config (editorconfig-mode t))

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
  (define-key cider-repl-mode-map (kbd "C-c M-o") 'cider-repl-clear-buffer))

(use-package slime
  :commands (slime slime-connect)
  :custom
  (slime-truncate-lines nil)
  (slime-net-coding-system 'utf-8-unix)
  (inferior-lisp-program "sbcl --disable-ldb --dynamic-space-size 4096")
  (slime-contribs '(slime-asdf slime-quicklisp slime-fancy))
  (slime-repl-auto-right-margin t)
  (slime-repl-history-size 10000)
  (common-lisp-hyperspec-root "@clhs@/HyperSpec/")
  (common-lisp-hyperspec-symbol-table "@clhs@/HyperSpec/Data/Map_Sym.txt")
  :bind ("C-c s" . 'slime-selector)
  :config
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

(use-package lisp-mode
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

;; search
(setenv "FZF_DEFAULT_COMMAND" "fd -LH")

(defun universal-argument-provided? ()
  (>= (prefix-numeric-value current-prefix-arg) 4))

(defun guess-directory (cmd-name)
  (if (universal-argument-provided?)
      (counsel-read-directory-name (concat cmd-name " in directory: "))
      (or (if (featurep 'projectile) (projectile-project-root))
          default-directory)))

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
