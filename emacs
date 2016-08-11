;; -*- emacs-lisp -*-

;; Time my .emacs.
(defvar *emacs-load-start* (current-time))

;; Common Lisp compatibility
(require 'cl)

;; OS detection
(defconst mswindows-p (string-match "windows" (symbol-name system-type)))
(defconst linux-p (string-match "linux" (symbol-name system-type)))

;; Separate custom file.
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; Dependencies
(require 'package)
(add-to-list 'package-archives
         '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
         '("marmalade" . "https://marmalade-repo.org/packages/") t)
(package-initialize)
(if (require 'quelpa nil t)
  (quelpa-self-upgrade)
  (with-temp-buffer
    (url-insert-file-contents
     "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))
(quelpa '(quelpa-use-package :fetcher github :repo "quelpa/quelpa-use-package"))
(require 'use-package)
(require 'quelpa-use-package)
(setq use-package-always-ensure t)

(use-package abbrev
  :ensure nil
  :diminish abbrev-mode
  :config
    (if (file-exists-p abbrev-file-name)
        (quietly-read-abbrev-file)))

(use-package ag :quelpa)

;; Alias for align-regexp
(use-package align
  :bind ("C-x a r" . align-regexp))

(use-package auctex :quelpa
  :mode ("\\.tex\\'" . latex-mode)
  :commands (latex-mode LaTeX-mode plain-tex-mode)
  :init ;; (add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)
        ;; (add-hook 'LaTeX-mode-hook #'flyspell-mode)
        ;; (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
        (setq TeX-auto-save t
              TeX-parse-self t
              TeX-save-query nil
              TeX-PDF-mode t))

(use-package color-theme)
(use-package color-theme-solarized :quelpa :demand
             :init (load-theme 'solarized))
;;   (setq solarized-italic nil)
;; (setq color-theme-is-global nil)
;; ;; Enable color-theme-solarized only for emacsclient -w.
;; (defvar after-make-console-frame-hooks '()
;;   "Hooks to run after creating a new TTY frame")
;; (defvar after-make-window-system-frame-hooks '()
;;   "Hooks to run after creating a new window-system frame")

;; (defun run-after-make-frame-hooks (frame)
;;   "Selectively run either `after-make-console-frame-hooks' or
;; `after-make-window-system-frame-hooks'"
;;   (select-frame frame)
;;   (run-hooks (if window-system
;;                  'after-make-window-system-frame-hooks
;;                'after-make-console-frame-hooks)))

;; (add-hook 'after-make-frame-functions 'run-after-make-frame-hooks)
;; (add-hook 'after-init-hook
;;           (lambda ()
;;             (run-after-make-frame-hooks (selected-frame))))

;; (add-hook 'after-make-window-system-frame-hooks
;;           (lambda () (load-theme 'solarized)))

(use-package diminish :quelpa :demand)

(use-package company :quelpa :demand
  :config
    (company-mode t)
    (use-package company-flx :quelpa :config (company-flx-mode t))
    (use-package company-quickhelp :quelpa :config (company-quickhelp-mode t))
    (use-package company-cabal :quelpa
       :config (add-to-list 'company-backends 'company-cabal)))

;; Be obsessive-compulsive about trailing whitespace.
(use-package ethan-wspace :quelpa :demand
  :init
    (setq mode-require-final-newline nil)
  :config
    (global-ethan-wspace-mode 1)
    (setq ethan-wspace-face '(t (:background "#05ff00")))
    (setq ethan-wspace-face-customized t))

(use-package haskell-mode :quelpa
  :init
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
  :config
    (setq haskell-program-name "ghci -Wall -fno-warn-type-defaults")
    (use-package haskell-cabal
                 :commands haskell-cabal-mode
                 :mode "\\.cabal\\'"
                 :ensure nil
                 :bind-keymap ("C-c C-c" . haskell-compile))
  :bind-keymap
    (("C-," . haskell-move-nested-left)
     ("C-." . haskell-move-nested-right)
     ("C-c C-c" . haskell-compile)
     ("C-x C-d" . nil)
     ("C-c C-z" . haskell-interactive-switch)
     ("C-c C-b" . haskell-interactive-switch)
     ("C-c C-t" . haskell-process-do-type)
     ("C-c C-i" . haskell-process-do-info)
     ("C-c M-." . nil)
     ("C-c C-d" . nil)))

(use-package which-func
  :ensure nil
  :config (add-to-list 'which-func-modes 'haskell-mode))

(use-package htmlize :quelpa)

(use-package ido :demand
  :init
    (setq ido-enable-prefix nil
          ido-enable-flex-matching t
          ido-auto-merge-work-directories-length nil
          ido-everywhere t
          ido-create-new-buffer 'always
          ido-use-filename-at-point 'guess)
  :config
    (ido-mode t)
    (use-package ido-ubiquitous :quelpa :config (ido-ubiquitous-mode t))
    (use-package flx-ido :quelpa :config (flx-ido-mode t))
    (use-package ido-vertical-mode :quelpa :config (ido-vertical-mode t))
    (use-package ido-yes-or-no :quelpa :config (ido-yes-or-no-mode t)))

(use-package ielm
  :ensure nil
  :config
    (add-hook 'inferior-emacs-lisp-mode-hook
              (lambda ()
                (turn-on-eldoc-mode))))

(use-package intero :quelpa)

(use-package markdown-mode :quelpa)

;; Highlight matching parentheses.
(use-package paren
  :config
    (show-paren-mode t))

(use-package pos-tip :quelpa)

(use-package projectile :quelpa)

(use-package recentf
  :config
    (setq recentf-save-file (expand-file-name "~/.recentf"))
    (recentf-mode t))

(use-package rect-mark)

(use-package rst-mode :quelpa)

(use-package smex :quelpa :demand
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c M-x" . execute-extended-command)))

(use-package tabbar :quelpa :demand
 :config
   (tabbar-mode)
   ;; Tweak faces
   (set-face-attribute 'tabbar-default nil :background "gray60")
   (set-face-attribute 'tabbar-unselected nil :background "gray85"
                       :foreground "gray30" :box nil)
   (set-face-attribute 'tabbar-selected nil :background "#f2f2f6"
                       :foreground "black" :box nil)
   (set-face-attribute 'tabbar-button nil
                       :box '(:line-width 1 :color "gray72"
                                          :style released-button))
   (set-face-attribute 'tabbar-separator nil :height 0.7)

 :bind
   ;; Ctrl-Tab/Ctrl-Shift-Tab for going forward/backwards between tabs
   (([control shift tab] . tabbar-backward)
    ([control tab] . tabbar-forward)))

(use-package yaml-mode :quelpa)

(use-package yasnippet :quelpa
  :init (yas-global-mode 1))

;; Do not show splash screen
(setq inhibit-startup-message t)

;; Fix clipboard on Linux
(if linux-p
    (progn
      (setq x-select-enable-primary nil)
      (setq x-select-enable-clipboard t)))

;; Don't clutter current directory with backups.
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

;; Misc tweaks.
(setq scroll-step 1)
(setq scroll-conservatively 5)
(menu-bar-mode 0)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode 0))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode 0))
(use-package fringe
  :ensure nil
  :config (set-fringe-mode nil))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode 0))
;; Unique bufffer names
(use-package uniquify
  :ensure nil
  :config (setq uniquify-buffer-name-style 'forward))
;; Save point position in visited files.
(use-package saveplace :demand
  :config (setq-default save-place t))
;; Save minibuffer history.
(use-package savehist
  :config
    (setq savehist-additional-variables
          '(kill-ring mark-ring global-mark-ring search-ring
                      regexp-search-ring extended-command-history))
    (savehist-mode t))

(use-package misc
  :ensure nil
  :bind ("M-z" . zap-up-to-char))

(use-package hippie-exp
  :bind ("M-/" . hippie-expand))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package isearch
  :ensure nil
  :bind
    ("C-s" . isearch-forward-regexp)
    ("C-r" . isearch-backward-regexp)
    ("C-M-s" . isearch-forward)
    ("C-M-r" . isearch-backward))

(setq-default indent-tabs-mode nil)
(setq save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      load-prefer-newer t
      ediff-window-setup-function 'ediff-setup-windows-plain
      save-place-file (concat user-emacs-directory "places"))

(use-package delsel
  :config (delete-selection-mode t))

; Make (shebanged) scripts executable automatically
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; Show line and column numbers
(use-package simple
  :ensure nil
  :config
    (line-number-mode 1)
    (column-number-mode 1))

;; Copy lines without selecting them
(defadvice kill-ring-save
  (before slick-copy activate compile)
  "When called interactively with no active region, copy a single
  line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position) (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
        (line-beginning-position 2)))))

;; language settings
(global-set-key [f6] '(lambda () (interactive)
                        (set-input-method "russian-computer")))
(global-set-key [f7] '(lambda () (interactive)
                        (set-input-method "swedish-postfix")))
;; More sane window management
(defun swap-with (dir)
  (interactive)
  (let ((other-window (windmove-find-other-window dir)))
    (when other-window
      (let* ((this-window (selected-window))
             (this-buffer (window-buffer this-window))
             (other-buffer (window-buffer other-window))
             (this-start (window-start this-window))
             (other-start (window-start other-window)))
        (set-window-buffer this-window other-buffer)
        (set-window-buffer other-window this-buffer)
        (set-window-start this-window other-start)
        (set-window-start other-window this-start)))))

(global-set-key (kbd "C-M-J") (lambda () (interactive) (swap-with 'down)))
(global-set-key (kbd "C-M-K") (lambda () (interactive) (swap-with 'up)))
(global-set-key (kbd "C-M-H") (lambda () (interactive) (swap-with 'left)))
(global-set-key (kbd "C-M-L") (lambda () (interactive) (swap-with 'right)))

(global-set-key (kbd "M-J") (lambda () (interactive) (enlarge-window 1)))
(global-set-key (kbd "M-K") (lambda () (interactive) (enlarge-window -1)))
(global-set-key (kbd "M-H") (lambda () (interactive) (enlarge-window -1 t)))
(global-set-key (kbd "M-L") (lambda () (interactive) (enlarge-window 1 t)))

(global-set-key (kbd "M-j") 'windmove-down)
(global-set-key (kbd "M-k") 'windmove-up)
(global-set-key (kbd "M-h") 'windmove-left)
(global-set-key (kbd "M-l") 'windmove-right)

;; Enable upcase-region
(put 'upcase-region 'disabled nil)

;; Encoding
(set-language-environment "Russian")
(setq default-buffer-file-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8)

;; Ask before killing unsaved buffer
(defun ask-before-killing-buffer ()
  (let ((buffer (current-buffer)))
    (cond
     ((equal (buffer-name) "*scratch*")
      ;; Never kill *scratch*
      nil)
     ((and buffer-file-name (buffer-modified-p))
      ;; If there's a file associated with the buffer,
      ;; make sure it's saved
      (yes-or-no-p (format "Buffer %s modified; kill anyway? "
                           (buffer-name))))
     ((get-buffer-process buffer)
      ;; If there's a process associated with the buffer,
      ;; make sure it's dead
      (yes-or-no-p (format "Process %s active; kill anyway? "
                           (process-name (get-buffer-process buffer)))))
     (t t))))
(add-to-list 'kill-buffer-query-functions
             'ask-before-killing-buffer)

;; Quickly kill current buffer with C-c C-w
(defun my-kill-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key [(ctrl c) (ctrl w)] 'my-kill-buffer)

;; Kill all buffers.
(defun kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

;; turn on syntax highlighting
(global-font-lock-mode t)
(setq fontlock-maximum-decoration t)
;; tabs == 4 spaces
(setq-default tab-width 4)
;; visual appearance
(setq icon-title-format "%b %* - GNU Emacs")
(setq frame-title-format "%f %* - GNU Emacs")
;; Adjust font
(if mswindows-p
    (add-to-list 'default-frame-alist
'(font . "-outline-Consolas-normal-r-normal-normal-13-97-96-96-c-*-iso8859-5")))
(when linux-p (add-to-list 'default-frame-alist '(font-backend . "xft"))
      (add-to-list 'default-frame-alist '(font . "Terminus-10")))

;; Flicker instead of beep
(setq visible-bell 1)
;; Highlight selected region
(setq-default transient-mark-mode t)

(use-package compile
  :bind ([f8] . recompile)
  :init
    (setq mode-compile-always-save-buffer-p t)
    ;; Make the compile window stick at 12 lines tall
    (setq compilation-window-height 12)
    ;; Scroll output of *compilation*
    (setq compilation-scroll-output 'first-error))

(use-package dockerfile-mode :quelpa
  :mode ("Dockerfile\\'" . dockerfile-mode))

;; Indicate empty lines.
(set-default 'indicate-empty-lines t)

;; Set encoding for .nfo files
(modify-coding-system-alist 'file "\\.nfo\\'" 'cp437)

;; Dired tweaks
;; Allow deletion of non-empty directories
(setq dired-recursive-deletes 'top)
;; Enable 'a' key
(put 'dired-find-alternate-file 'disabled nil)

;; Shell mode tweaks
(setq ansi-color-names-vector           ; better contrast colors
      ["black" "red4" "green4" "yellow4"
       "blue3" "magenta4" "cyan4" "white"])
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'shell-mode-hook '(lambda () (toggle-truncate-lines 1)))
(setq comint-prompt-read-only t)
(if linux-p
    (setenv "ESHELL" (expand-file-name "~/bin/eshell")))

;; Disable vc
(setq vc-handled-backends nil)

;; Make text-mode the default
(setq default-major-mode 'text-mode)

;; Count words
(defun count-words (&optional begin end)
  "Count words between BEGIN and END (region); if no region
  defined, count words in buffer"
  (interactive "r")
  (let ((b (if mark-active begin (point-min)))
      (e (if mark-active end (point-max))))
    (message "Word count: %s" (how-many "\\w+" b e))))

;; Smart split
(defun smart-split ()
  "Split the frame into 80-column sub-windows, and make sure no
   window has fewer than 80 columns."
  (interactive)
  (defun smart-split-helper (w)
    "Helper function to split a given window into two, the first
     of which has 80 columns."

    (if (> (window-width w) (* 2 81))
    (let ((w2 (split-window w 84 t)))
      (smart-split-helper w2))))
  (smart-split-helper nil))

(server-start)

;; Time my .emacs
(message "My .emacs loaded in %ds"
         (destructuring-bind
             (hi lo us ps) (current-time)
           (- (+ hi lo)
              (+ (first *emacs-load-start*)
                 (second *emacs-load-start*)))))
