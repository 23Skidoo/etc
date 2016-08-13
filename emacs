;; -*- emacs-lisp -*-

;;;; Prologue.

;; Time my .emacs.
(defvar *emacs-load-start* (current-time))
;; Common Lisp compatibility.
(require 'cl)

;; OS detection.
(defconst mswindows-p (eq system-type 'mswindows))
(defconst linux-p     (eq system-type 'gnu/linux))

;; Separate custom file.
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; Don't load stale bytecode.
(setq load-prefer-newer t)

;; Set up package management.
(require 'package)
(add-to-list 'package-archives
         '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
         '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(if (require 'quelpa nil t)
  (quelpa-self-upgrade)
  (with-temp-buffer
    (url-insert-file-contents
     "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))
(quelpa '(quelpa-use-package :fetcher github :repo "quelpa/quelpa-use-package"))
(require 'use-package)
(require 'quelpa-use-package)

;;;; My settings.

;; Do not show splash screen.
(setq inhibit-startup-message t)

;; Fix clipboard on Linux.
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
(set-fringe-mode '(8 . 0))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode 0))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode 0))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode 0))

;; Spaces instead of tabs.
(setq-default indent-tabs-mode nil)
;; Tabs == 4 spaces.
(setq-default tab-width 4)

(setq save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      load-prefer-newer t
      ediff-window-setup-function 'ediff-setup-windows-plain
      save-place-file (concat user-emacs-directory "places"))

; Make (shebanged) scripts executable automatically.
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; M-w with no active region copies a line.
(defadvice kill-ring-save
  (before slick-copy activate compile)
  "When called interactively with no active region, copy a single
  line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position) (line-beginning-position 2)))))

;; C-w with no active region kills a line.
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
        (line-beginning-position 2)))))

;; Enable upcase-region.
(put 'upcase-region 'disabled nil)

;; Ask before killing unsaved buffer.
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

;; Quickly kill current buffer with C-c C-w.
(defun my-kill-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key [(ctrl c) (ctrl w)] 'my-kill-buffer)

;; Kill all buffers.
(defun kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

;; Syntax highlighting.
(global-font-lock-mode t)
(setq fontlock-maximum-decoration t)
;; Visual appearance.
(setq icon-title-format "%b %* - GNU Emacs")
(setq frame-title-format "%f %* - GNU Emacs")
;; Font.
(if mswindows-p
    (add-to-list 'default-frame-alist
'(font . "-outline-Consolas-normal-r-normal-normal-13-97-96-96-c-*-iso8859-5")))
 (when linux-p (add-to-list 'default-frame-alist '(font-backend . "xft"))
       (add-to-list 'default-frame-alist '(font . "Terminus-10")))

;; Flicker instead of beep.
(setq visible-bell 1)
;; Highlight selected region.
(setq-default transient-mark-mode t)

;; Indicate empty lines.
(set-default 'indicate-empty-lines t)

;; Set encoding for .nfo files.
(modify-coding-system-alist 'file "\\.nfo\\'" 'cp437)

;; Shell mode tweaks.
(setq ansi-color-names-vector           ; better contrast colors
      ["black" "red4" "green4" "yellow4"
       "blue3" "magenta4" "cyan4" "white"])
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'shell-mode-hook '(lambda () (toggle-truncate-lines 1)))
(setq comint-prompt-read-only t)
(if linux-p
    (setenv "ESHELL" (expand-file-name "~/bin/eshell")))

;; Disable vc.
(setq vc-handled-backends nil)

;; Make text-mode the default.
(setq-default major-mode 'text-mode)

;;;; Built-in packages.

(use-package abbrev
  :config
    (if (file-exists-p abbrev-file-name)
        (quietly-read-abbrev-file)))

(use-package align
  :bind ("C-x a r" . align-regexp))

(use-package compile
  :bind ([f8] . recompile)
  :init
    (setq mode-compile-always-save-buffer-p t)
    ;; Make the compile window stick at 12 lines tall
    (setq compilation-window-height 12)
    ;; Scroll output of *compilation*
    (setq compilation-scroll-output 'first-error))

(use-package delsel
  ;; Typed text replaces selection when selection is active.
  :config (delete-selection-mode t))

(use-package dired
  :init
    ;; Allow deletion of non-empty directories.
    (setq dired-recursive-deletes 'top)
    ;; Enable 'a' key.
    (put 'dired-find-alternate-file 'disabled nil))

(use-package hippie-exp
  :bind ("M-/" . hippie-expand))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package isearch
  :bind
    ("C-s" . isearch-forward-regexp)
    ("C-r" . isearch-backward-regexp)
    ("C-M-s" . isearch-forward)
    ("C-M-r" . isearch-backward))

(use-package misc
  :bind ("M-z" . zap-up-to-char))

;; Encoding/language.
(use-package mule
  :preface
    (defun set-input-method-russian-computer () (interactive)
           (set-input-method "russian-computer"))
    (defun set-input-method-swedish-postfix () (interactive)
           (set-input-method "swedish-postfix"))
  :bind*
    (([f6] . set-input-method-russian-computer)
     ([f7] . set-input-method-swedish-postfix))

  :config
    (set-language-environment "Russian")
    (setq default-buffer-file-coding-system 'utf-8-unix)
    (prefer-coding-system 'utf-8))

(use-package paren
  :config
    ;; Highlight matching parentheses.
    (show-paren-mode t))

(use-package recentf
  :config
    (setq recentf-save-file (expand-file-name "~/.recentf"))
    (recentf-mode t))

;; Save minibuffer history.
(use-package savehist
  :config
    (setq savehist-additional-variables
          '(kill-ring mark-ring global-mark-ring search-ring
                      regexp-search-ring extended-command-history))
    (savehist-mode t))

;; Save point position in visited files.
(use-package saveplace
  :config (setq-default save-place t))

;; Show line and column numbers
(use-package simple
  :config
    (line-number-mode 1)
    (column-number-mode 1))

;; More sane window management
(use-package windmove
  :preface
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

  (defun swap-with-down  () (interactive) (swap-with 'down))
  (defun swap-with-up    () (interactive) (swap-with 'up))
  (defun swap-with-left  () (interactive) (swap-with 'left))
  (defun swap-with-right () (interactive) (swap-with 'right))

  (defun enlarge-window-plus      () (interactive) (enlarge-window  1))
  (defun enlarge-window-minus     () (interactive) (enlarge-window -1))
  (defun enlarge-window-hor-plus  () (interactive) (enlarge-window  1 t))
  (defun enlarge-window-hor-minus () (interactive) (enlarge-window -1 t))

  :bind*
    (("C-M-J" . swap-with-down)
     ("C-M-K" . swap-with-up)
     ("C-M-H" . swap-with-left)
     ("C-M-L" . swap-with-right)
     ("M-J"   . enlarge-window-plus)
     ("M-K"   . enlarge-window-minus)
     ("M-H"   . enlarge-window-hor-plus)
     ("M-L"   . enlarge-window-hor-minus)
     ("M-j"   . windmove-down)
     ("M-k"   . windmove-up)
     ("M-h"   . windmove-left)
     ("M-l"   . windmove-right)))

;; Unique bufffer names.
(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

;;;; External packages.

(use-package ag :quelpa :defer t
  :config (setq ag-highlight-search t
                ag-reuse-buffers t))

(use-package auctex :ensure t :pin gnu :defer t
  :init (add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)
        (add-hook 'LaTeX-mode-hook #'flyspell-mode)
        (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
        (setq TeX-auto-save t
              TeX-parse-self t
              TeX-save-query nil
              TeX-PDF-mode t))

(use-package beacon :quelpa
  :config (beacon-mode 1))

(setq color-themes '())
(use-package color-theme-solarized :quelpa
  :config (setq solarized-italic nil
                x-underline-at-descent-line t)
          (load-theme 'solarized)
          (add-hook 'after-make-frame-functions
                    (lambda (frame)
                      (let ((mode (if (display-graphic-p frame) 'light 'light)))
                        (set-frame-parameter frame 'background-mode mode)
                        (set-terminal-parameter frame 'background-mode mode))
                      (enable-theme 'solarized))))

(use-package smart-mode-line :quelpa
  :config
    (sml/setup)
    (use-package rich-minority
      :config
      (setq rm-blacklist
            (mapconcat #'identity
                       '(" (\\*)" " Abbrev" " company"
                         " ew\\:[mnltMNLT]+" "PgLn" "Projectile")
                       "\\|"))))

(use-package company :quelpa
  :config
    (global-company-mode)
    (use-package company-flx :quelpa :config (company-flx-mode t))
    (use-package company-quickhelp :quelpa :config (company-quickhelp-mode t))
    (use-package company-cabal :quelpa
      :config (add-to-list 'company-backends 'company-cabal))
    (use-package company-auctex :quelpa
      :config (company-auctex-init)))

(use-package dockerfile-mode :quelpa :defer t)

;; Be obsessive-compulsive about trailing whitespace.
(use-package ethan-wspace :quelpa
  :init
    (setq mode-require-final-newline nil)
  :config
    (global-ethan-wspace-mode 1)
    (setq ethan-wspace-face '(t (:background "#05ff00")))
    (setq ethan-wspace-face-customized t))

(use-package intero :quelpa :defer t)

(use-package haskell-mode :quelpa :defer t
  :init
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
  :config
    (setq haskell-program-name "ghci -Wall -fno-warn-type-defaults")
    (use-package haskell-cabal :defer t
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

(use-package htmlize :ensure t :pin melpa :defer t)

(use-package ido
  :init
    (setq ido-enable-prefix nil
          ido-enable-flex-matching t
          ido-auto-merge-work-directories-length nil
          ido-everywhere t
          ido-create-new-buffer 'always
          ido-use-filename-at-point 'guess)
  :config
    (ido-mode t)
    (setq ido-use-faces t)
    (use-package ido-ubiquitous :quelpa :config (ido-ubiquitous-mode t))
    (use-package flx-ido :quelpa :config (flx-ido-mode t))
    (use-package ido-vertical-mode :quelpa
      :config
        (ido-vertical-mode t)
        (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
        (setq ido-vertical-show-count t))
    (use-package ido-yes-or-no :quelpa :config (ido-yes-or-no-mode t)))

(use-package markdown-mode :quelpa :defer t)

(use-package page-break-lines :quelpa
  :config (global-page-break-lines-mode))

(use-package projectile :quelpa :defer t
  :init (projectile-global-mode t))

(use-package rect-mark :ensure t :pin marmalade :defer t)

(use-package rust-mode :quelpa :defer t
  :init (use-package cargo :quelpa :defer t)
        (use-package racer :quelpa :defer t)
        (add-hook 'rust-mode-hook #'cargo-minor-mode)
        (add-hook 'rust-mode-hook #'racer-mode)
        (add-hook 'racer-mode-hook #'company-mode)
  :bind (:map rust-mode-map
         ([tab] . company-indent-or-complete-common)))

(use-package smex :quelpa
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
   (("C-S-<iso-lefttab>" . tabbar-backward)
    ("C-<tab>" . tabbar-forward)))

(use-package toml-mode :quelpa :defer t)

(use-package yaml-mode :quelpa :defer t)

(use-package yasnippet :quelpa
  :init (yas-global-mode 1))

;;;; Helper functions.

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

;;;; Coda.

;; Start the server.
(server-start)

;; Time my .emacs
(message "My .emacs loaded in %ds"
         (destructuring-bind
             (hi lo us ps) (current-time)
           (- (+ hi lo)
              (+ (first *emacs-load-start*)
                 (second *emacs-load-start*)))))
