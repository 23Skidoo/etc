;; -*- emacs-lisp -*-

;; Time my .emacs.
(defvar *emacs-load-start* (current-time))

;; Common Lisp compatibility
(require 'cl)

;; el-get
(setq my-packages '(auctex
                    auto-complete
                    hs-lint
                    markdown-mode
                    yaml-mode
                    cider
                    color-theme-solarized
                    ethan-wspace
                    haskell-mode
                    htmlize
                    rect-mark
                    rst-mode
                    tabbar
                    yasnippet))

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

(el-get 'sync my-packages)

;; color-theme
(setq color-theme-is-global nil)
(setq solarized-italic nil)

;; Enable color-theme-solarized only for emacsclient -w.
(defvar after-make-console-frame-hooks '()
  "Hooks to run after creating a new TTY frame")
(defvar after-make-window-system-frame-hooks '()
  "Hooks to run after creating a new window-system frame")

(defun run-after-make-frame-hooks (frame)
  "Selectively run either `after-make-console-frame-hooks' or
`after-make-window-system-frame-hooks'"
  (select-frame frame)
  (run-hooks (if window-system
                 'after-make-window-system-frame-hooks
               'after-make-console-frame-hooks)))

(add-hook 'after-make-frame-functions 'run-after-make-frame-hooks)
(add-hook 'after-init-hook
          (lambda ()
            (run-after-make-frame-hooks (selected-frame))))

(add-hook 'after-make-window-system-frame-hooks
          (lambda () (load-theme 'solarized)))

;; OS detection
(defconst mswindows-p (string-match "windows" (symbol-name system-type)))
(defconst linux-p (string-match "linux" (symbol-name system-type)))

;; Do not show splash screen
(setq inhibit-startup-message t)

;; Fix clipboard on Linux
(if linux-p
    (progn
      (setq x-select-enable-primary nil)
      (setq x-select-enable-clipboard t)))

;; Don't clutter current directory with autosave/backup.
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; 'y' instead of 'yes'
(fset 'yes-or-no-p 'y-or-n-p)
;; Misc tweaks
(setq scroll-step 1)
(setq scroll-conservatively 5)
(find-file "~/")
(tool-bar-mode 0)
(menu-bar-mode 0)
(delete-selection-mode t)
; Make (shebanged) scripts executable automatically
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; Show line and column numbers
(line-number-mode 1)
(column-number-mode 1)
;; Maximize frame on startup (on Win32; on Linux this is done by devilspie)
(if mswindows-p
    (setq emacsw32-max-frames t))

;; Scrollbar
(scroll-bar-mode 0)
(set-fringe-mode nil)

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

;; Alias for M-x
(global-set-key "\C-c\C-x" 'execute-extended-command)
;; Compilation key
(global-set-key [f8] 'recompile)
(setq mode-compile-always-save-buffer-p t)
;; Make the compile window stick at 12 lines tall
(setq compilation-window-height 12)
;; Alias for align-regexp
(global-set-key (kbd "C-x a r") 'align-regexp)
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

;;highlight matching parentheses
(show-paren-mode t)
;; Flicker instead of beep
(setq visible-bell 1)
;; Highlight selected region
(setq-default transient-mark-mode t)

;; Be obsessive-compulsive about trailing whitespace
;; (add-hook 'before-save-hook 'whitespace-cleanup)
(setq mode-require-final-newline nil)
(global-ethan-wspace-mode 1)
(setq ethan-wspace-face '(t (:background "#05ff00")))
(setq ethan-wspace-face-customized t)

;; Scroll output of *compilation*
(setq compilation-scroll-output 'first-error)
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

;; Use ack for searching
(setq ack-command "ack-grep --nocolor --nogroup ")

;; Ido completion
(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess)

;; Find file at point
(require 'ffap)
(defvar ffap-c-commment-regexp "^/\\*+"
  "Matches an opening C-style comment, like \"/***\".")

(defadvice ffap-guesser (after avoid-c-comments activate)
  "Don't return paths like \"/******\" unless they actually exist.

This fixes the bug where ido would try to suggest a C-style
comment as a filename."
  (ignore-errors
    (when (and ad-return-value
               (string-match-p ffap-c-commment-regexp
                               ad-return-value)
               (not (ffap-file-exists-string ad-return-value)))
      (setq ad-return-value nil))))

; Enable fuzzy matching
(setq ido-enable-flex-matching t)

;; Tabbar mode
;(autoload 'tabbar-mode "tabbar" t)
(tabbar-mode)

;; Tweak faces in tabbar mode
(set-face-attribute
 'tabbar-default nil
 :background "gray60")
(set-face-attribute
 'tabbar-unselected nil
 :background "gray85"
 :foreground "gray30"
 :box nil)
(set-face-attribute
 'tabbar-selected nil
 :background "#f2f2f6"
 :foreground "black"
 :box nil)
(set-face-attribute
 'tabbar-button nil
 :box '(:line-width 1 :color "gray72" :style released-button))
(set-face-attribute
 'tabbar-separator nil
 :height 0.7)

;; Ctrl-Tab/Ctrl-Shift-Tab for going forward/backwards between tabs
(if linux-p
    (define-key global-map [(control shift iso-lefttab)] 'tabbar-backward))
(if mswindows-p
    (define-key global-map [(control shift tab)] 'tabbar-backward))
(define-key global-map [(control tab)] 'tabbar-forward)

;; Make text-mode the default
(setq default-major-mode 'text-mode)

;; Yasnippet
(yas-global-mode 1)

;; Haskell-mode tweaks
(setq haskell-program-name "ghci -Wall -fno-warn-type-defaults")
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(eval-after-load "haskell-mode"
    '(progn
       (define-key haskell-mode-map (kbd "C-,") 'haskell-move-nested-left)
       (define-key haskell-mode-map (kbd "C-.") 'haskell-move-nested-right)
       (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile)
       (define-key haskell-mode-map (kbd "C-x C-d") nil)
       (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
       (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
       (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
       (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
       (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
       (define-key haskell-mode-map (kbd "C-c M-.") nil)
       (define-key haskell-mode-map (kbd "C-c C-d") nil)))
(eval-after-load "haskell-cabal"
    '(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))
(eval-after-load "which-func"
  '(add-to-list 'which-func-modes 'haskell-mode))

;; Agda2 mode
;(if linux-p
;   (load-file (let ((coding-system-for-read 'utf-8))
;                (shell-command-to-string "agda-mode locate"))))

;; AuCTeX mode (Linux-only for now)
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)

;; Set C-TAB in scala-mode to the default value.
(defun my-scala-mode-hook ()
  (local-unset-key [(control tab)]))
(add-hook 'scala-mode-hook 'my-scala-mode-hook)

;; Separate custom file.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

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
