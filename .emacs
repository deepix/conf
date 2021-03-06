;; deepaknag's dot emacs

;; (setq debug-on-error t)

;; silence is golden ...
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)

;; change this to point to your elisp modules (if any)
(add-to-list 'load-path "~/conf/elisp/")

;; else "loop" below won't run
(require 'cl)

(if (< emacs-major-version 24)
    (require 'package)
  (load "package"))

(package-initialize)

;; (add-to-list 'package-archives
;; 	     '("marmalade" .
;; 	       "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

;; p4 mode
(require 'p4)

;; code below plagiarized from:
;; http://www.aaronbedra.com/emacs.d/
(defvar deepaknag/packages '(solarized-theme
			     color-theme
			     better-defaults
                             column-marker
			     auto-complete
			     flycheck
                             highlight
			     xcscope
                             nyan-mode
                             ack
			     linum-relative)
  "Default packages")

(defun deepaknag/packages-installed-p ()
  (loop for pkg in deepaknag/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (deepaknag/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg deepaknag/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(require 'better-defaults)

(require 'column-marker)
(add-hook 'prog-mode-hook (lambda () (interactive) (column-marker-1 80)))

;; else M-x rgrep doesn't work in fish
(setq shell-file-name "/bin/sh")

(require 'saveplace)
(setq-default save-place t)

;;; Setup auto-complete ;;;
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(setq ac-delay 0.1)

;; (add-hook 'after-init-hook 'global-flycheck-mode)
;; (global-set-key (kbd "C-c <up>") 'flycheck-next-error)
;; (global-set-key (kbd "C-c <down>") 'flycheck-previous-error)
;; (global-set-key (kbd "C-c l") 'flycheck-list-errors)
;; (setq-default flymake-no-changes-timeout '3)

(setq ediff-diff-options "-w")

(when (fboundp 'winner-mode)
      (winner-mode 1))

(add-hook 'c-mode-common-hook   'hs-minor-mode)

(defun color-theme-emacs-default ()
  ;; change theme to day/night depending on time of day
  (load-theme 'solarized-light t)
  (setq calendar-location-name "San Jose, CA")
  (setq calendar-latitude 37.33)
  (setq calendar-longitude -121.89))

(defun color-theme-emacs23 ()
  (require 'color-theme)
  (eval-after-load "color-theme"
    '(progn
       (color-theme-initialize)
       (color-theme-high-contrast)))
  )

;; easy on eyes first
(if (< emacs-major-version 24)
    (color-theme-emacs23)
  (color-theme-emacs-default))

;; some customizations
(column-number-mode t)
(transient-mark-mode t)
(setq make-backup-files nil)
;; this makes backup files go away
(setq backup-directory-alist `((".*" . , temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" , temporary-file-directory t)))
;; show date and time in mode-line
(setq display-time-day-and-date t)
(display-time)

;; don't let emacs die when i mistype c-x c-c
(setq confirm-kill-emacs 'yes-or-no-p)
(defalias 'yes-or-no-p 'y-or-n-p)

;; dev environment
(cwarn-mode t)
(which-func-mode t)

(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(size-indication-mode t)
(nyan-mode)

;; syntax highlighting
(global-font-lock-mode t)
(global-cwarn-mode t)
(setq font-lock-maximum-decoration t)
(setq font-lock-maximum-size 262144)
;; compilation
;; (setq compilation-scroll-output t)

;; cscope
(setq cscope-option-do-not-update-database t)
(cscope-setup)

;; for p4 diff and merge
;; from: http://stackoverflow.com/questions/3822473/p4config-with-emacs
;; -diff
(defun command-line-diff (switch)
  (let ((file1 (pop command-line-args-left))
        (file2 (pop command-line-args-left)))
    (ediff file1 file2)))
(add-to-list 'command-switch-alist '("-diff" . command-line-diff))

;; -merge
(defun command-line-merge (switch)
  (let ((base (pop command-line-args-left))
        (sccs (pop command-line-args-left))
        (mine (pop command-line-args-left))
        (merg (pop command-line-args-left)))
   (ediff-merge-with-ancestor sccs mine base () merg)))
(add-to-list 'command-switch-alist '("-merge" . command-line-merge))

;; comment out "#if 0" blocks in c mode
;; very handy and saves a ton of cursing
;; this fn from http://stackoverflow.com/questions/4549015/in-c-c-mode-in-emacs-change-face-of-code-in-if-0-endif-block-to-comment-fa
(defun my-c-mode-font-lock-if0 (limit)
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (let ((depth 0) str start start-depth)
        (while (re-search-forward
		"^\\s-*#\\s-*\\(if\\|else\\|endif\\)" limit 'move)
          (setq str (match-string 1))
          (if (string= str "if")
              (progn
                (setq depth (1+ depth))
                (when (and (null start) (looking-at "\\s-+0"))
                  (setq start (match-end 0)
                        start-depth depth)))
            (when (and start (= depth start-depth))
              (c-put-font-lock-face start (match-beginning 0)
				    'font-lock-comment-face)
              (setq start nil))
            (when (string= str "endif")
              (setq depth (1- depth)))))
        (when (and start (> depth 0))
          (c-put-font-lock-face start (point) 'font-lock-comment-face)))))
  nil)

;; this piece comes from linux CodingStyle doc
;; https://www.kernel.org/doc/Documentation/CodingStyle
(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
	 (column (c-langelem-2nd-pos c-syntactic-element))
	 (offset (- (1+ column) anchor))
	 (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(add-hook 'prog-mode-hook 'linum-mode)
(setq linum-format "%5d ")

(add-hook 'after-save-hook
  'executable-make-buffer-file-executable-if-script-p)

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; p4 diff/merge

;; -diff
(defun command-line-diff (switch)
  (let ((file1 (pop command-line-args-left))
        (file2 (pop command-line-args-left)))
    (ediff file1 file2)))
(add-to-list 'command-switch-alist '("-diff" . command-line-diff))

;; -merge
(defun command-line-merge (switch)
  (let ((base (pop command-line-args-left))
        (sccs (pop command-line-args-left))
        (mine (pop command-line-args-left))
        (merg (pop command-line-args-left)))
   (ediff-merge-with-ancestor sccs mine base () merg)))
(add-to-list 'command-switch-alist '("-merge" . command-line-merge))

;;(require 'go-mode-autoloads)
;;(require 'go-mode-load)
(add-hook 'go-mode-hook
          (lambda ()
            (cscope-minor-mode)
            (add-hook 'before-save-hook 'gofmt-before-save)))

(require 'jinja2-mode)
(add-to-list 'auto-mode-alist '("\\.j2$" . jinja2-mode))

(add-hook 'java-mode-hook
      (lambda ()
        (cscope-minor-mode)))

;; configure python tabs
(add-hook 'python-mode-hook
      (lambda ()
        (setq indent-tabs-mode nil)
        (setq tab-width 4)
        (cscope-minor-mode)))
;; Configure flymake for Python
(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "epylint" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))
;; Configure to wait a bit longer after edits before starting
(setq-default flymake-no-changes-timeout '3)
;; Keymaps to navigate to the errors
(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-cn" 'flymake-goto-next-error)))
(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-cp" 'flymake-goto-prev-error)))
;; Set as a minor mode for Python
;;(add-hook 'python-mode-hook '(lambda () (flymake-mode t)))
;; To avoid having to mouse hover for the error message, these functions make flymake error messages
;; appear in the minibuffer
(defun show-fly-err-at-point ()
  "If the cursor is sitting on a flymake error, display the message in the minibuffer"
  (require 'cl)
  (interactive)
  (let ((line-no (line-number-at-pos)))
    (dolist (elem flymake-err-info)
      (if (eq (car elem) line-no)
      (let ((err (car (second elem))))
        (message "%s" (flymake-ler-text err)))))))
(add-hook 'post-command-hook 'show-fly-err-at-point)

(add-hook 'c-mode-common-hook
          (lambda ()
            (c-add-style
             "linux-tabs-only"
             '("linux" (c-offsets-alist
                        (arglist-cont-nonempty
                         c-lineup-gcc-asm-reg
                         c-lineup-arglist-tabs-only))))
	    (font-lock-add-keywords
	     nil
	     '((my-c-mode-font-lock-if0 (0 font-lock-comment-face prepend))) 'add-to-end)
	    (setq indent-tabs-mode t)
	    (c-set-style "linux-tabs-only")
))


(setq clean-buffer-list-delay-general 30)

;; remove trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)
