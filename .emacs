;; deepaknag's dot emacs

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

;; code below plagiarized from:
;; http://www.aaronbedra.com/emacs.d/
(defvar deepaknag/packages '(solarized-theme
			     theme-changer
			     auto-complete
			     color-theme
			     xcscope
			     org
			     magit
			     fill-column-indicator
			     go-mode
			     markdown-mode
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

(defun color-theme-emacs-default ()
  ;; change theme to day/night depending on time of day
  (load-theme 'solarized-light t)
  (setq calendar-location-name "San Jose, CA")
  (setq calendar-latitude 37.33)
  (setq calendar-longitude -121.89)
  (require 'theme-changer)
  (change-theme 'solarized-light 'solarized-dark))

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
;; gives problems on emacs23
;;(scroll-bar-mode -1)
;;(tool-bar-mode nil)
(menu-bar-mode -99)
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

;; dev environment
(cwarn-mode t)
(which-func-mode t)
(show-paren-mode t)
(require 'auto-complete-config)
(ac-config-default)

(require 'fill-column-indicator)

;; syntax highlighting
(global-font-lock-mode t)
(global-cwarn-mode t)
(setq font-lock-maximum-decoration t)
(setq font-lock-maximum-size 262144)
;; compilation
(setq compilation-scroll-output t)
(global-set-key (kbd "C-x g") 'magit-status)

;; cscope
(setq cscope-option-do-not-update-database t)
;; (setq cscope-do-not-update-database t)
(cscope-setup)

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
	    (linum-mode 1)))

(setq clean-buffer-list-delay-general 30)

;; deepaknag 07/10/15
(add-to-list 'load-path "/opt/boxen/homebrew/share/emacs/site-lisp")

;; remove trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)
