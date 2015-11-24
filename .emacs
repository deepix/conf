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
			     auto-complete
			     flycheck
			     ggtags
			     xcscope
			     function-args
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

(require 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))
(define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
(define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
(define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)
(define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)

;;; Setup auto-complete ;;;
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(setq ac-delay 0.1)

(add-hook 'after-init-hook 'global-flycheck-mode)
(global-set-key (kbd "C-c <up>") 'flycheck-next-error)
(global-set-key (kbd "C-c <down>") 'flycheck-previous-error)
(global-set-key (kbd "C-c l") 'flycheck-list-errors)
(setq-default flymake-no-changes-timeout '3)

(when (fboundp 'winner-mode)
      (winner-mode 1))

(require 'function-args)
(fa-config-default)

(add-hook 'c-mode-common-hook   'hs-minor-mode)

;; use with gdb -i=mi ...
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

(defun color-theme-emacs-default ()
  ;; change theme to day/night depending on time of day
  (load-theme 'solarized-dark t)
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

;; dev environment
(cwarn-mode t)
(which-func-mode t)

;; syntax highlighting
(global-font-lock-mode t)
(global-cwarn-mode t)
(setq font-lock-maximum-decoration t)
(setq font-lock-maximum-size 262144)
;; compilation
(setq compilation-scroll-output t)

;; cscope
(setq cscope-option-do-not-update-database t)
(setq cscope-program "gtags-cscope")
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
            (linum-mode 1)
	    (c-set-style "linux-tabs-only")
	    (set-face-attribute 'linum nil :background "#222")
))


(setq clean-buffer-list-delay-general 30)

;; remove trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(custom-set-faces (if (not window-system)
		      '(default ((t (:background "nil"))))))
