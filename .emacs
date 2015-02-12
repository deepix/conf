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
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; code below plagiarized from:
;; http://www.aaronbedra.com/emacs.d/
(defvar deepaknag/packages '(solarized-theme
			     theme-changer
			     auto-complete
			     color-theme
			     xcscope
			     org
			     p4
			     column-marker
			     go-mode
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
       (require 'color-theme-solarized)
       (color-theme-solarized-dark)))
  )

;; easy on eyes first
(if (< emacs-major-version 24)
    (color-theme-emacs23)
  (color-theme-emacs-default))

;; some customizations
;; gives problems on emacs23
;;(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(column-number-mode t)
(transient-mark-mode t)
(global-linum-mode 1)
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
(add-hook 'c-mode-common-hook (lambda () (interactive) (column-marker-1 72)))

;; syntax highlighting
(global-font-lock-mode t)
(global-cwarn-mode t)
(setq font-lock-maximum-decoration t)
(setq font-lock-maximum-size 262144)
;; compilation
(setq compilation-scroll-output t)
(set-default 'compile-command "make buildnsvm_assert")
(load-library "p4")

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

;;(add-to-list 'clean-buffer-list-kill-regexps
;;	     '("\\`\\*P4 .*"))
(setq clean-buffer-list-delay-general 30)

;; remove trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes (quote ("e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "cd70962b469931807533f5ab78293e901253f5eeb133a46c2965359f23bfb2ea" default)))
 '(ediff-diff-options "-w ")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-tail-colors (quote (("#eee8d5" . 0) ("#B4C342" . 20) ("#69CABF" . 30) ("#69B7F0" . 50) ("#DEB542" . 60) ("#F2804F" . 70) ("#F771AC" . 85) ("#eee8d5" . 100))))
 '(magit-diff-use-overlays nil)
 '(weechat-color-list (quote (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
