;;;; Package.el. This must come before configurations of installed
;;;; packages. Don't delete this line.
(package-initialize)

;;;; Keybindings
(load-library "@out@/keybindings.el")

;;;; Programming hooks
(defun programming-customization ()
  "Called when entering a programming mode. General programming packages
   should add advice after this function. Programming modes should add this
   as a hook alongside whatever other hooks they need to run."
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))
(defun lisp-customization ()
  "Called when entering a Lisp mode. Lisp packages should add advice
   after this function.")

;;;; Package configuration
(load-library "@out@/packages.el")

;;;; Color theme
(load-theme 'github-modern t)

;;;; clean-mode-line
(load-library "@out@/elisp/clean-mode-line.el")
(require 'clean-mode-line)

;;;; Customizes
(setq custom-file "@out@/customize.el")
(load custom-file)

;; Disabled for now, since we don't know the best way to package fonts
;; with this Emacs installation...
;; (load-library "@out@/font.el")

;;;; General configuration
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(setq tooltip-use-echo-area t)

(setq default-left-margin-width nil)
(setq default-right-margin-width nil)

(setq inhibit-startup-message t)
(setq inhibit-startup-echo-message t)

(fset 'yes-or-no-p 'y-or-n-p)

(setq confirm-nonexistant-file-or-buffer nil)

(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
	kill-buffer-query-functions))

(setq ring-bell-function (lambda ()))
(setq show-help-function nil)

(setq-default make-backup-files nil)
(setq-default truncate-lines t)

(show-paren-mode 1)
(blink-cursor-mode 0)

(global-subword-mode 1)
(setq-default sentence-end "[\\.;:!?] ")

(define-globalized-minor-mode global-delete-selection-mode
  delete-selection-mode
  (lambda ()
    (delete-selection-mode 1)))
(global-delete-selection-mode 1)

;;;; Tabulation and indentation.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
