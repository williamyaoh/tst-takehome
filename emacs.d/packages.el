(require 'use-package)

;;; Programming languages.

(use-package emacs
  :config
  (add-hook 'emacs-lisp-mode-hook 'programming-customization)
  (add-hook 'emacs-lisp-mode-hook 'lisp-customization))
(use-package haskell-mode
  :config
  (add-hook 'haskell-mode-hook 'programming-customization))
(use-package purescript-mode
  :config
  (add-hook 'purescript-mode-hook 'programming-customization))
(use-package nix-mode
  :config
  (add-hook 'nix-mode-hook 'programming-customization))
(use-package erlang
  :config
  (add-hook 'erlang-mode-hook 'programming-customization))
(use-package elixir-mode
  :config
  (add-hook 'elixir-mode-hook 'programming-customization))
(use-package rust-mode
  :config
  (setq rust-indent-offset 2)
  (add-hook 'rust-mode-hook 'programming-customization))
(use-package lisp-mode
  :config
  (add-hook 'lisp-mode-hook 'programming-customization)
  (add-hook 'lisp-mode-hook 'lisp-customization))
(use-package cc-mode
  :config
  (setq-default c-basic-offset 4)
  (setq-default c-default-style "linux")
  (add-hook 'c-mode-hook 'programming-customization)
  (add-hook 'c++-mode-hook 'programming-customization))
(use-package slime
  :config
  (setq-default inferior-lisp-program "@sbcl@/bin/sbcl")

  (require 'slime-autoloads)
  (add-to-list 'slime-contribs 'slime-fancy)
  (add-to-list 'slime-contribs 'slime-indentation)

  (defun slime-eval-print-interactive (string)
    (interactive "MSLIME eval/print: ")
    (slime-eval-print string))
  (defun slime-format-print (string)
    (interactive "M[SLIME] Format string: ")
    (slime-eval-print
     (concat "(progn (format t \""
             string
             "\") (values))")))

  (put 'iter 'common-lisp-indent-function '(&rest (&whole 6 &rest)))

  :bind (("C-x M-:" . slime-eval-print-interactive)
         ("C-x C-M-f" . slime-format-print)
         :map slime-mode-map
         ("C-c M-t" . slime-trace-dialog-toggle-trace)
         ("C-c T" . slime-trace-dialog)))
(use-package typescript-mode
  :config
  (add-hook 'typescript-mode-hook 'programming-customization))
(use-package ps-mode
  :config
  (add-hook 'ps-mode-hook 'programming-customization))

(use-package markdown-mode
  :config
  (add-hook 'markdown-mode-hook 'programming-customization))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package ledger-mode
  :mode "ledger.dat")

(use-package highlight-indentation
  :config
  (advice-add 'programming-customization
              :after
              (lambda ()
                (highlight-indentation-current-column-mode 1))))

(use-package ido
  :config
  (ido-mode 1)
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (setq ido-create-new-buffer 'always))

(use-package smex
  :config
  (smex-initialize)
  (setq smex-prompt-string "> "))

(use-package undo-tree
  :config
  (global-undo-tree-mode 1))

(use-package recentf
  :config
  (recentf-mode t)
  (setq recentf-max-saved-items 50)
  (defun ido-recentf-open ()
    "Use `ido-completing-read' to \\[find-file] a recent file"
    (interactive)
    (if (find-file (ido-completing-read "Find recent file: " recentf-list))
	(message "Opening file...")
    (message "Aborting"))))

(use-package powerline
  :config
  (load-library "@out@/elisp/custom-powerline.el")
  (powerline-william-theme))

(use-package paren-face
  :config
  (global-paren-face-mode 1))

;;; Removed because it unnecessarily enabled `font-lock-mode'.
;;; Smarter would be to use `hi-lock-mode'.

;; (use-package rainbow-mode
;;   :config
;;   (define-globalized-minor-mode global-rainbow-mode rainbow-mode
;;     (lambda ()
;;       (rainbow-mode 1)))
;;
;;   (global-rainbow-mode 1))

(use-package hl-line
  :config
  (global-hl-line-mode 1))

(use-package tramp
  :config
  (setq tramp-default-method "ssh"))

(use-package magit)

(use-package discover
  :config
  (global-discover-mode 1))

(use-package beacon
  :config
  (beacon-mode 1))

(use-package vlf)

(use-package fill-column-indicator
  :config
  (setq-default fci-rule-column 80)
  (advice-add 'programming-customization
              :after
              (lambda () (fci-mode 1))))

(use-package hideshow
  :config
  (advice-add 'programming-customization
              :after
              (lambda ()
                (hs-minor-mode 1))))

(use-package paredit
  :config
  (advice-add 'lisp-customization
              :after
              (lambda ()
                (paredit-mode 1))))

(use-package vlf)

(use-package doc-view
  :bind
  (:map doc-view-mode-map
        ("M--" . doc-view-shrink)
        ("M-=" . doc-view-enlarge)
        ("M-g M-g" . doc-view-goto-page)))

(use-package avy
  :config
  (setq-default avy-timeout-seconds 0.25))

(use-package erc
  :config
  (setq erc-hide-list '("JOIN" "PART" "QUIT"))
  (add-hook 'erc-mode-hook
            (lambda ()
              (erc-scrolltobottom-mode 1)
              (setq erc-input-line-position -2))))
