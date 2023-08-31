(require 'cl)

(defun bind (k f)
  (if (fboundp f)
      (global-set-key (kbd k) f)
    (warn "Function `%s' is not bound. Could not bind %s."
          f
          k)))

(bind "M-!" 'async-shell-command)
(defun shell-command-on-buffer (p)
  (interactive "P")
  (let ((replace-buffer (if p t nil)))
    (shell-command-on-region
     (point-min)
     (point-max)
     (read-string "Shell command: ")
     replace-buffer
     replace-buffer)))
(bind "C-!" 'shell-command-on-buffer)

(defun open-insert-line (p)
  (interactive "P")
  (if p
      (progn (beginning-of-line)
	     (open-line 1))
    (progn (end-of-line)
	   (open-line 1)
	   (next-line)))
  (indent-for-tab-command))
(bind "C-o" 'open-insert-line)
(bind "C-w" 'backward-kill-word)
(bind "C-x C-k" 'kill-region)
(bind "C-x j" 'delete-indentation)
(bind "C-." 'indent-relative)
(bind "C-x C-o" 'delete-blank-lines)

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;;; Movement.
(setf next-line-add-newlines nil)
(setf line-move-visual t)

(bind "C-x M-k" 'windmove-up)
(bind "C-x M-j" 'windmove-down)
(bind "C-x M-l" 'windmove-right)
(bind "C-x M-h" 'windmove-left)

(use-package hideshow
  :bind (("<C-tab>" . hs-toggle-hiding)))
(use-package magit
  :bind (("C-x C-h" . magit-status)))
(use-package smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))
(use-package undo-tree
  :bind
  (("C-/" . undo-tree-undo)
   ("C-x C-/" . undo-tree-redo)))
(use-package avy
  :bind
  ("C-," . avy-goto-char-2)
  ("C-;" . avy-goto-line))
