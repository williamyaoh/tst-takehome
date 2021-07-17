(custom-set-variables
 '(fringe-mode (quote (0 . 0)) nil (fringe))
 '(show-paren-delay 0))

(when (custom-theme-p 'github-modern)
  (custom-theme-set-faces
   'github-modern
   '(highlight-indentation-current-column-face ((t (:background "light gray"))))
   '(highlight-indentation-face ((t (:background "white smoke"))))))
