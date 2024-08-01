;; This works when using emacs --daemon + emacsclient
(add-hook 'after-make-frame-functions (lambda (frame) (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code-12:medium")))
;; This works when using emacs without server/client
(set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code-12:medium")
;; I haven't found one statement that makes both of the above situations work, so I use both for now

(set-frame-font "Fira Code-12:medium" nil t)

;; Enable the www ligature in every possible major mode
(ligature-set-ligatures 't '("www"))

;; Enable ligatures in programming modes
(ligature-set-ligatures
 'prog-mode
 '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
   ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
   "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
   "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
   "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
   "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
   "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
   "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
   "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
   "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
