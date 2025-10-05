;;; .dir-locals.el --- Emacs directory-local variables for guile-roulette

((scheme-mode . ((geiser-guile-load-path . ("."))))
 (org-mode . ((org-babel-load-languages . ((scheme . t)))
              (org-confirm-babel-evaluate . nil))))

;;; .dir-locals.el ends here
