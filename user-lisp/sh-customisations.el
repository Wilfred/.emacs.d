(add-to-list 'auto-mode-alist '("\\.zsh$" . sh-mode))
(add-to-list 'auto-mode-alist '("PKGBUILD$" . sh-mode))

;; Backported from http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=63d0a3c63f833faad7a04fb4bf384d55ae6ae8d1
(require 'sh-script)
(setq sh-mode-syntax-table-input
      `((sh . nil)
        ;; Treat ' as punctuation rather than a string delimiter, as RPM
        ;; files often contain prose with apostrophes.
        (rpm . (,sh-mode-syntax-table ?\' "."))))

(provide 'sh-customisations)
