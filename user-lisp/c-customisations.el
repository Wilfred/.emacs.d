(require 'flycheck)

;; todo: show eldoc in modeline, since the minibuffer is used by
;; flycheck.
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

(add-hook 'c-mode-common-hook #'flycheck-mode)

(add-hook 'c-mode-hook #'which-function-mode)

;; C++

(add-hook
 'c++-mode-hook
 (lambda ()
   (setq flycheck-clang-language-standard "c++11")))

(provide 'c-customisations)
