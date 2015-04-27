(require 'flycheck)

;; todo: show eldoc in modeline, since the minibuffer is used by
;; flycheck.
(add-hook 'c-mode-hook #'c-turn-on-eldoc-mode)

(require 'which-func)
(setq which-func-modes (list #'c-mode))
(add-hook 'c-mode-hook #'which-function-mode)

;; C++
(add-hook
 'c++-mode-hook
 (lambda ()
   (setq flycheck-clang-language-standard "c++11"
         flycheck-clang-definitions '("__STDC_LIMIT_MACROS"
                                      "__STDC_CONSTANT_MACROS"))))

(provide 'c-customisations)
