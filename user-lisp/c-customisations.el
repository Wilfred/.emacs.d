(require 'flycheck)

;; TODO: this doesn't belong here.
(require 'which-func)
(setq which-func-modes (list #'c-mode))
(which-function-mode)

;; C++
(add-hook
 'c++-mode-hook
 (lambda ()
   (setq flycheck-clang-language-standard "c++11"
         flycheck-clang-definitions '("__STDC_LIMIT_MACROS"
                                      "__STDC_CONSTANT_MACROS"))))

(add-hook
 'c++-mode-hook
 (lambda ()
   ;; LLVM convention
   (setq c-basic-offset 2)))

(provide 'c-customisations)
