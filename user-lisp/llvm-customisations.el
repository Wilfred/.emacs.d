(require 'llvm-mode)

;; TODO: llvm-mode should really inherit from prog-mode.
(add-hook 'llvm-mode-hook #'highlight-symbol-mode)

(provide 'llvm-customisations)
