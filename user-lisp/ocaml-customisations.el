(use-package tuareg
  :config
  ;; Don't move past the expression after evaluating. I usually want
  ;; to repeatedly eval the same expression.
  (setq tuareg-skip-after-eval-phrase nil))

(use-package merlin
  :config
  (define-key merlin-mode-map (kbd "M-.") #'merlin-locate)
  (define-key merlin-mode-map (kbd "M-,") #'merlin-pop-stack))

(add-hook 'tuareg-mode-hook #'merlin-mode)

;; Show types in eldoc, and highlight other references to the same
;; symbol.
(add-hook 'merlin-mode-hook #'merlin-eldoc-setup)

;; merlin-eldoc is more precise than highlight-symbol-mode, because it
;; understands shadowing.
(add-hook 'merlin-mode-hook
          (function (lambda () (highlight-symbol-mode -1))))

;; Style merlin occurrences to match highlight-symbol occurrences.
(set-face-attribute 'merlin-eldoc-occurrences-face nil :underline t)
(set-face-attribute 'merlin-eldoc-occurrences-face nil :inherit nil)

;; Ensure we can access binaries installed with opam, such as
;; ocp-indent.
(add-to-list 'exec-path "/home/wilfred/.opam/system/bin/")

;; _oasis files are basically conf syntax.
;; http://oasis.forge.ocamlcore.org/MANUAL.html#writing-_oasis-file
(add-to-list 'auto-mode-alist '("_oasis" . conf-mode))
