(use-package tuareg
  :config
  ;; Don't move past the expression after evaluating. I usually want
  ;; to repeatedly eval the same expression.
  (setq tuareg-skip-after-eval-phrase nil)

  ;; Clarify if the current file has a .mli file. It was off by default.
  ;; Implementation: https://github.com/ocaml/tuareg/commit/ccb9ee29edbf4fad1017b382b8e9d9d5fedec89c
  ;; Option: https://github.com/ocaml/tuareg/commit/85472b109a2f8d340ccbb9cc66f25de080a71593
  (setq tuareg-mode-line-other-file t)

  ;; No mnemonic, this mode map is crowded when merlin is active.
  (define-key tuareg-mode-map (kbd "C-c s") #'wh/type-annotation)

  (require 'ocamlformat)
  (define-key tuareg-mode-map (kbd "C-c f") #'ocamlformat)

  (add-hook 'tuareg-mode-hook #'merlin-mode))

(defun wh/type-annotation ()
  "Insert a type annotation skeleton for the symbol at point."
  (interactive)
  (unless (looking-at (rx symbol-start))
    (forward-symbol -1))
  (let ((sym-name (symbol-name (symbol-at-point))))
    (insert "(")
    (forward-symbol 1)
    (insert " : )")
    (backward-char)))

(use-package merlin
  :config
  (define-key merlin-mode-map (kbd "M-.") #'merlin-locate)
  (define-key merlin-mode-map (kbd "M-,") #'merlin-pop-stack)

  ;; Don't change window when jumping to a definition.
  (setq merlin-locate-in-new-window 'never)

  ;; Show types in eldoc, and highlight other references to the same
  ;; symbol.
  (add-hook 'merlin-mode-hook #'merlin-eldoc-setup)

  ;; merlin-eldoc is more precise than highlight-symbol-mode, because it
  ;; understands shadowing.
  (add-hook 'merlin-mode-hook
            (function (lambda () (highlight-symbol-mode -1)))))

;; Style merlin occurrences to match highlight-symbol occurrences.
(use-package merlin-eldoc
  :config
  (set-face-attribute 'merlin-eldoc-occurrences-face nil :underline t)
  (set-face-attribute 'merlin-eldoc-occurrences-face nil :inherit nil))

;; Ensure we can access binaries installed with opam, such as
;; ocp-indent.
(add-to-list 'exec-path "/home/wilfred/.opam/system/bin/")
(setenv
 "CAML_LD_LIBRARY_PATH"
 "/home/wilfred/.opam/system/lib/stublibs:/usr/lib/ocaml/stublibs")
(when (executable-find "utop")
  (setq tuareg-interactive-program "utop"))

;; _oasis files are basically conf syntax.
;; http://oasis.forge.ocamlcore.org/MANUAL.html#writing-_oasis-file
(add-to-list 'auto-mode-alist '("_oasis" . conf-mode))

(provide 'ocaml-customisations)
