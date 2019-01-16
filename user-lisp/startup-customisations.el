; no startup screen
(setq-default inhibit-startup-screen t)

(defvar programming-quotes
      '("First, solve the problem. Then, write the code. -- John Johnson"
        "The computing scientist’s main challenge is not to get confused by the complexities of his own making. -- E. W. Dijkstra"
        "There are two ways of constructing a software design: One way is to make it so simple that there are obviously no deficiencies and the other way is to make it so complicated that there are no obvious deficiencies. -- C. A. R. Hoare"
        "Controlling complexity is the essence of computer programming. -- Brian Kernigan"
        "If you’re willing to restrict the flexibility of your approach, you can almost always do something better. -- John Carmack"
        "Measuring programming progress by lines of code is like measuring aircraft building progress by weight. -- Bill Gates"
        "The best code is no code at all."
        "There is not now, nor has there ever been, nor will there ever be, any programming language in which it is the least bit difficult to write bad code."
        "Code never lies, comments sometimes do. -- Ron Jeffries"
        "Simplicity carried to the extreme becomes elegance. -- Jon Franklin"
        "The unavoidable price of reliability is simplicity. -- C. A. R. Hoare"
        "Good code is short, simple, and symmetrical – the challenge is figuring out how to get there. -- Sean Parent"
        "True glory consists in doing what deserves to be written; in writing what deserves to be read. -- Pliny the Elder"
        "The whole point of getting things done is knowing what to leave undone. -- Oswald Chambers"))

(require 'random-utils)

;; populate the initial scratch buffer with a random quote.
(setq initial-scratch-message (random-choice programming-quotes))
(setq initial-major-mode #'fundamental-mode)

;; Use hard line wrapping on the quote in the scratch buffer.
(with-current-buffer (get-buffer-create "*scratch*")
  (goto-char (point-min))
  (fill-paragraph))

;; create a separate scratch buffer for elisp experimentation
(with-current-buffer (get-buffer-create "*scratch-elisp*")
  (emacs-lisp-mode))

;; create a scratch Python buffer too
(with-current-buffer (get-buffer-create "*scratch-python*")
  (python-mode))

;; create a scratch Python buffer too
(with-current-buffer (get-buffer-create "*scratch-hack*")
  ;; hack-mode may not be installed.
  (ignore-errors (hack-mode)))

(with-current-buffer (get-buffer-create "*scratch-ocaml*")
  ;; hack-mode may not be installed.
  (tuareg-mode))

;; start in the scratch buffer
(switch-to-buffer  (get-buffer-create "*scratch*"))

(provide 'startup-customisations)
