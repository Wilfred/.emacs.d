(require 'flycheck)


(flycheck-define-checker python-pyflakes
  "A Python syntax and style checker using the pyflakes utility.

See URL `http://pypi.python.org/pypi/pyflakes'."
  :command ("pyflakes" source-inplace)
  :error-patterns
  ((error line-start (file-name) ":" line ":" (message) line-end))
  :modes python-mode)

(add-to-list 'flycheck-disabled-checkers 'python-flake8)
(add-to-list 'flycheck-disabled-checkers 'python-pylint)
(add-to-list 'flycheck-checkers 'python-pyflakes)
