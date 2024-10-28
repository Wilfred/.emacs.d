;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "deadgrep" "20241027.2316"
  "Fast, friendly searching with ripgrep."
  '((emacs   "25.1")
    (dash    "2.12.0")
    (s       "1.11.0")
    (spinner "1.7.3"))
  :url "https://github.com/Wilfred/deadgrep"
  :commit "c37365013ece2a34d7913146cc251d828b6aa5fe"
  :revdesc "c37365013ece"
  :keywords '("tools")
  :authors '(("Wilfred Hughes" . "me@wilfred.me.uk"))
  :maintainers '(("Wilfred Hughes" . "me@wilfred.me.uk")))
