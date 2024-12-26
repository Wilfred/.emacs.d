;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "deadgrep" "20241210.1630"
  "Fast, friendly searching with ripgrep."
  '((emacs   "25.1")
    (dash    "2.12.0")
    (s       "1.11.0")
    (spinner "1.7.3"))
  :url "https://github.com/Wilfred/deadgrep"
  :commit "bb555790c6f404572d537e1e4adec8b4ff0515f5"
  :revdesc "bb555790c6f4"
  :keywords '("tools")
  :authors '(("Wilfred Hughes" . "me@wilfred.me.uk"))
  :maintainers '(("Wilfred Hughes" . "me@wilfred.me.uk")))
