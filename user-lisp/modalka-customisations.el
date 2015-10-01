(require 'modalka)

;;; Commentary:
;; Modal editing for Emacs.
;;
;; According to `keyfreq-show`, some of my most common commands are
;; `next-line', `previous-line', `forward-char' and `backward-char'.
;; For comfort and efficiency, it makes sense to use some sort of
;; modal editing so these are a single keystroke.
;;
;; I've experimented with a number of tools to find what suits me.
;;
;; I tried sticky-keys, an operating system feature where you can
;; press Ctrl twice and it stays on (like Caps Lock) until you press
;; it again. However, Gnome offers poor visibility of Ctrl/Alt sticky
;; state and you often find that typing causes surprising behaviours
;; because modifiers are on that you didn't realise.
;;
;; God-mode is similar to sticky-keys, but only applies within
;; Emacs. I tried this for a while, but I found I had to think a lot
;; about what I wanted to do. It's also difficult because because
;; movement is a mix of C-foo, M-foo and C-M-foo. It's too general.
;;
;; I tried evil-mode, which is a superb vim emulation
;; library. However, I have mediocre vim ability and it doesn't let me
;; reuse Emacs experience. It also doesn't work well with all the
;; packages I use. It does, however, show the current state ('mode' in
;; vim terms) very well.
;;
;; I also tried defining a hydra command, with a carefully chosen list
;; of movement commands:
;;
;; (defhydra hydra-move ()
;;   "move"
;;   ("n" next-line)
;;   ("p" previous-line)
;;   ("f" forward-char)
;;   ("b" backward-char)
;;   ("a" beginning-of-line-dwim)
;;   ("e" move-end-of-line)
;;   ("v" scroll-up-command)
;;   ;; Converting M-v to V here by analogy.
;;   ("V" scroll-down-command)
;;   ("l" recenter-top-bottom))
;;
;; I then replaced the movement commands with a command that started
;; this hydra:
;;
;; (defun next-line-repeatable ()
;;   "Move to the next line, then enter `hydra-move/body'."
;;   (interactive)
;;   (next-line)
;;   (hydra-move/body))
;;
;; (global-set-key (kbd "C-n") #'next-line-repeatable)
;;
;; However, I found I didn't use this much in practice. It's so close
;; to the default movement keys that I found myself repeatedly
;; pressing C-n anyway. I also found visibility poor (it shows in the
;; minibuffer but not the current buffer) and I sometimes forgot it
;; was active. This was especially tricky in macros, where you want to
;; be careful about using C-g.
;;
;; I'm now experimenting with modalka, and initial results seem
;; promising. I like having an explicit key to toggle the minor mode:

(global-set-key (kbd "C-<return>") #'modalka-global-mode)

;; Modalka also encourages setting a different cursor when it's
;; active. This is a big help to avoid surprises.

;; Usually redundant, but it's nice to be explicit
(setq-default cursor-type 'box)
;; so we know this cursor is visibly different.
(setq modalka-cursor-type 'bar)

;; Pretty much the standard configuration taken from the modalka
;; README.md:

;; kill ring
(modalka-define-kbd "W" "M-w")
(modalka-define-kbd "Y" "M-y")
(modalka-define-kbd "w" "C-w")
(modalka-define-kbd "y" "C-y")
(modalka-define-kbd "k" "C-k")

;; movement
(modalka-define-kbd "a" "C-a")
(modalka-define-kbd "b" "C-b")
(modalka-define-kbd "e" "C-e")
(modalka-define-kbd "f" "C-f")
(modalka-define-kbd "n" "C-n")
(modalka-define-kbd "p" "C-p")
(modalka-define-kbd "v" "C-v")
(modalka-define-kbd "<" "M-<")
(modalka-define-kbd ">" "M->")
(modalka-define-kbd "l" "C-l")

;; Open files
(modalka-define-kbd "g" "C-x C-g")

(modalka-define-kbd "SPC" "C-SPC")

(provide 'modalka-customisations)
