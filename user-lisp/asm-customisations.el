(require 'asm-mode)

;; GNU as comment syntax varies, but # is used on i386 and x86_64.
;; see http://en.wikipedia.org/wiki/GNU_Assembler#Comments
(setq asm-comment-char ?#)

(provide 'asm-customisations)
