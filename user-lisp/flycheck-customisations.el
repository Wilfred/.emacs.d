
;; By default flycheck stops highlighting if there are more than four
;; lines of errors, which is super confusing. Increase that limit. 
(setq flycheck-highlighting-style
      '(conditional 9999 level-face
                    (delimiters "" "")))
