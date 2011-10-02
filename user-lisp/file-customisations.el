; better dired, with colouring
(require 'dired+)

; deleting files should go to recycle bin
(setq delete-by-moving-to-trash t)

; better backups rather than just littering the directory with foo~
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups


(provide 'file-customisations)