

(defadvice viper-maybe-checkout (around viper-svn-git-checkin-fix activate)
  "Advise viper-maybe-checkout to ignore svn and git files."
  (let ((file (expand-file-name (buffer-file-name buf))))
    (when (and (featurep 'vc-hooks)
               (not (memq (vc-backend file) '(nil SVN Git))))
      ad-do-it)))

(ad-activate 'viper-maybe-checkout)

;; the above advice doesn't work, so just rip out the function for now
(defun viper-maybe-checkout (buf))

;; for some reason, sometimes C-z doesn't map to viper-change-state-to-vi
(global-set-key (kbd "<f10>") 'viper-change-state-to-vi)
