;;;###autoload
;; Author: Zane Ashby
;; (http://demonastery.org/2013/04/emacs-narrow-to-region-indirect/)
;; via https://gist.githubusercontent.com/RGalacho/830684e3492147374482/raw/9fa8c6d00edeb00bfeadf3ee4e8d24597d057adc/gistfile1.el

(defun narrow-to-region-indirect (start end &optional prefix)
  "Restrict editing in this buffer to the current region, indirectly.
 To deactivate indirect region when you're done, just kill the buffer.
 The new buffer is named as [wide-buffer-name].
 If non-nil, optional argument `prefix' is put ahead of indirect
buffer's name.
 If invoked with C-u, prompt user for `prefix' value."
  (interactive 
   (cond 
      ((eq current-prefix-arg nil)             ;; normal invocation
       (list (region-beginning) (region-end)))
      (t                                       ;; universal argument invocation
       (let ((prefix-readed (read-string "Prefix: "))) 
     (list (region-beginning) (region-end) prefix-readed)))))

  (deactivate-mark)
  (let ((indirect-buffer-name (format "%s[%s]" 
                                      (or prefix "") (buffer-name)))
        (buf (clone-indirect-buffer nil nil)))
    (with-current-buffer buf
      (narrow-to-region start end)
      (rename-buffer indirect-buffer-name t))
    (switch-to-buffer buf)))
