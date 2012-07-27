(defun nxml-pretty-print-region (begin end)
  "Pretty format XML markup in region.
The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (goto-char begin)
    (while (re-search-forward ">[ \\t]*<" end t)
      (replace-match ">\n<" nil nil))
    (indent-region begin end)))

(provide 'xml-customisations)
