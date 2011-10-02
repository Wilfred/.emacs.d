(defun nxml-pretty-print-region (begin end)
  "Pretty format XML markup in region.
The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (replace-regexp ">[ \\t]*<" ">\n<" nil begin end)
    (indent-region begin end)))

(provide 'xml-customisations)
