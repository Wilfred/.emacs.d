(require 's)
(require 'f)

(defun new-blog-post ()
  (interactive)
  (let* ((dir (ido-read-directory-name "Posts directory: "))
         (title (read-string "Post title: "))
         (date-string (format-time-string "%Y-%m-%d"))
         (file-name (format "%s-%s.markdown" date-string (s-dashed-words title))))
    (find-file (f-join dir file-name))
    (insert (format "--- 
layout: post
title: \"%s\"
---

" title))))
