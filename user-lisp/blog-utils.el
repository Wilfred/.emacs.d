(require 's)
(require 'f)

(defun new-blog-post ()
  "Create a new blog post with appropriate filename and header."
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

(defun convert-creole ()
  "Convert creole text in the current buffer to markdown."
  (interactive)
  ;; h2 headings
  (query-replace-regexp "^==\\(.*\\)$" "##\\1")
  ;; inline code
  (query-replace-regexp "{{{\\(.*?\\)}}}" "`\\1`")
  ;; italics (note bold syntax is the same in markdown and creole
  (query-replace-regexp "//\\([[:alnum:]]*\\)//" "_\\1_")
  ;; links
  (query-replace-regexp "\\[\\[\\(.*?\\)|\\(.*?\\)\\]\\]" "[\\2](\\1)"))

(provide 'blog-utils)
