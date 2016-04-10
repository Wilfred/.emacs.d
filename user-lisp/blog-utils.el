(require 's)
(require 'f)

(defun blog-file-name (title &optional time)
  (format "%s-%s.markdown"
          ;; use `parse-time-string' to reverse this formatting
          (format-time-string "%Y-%m-%d" time)
          (s-dashed-words title)))

(defun blog-new-post ()
  "Create a new blog post with appropriate filename and header."
  (interactive)
  (let* ((dir (ido-read-directory-name "Posts directory: "))
         (title (read-string "Post title: "))
         (file-name (blog-file-name title)))
    (find-file (f-join dir file-name))
    (insert (format "--- 
layout: post
title: \"%s\"
---

" title))))

(defun convert-creole ()
  "Convert creole text in the current buffer to markdown."
  (interactive)
  (goto-char (point-min))
  ;; h2 headings
  (query-replace-regexp "^==\\(.*\\)$" "##\\1")
  (goto-char (point-min))
  ;; inline code
  (query-replace-regexp "{{{\\(.*?\\)}}}" "`\\1`")
  (goto-char (point-min))
  ;; code blocks
  ;; FIXME: this is GFM, we'd be better off with vanilla markdown
  (query-replace-regexp (rx "{{{" (group (*? anything)) "}}}")
                        "```\\1```")
  (goto-char (point-min))
  ;; links
  (query-replace-regexp "\\[\\[\\(.*?\\)|\\(.*?\\)\\]\\]" "[\\2](\\1)")
  (goto-char (point-min))
  ;; italics (note bold syntax is the same in markdown and creole
  (query-replace-regexp "//\\(.*?\\)//" "_\\1_")
  (goto-char (point-min)))

(provide 'blog-utils)
