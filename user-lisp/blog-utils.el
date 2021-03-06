(require 's)
(require 'f)

;; TODO: migrate these functions to https://github.com/fred-o/jekyll-modes/
(defun wh/blog-file-name (title &optional time)
  (format "%s-%s.markdown"
          ;; use `parse-time-string' to reverse this formatting
          (format-time-string "%Y-%m-%d" time)
          (s-dashed-words title)))

(defun wh/new-blog-post ()
  "Create a new blog post with appropriate filename and header."
  (interactive)
  (let* ((default-posts-dir
           (f-slash (f-expand "~/projects/wilfred.github.com/_posts/")))
         (dir (read-directory-name "Posts directory: "
                                   (when (f-exists-p default-posts-dir)
                                     default-posts-dir)))
         (title (read-string "Post title: "))
         (file-name (wh/blog-file-name title)))
    (find-file (f-join dir file-name))
    (insert (format "--- 
layout: post
title: \"%s\"
---

" title))))

(defun wh/blog-post-title ()
  "Get the post title from the YAML front matter
in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (search-forward "title: ")
    (let* ((title-start (point))
           (title-end (progn (end-of-line) (point)))
           (raw-title (buffer-substring-no-properties
                       title-start title-end)))
      (if (s-starts-with-p "\"" raw-title)
          (substring raw-title 1 -1)
        raw-title))))

(defun wh/blog-update-file-name ()
  "Set the current blog post file name according to the current title and date."
  (interactive)
  (let* ((title (wh/blog-post-title))
         ;; TODO: preserve the existing date.
         (filename (wh/blog-file-name title)))
    (rename-file (buffer-file-name) filename)
    (set-visited-file-name filename t t)))

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
