; indent html with tabs only
(add-hook 'html-mode-hook
  (function
   (lambda ()
     (progn
       (setq indent-tabs-mode t)
       (setq sgml-basic-offset 8)))))
                                        ;
; we use .dtml for Django templates that are largely HTML
(add-to-list 'auto-mode-alist '("\\.dtml$" . html-mode))

; highlight Django templating stuff
(defvar django-tag-face (make-face 'django-tag-face))
(set-face-foreground 'django-tag-face "Orange")
;
(defvar django-variable-face (make-face 'django-variable-face))
(set-face-foreground 'django-variable-face "Green")

(defvar django-comment-face (make-face 'django-comment-face))
(set-face-foreground 'django-comment-face "Gray")

(font-lock-add-keywords
 'html-mode
 '(("\\({%[^%]*%}\\)" 1 django-tag-face prepend)
   ("\\({{[^}]*}}\\)" 1 django-variable-face prepend)
   ("\\({#[^}]*#}\\)" 1 django-comment-face prepend)
   ))


; skeletons for Django template tags
(define-skeleton template-tag-skeleton
  "Insert a {% foo %} template tag"
  "Template tag name: "
  "{% " str " %}")
(define-skeleton template-variable-skeleton
  "Insert a {{ foo }} template variable"
  "Template variable: "
  "{{ " str " }}")
(define-skeleton template-comment-skeleton
  "Insert a {# foo #} template variable"
  "Comment: "
  "{# " str " #}")
(global-set-key "\C-ctt" 'template-tag-skeleton)
(global-set-key "\C-ctv" 'template-variable-skeleton)
(global-set-key "\C-ctc" 'template-comment-skeleton)

(defun html-linkify-region (url)
  "Wraps the region in a <a> tag with with provided URL."
  (interactive "sURL: ")
  (let* (
         (initial-cursor-position (point))
         (beginning (region-beginning))
         (end (region-end))
         (first-replacement (concat "<a href=\"" url "\">"))
         (second-replacement "</a>"))
  (goto-char beginning)
  (insert first-replacement)
  (goto-char (+ end (length first-replacement)))
  (insert second-replacement)
  (goto-char (+ initial-cursor-position (length first-replacement)))
  ))

; zen coding: converts selector-style lines to tags
; e.g. table>tr*2 becomes <table><tr></tr><tr></tr></table>
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes

(provide 'html-customisations)