;;; tags-utils.el --- programmatically regenerate TAGS using etags

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Created: 1 October 2012
;; Version: 0.1

;;; Commentary

;; Requires ack to be installed.

;; Code convention: tags/* are internal functions, tags-* are
;; top-level interactive commands.


(autoload 'vc-git-root "vc-git")

(defun tags-utils/shell-in-dir (command directory)
  "Behaves the same as `shell-command-to-string', but allows us
to specify the directory to run the command in.

Note that it's not sufficient to use absolute paths with
shell-command-to-string, since it assumes that the buffer's value
of `default-directory' is a path that exists. If not, it crashes."
  (let ((normalized-path (file-name-as-directory directory))
        (original-default-directory default-directory)
        (command-output))
    (when (not (file-exists-p normalized-path))
      (error "Directory %s doesn't exist" normalized-path))
    (setq default-directory normalized-path)
    (setq command-output (shell-command-to-string command))
    (setq default-directory original-default-directory)
    command-output))

(defun tags-utils/regenerate (file-path)
  (tags-utils/shell-in-dir "rm -f TAGS" file-path)
  (tags-utils/shell-in-dir "ack -f --print0 | xargs --null etags -a" file-path))

(defun tags-utils/project-root (file-path)
  (unless file-path (error "This buffer isn't associated with a file"))
  (let ((git-repo-path (vc-git-root file-path)))
    (unless git-repo-path (error "This buffer isn't associated with a git repo."))
    git-repo-path))

(defun tags-regenerate ()
  "Regenerate the TAGS file in the root of the current git repository."
  (interactive)
  (let ((project-root (tags-utils/project-root default-directory)))
    (tags-utils/regenerate project-root)))

(require 'etags-select)

(global-set-key (kbd "<f6>") 'etags-select-find-tag-at-point)
(global-set-key (kbd "M-.") 'etags-select-find-tag)
(global-set-key (kbd "M-?") 'etags-select-find-tag-at-point)


(provide 'tags-utils)
