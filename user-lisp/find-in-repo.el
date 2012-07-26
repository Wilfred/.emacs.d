;;; find-in-repo.el --- quickly find files in a SVN or git repo

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Created: 24 July 2012
;; Version: 1.1

;;; Commentary

;; find-in-repo is an interactive command based on ido-find-file, but
;; allows you to easily choose any file in the repository.

;; You can call it with M-x find-in-repo, but I suggest you bind it
;; to a useful keybinding. This doesn't replace normal find-in-repo, so I
;; like to use C-x C-g:

;; (global-set-key (kbd "C-x C-g") 'find-in-repo)

;;; License:

;; This file is not part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

(defun find-in-repo/shell-in-dir (command directory)
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

(defun find-in-repo/git-list (directory)
  "Recursively list all the files in DIRECTORY, assuming
DIRECTORY is in a git repo. Return nil otherwise."
  (split-string
   (find-in-repo/shell-in-dir "git ls-files" directory) "\n" t))

(defun find-in-repo/svn-list (directory)
  "Recursively list all the files in DIRECTORY, assuming
DIRECTORY is in a subversion repo. Return nil otherwise."
  (split-string
   (find-in-repo/shell-in-dir "svn list --depth infinity | grep -v \"/$\" " directory) "\n" t))

(defun find-in-repo/generic-list (directory)
  "Recursively list all the files in DIRECTORY."
  (split-string
   (find-in-repo/shell-in-dir "find . -type f | sed \"s|^\./||\"" (message directory))
   "\n" t))

(autoload 'vc-git-root "vc-git")
(autoload 'vc-svn-root "vc-svn")

(defun find-in-repo/list-files (file-path)
  "If FILE-PATH is in a Subversion or git repository, list all
the files in the repo. Otherwise, list all the files in FILE-PATH
and its subdirectories.

We return a pair, with the car being the base path and the cdr
being the file name list."
  (let ((git-root (vc-git-root file-path))
        (svn-root (vc-svn-root file-path)))
    (cond (git-root (cons git-root (find-in-repo/git-list git-root)))
          (svn-root (cons svn-root (find-in-repo/svn-list svn-root)))
          (t (cons file-path (find-in-repo/generic-list file-path))))))

(autoload 'ido-completing-read "ido")

;; TODO: use a timeout
(defun find-in-repo ()
  "Use ido to a pick a file anywhere in the current project."
  (interactive)
  (unless default-directory (error "This buffer isn't associated with a file"))
  (let* ((ido-enable-flex-matching nil) ; required for acceptable performance (over 1000 items)
         (file-list-with-base (find-in-repo/list-files default-directory))
         (base-path (car file-list-with-base))
         (file-list (cdr file-list-with-base)))
    (find-file
     (concat
      base-path
      (ido-completing-read
       (format "Pick a file in %s: " base-path)
       file-list)))))

(provide 'find-in-repo)
