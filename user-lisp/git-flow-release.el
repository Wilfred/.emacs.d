;;; git-flow-release.el --- Automating git-flow release

;; Copyright (C) 2013 Wilfred Hughes <me@wilfred.me.uk>
;;
;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Created: 12 September 2013
;; Version: 1.0

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

;;; Code:

(require 's)
(require 'vc-git)
(require 'execute-commands)

(defun get-latest-version (repo-path)
  "Find the last tagged version. Assumes GNU sort (BSD sort lacks --version-sort)."
  (let ((default-directory repo-path))
    (s-trim
     (shell-command-to-string "git tag --list | grep -v vv | grep -v version | sort --version-sort | tail -n 1"))))

(defun get-next-version (repo-path)
  "Increment the minor part of the current version."
  (let* ((current-version (get-latest-version repo-path))
         (minor-version-part (car (last (s-split "\\." current-version))))
         (major-version-parts (s-join "." (butlast (s-split "\\." current-version))))
         (minor-version-number (string-to-number minor-version-part)))
    (format "%s.%s" major-version-parts (1+ minor-version-number))))

;; ideally, we'd let bind this in `git-flow-release'. This doesn't work
;; because `execute-commands' is asynchronous
(setenv "GIT_MERGE_AUTOEDIT" "no")

(defun git-flow-shell-quote (string)
  "Wrap STRING in single quotes, and quote existing single quotes to make shell safe."
  (concat "'" (s-replace "'" "'\\''" string) "'"))

;;;###autoload
(defun git-flow-release (tag-message)
  "Use gitflow to mark a new release."
  (interactive "sTag message: ")
  ;; todo: check we're in an Editd buffer
  (let* ((project-root (vc-git-root default-directory))
         (output-buffer (get-buffer-create (format "*New release %s*" project-root)))
         (next-version (get-next-version project-root))
         (process-environment process-environment)) ;; temporary environment change
    (switch-to-buffer output-buffer)
    (setq default-directory project-root)
    (setq truncate-lines nil)
    (let (buffer-read-only)
      (erase-buffer))

    (execute-commands output-buffer
                      (format "git flow release start %s" next-version)
                      (format "git flow release finish -m %s %s" (git-flow-shell-quote tag-message) next-version)
                      "git push"
                      "git push --tags"
                      "git checkout develop")))

;; TODO: implement git-flow-release-retry

(provide 'git-flow-release)
;;; git-flow-release.el ends here
