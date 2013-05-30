;;; execute-commands.el --- asynchronous sequential commands

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; When running commands from Emacs, I want them to be asynchronous,
;; so they don't hang Emacs. I also want output to be displayed as
;; soon as possible, so slow commands are still shown in
;; realtime. Finally, I want to be be able to excute commands
;; sequentially, so I can be sure the previous commands are finished.

;; This code is taken from http://stackoverflow.com/a/16816575/509706 .

;; Example usage:
;; (with-current-buffer (get-buffer-create "*output*") (erase-buffer))
;; (execute-commands "*output*"
;;                   "echo 1"
;;                   "sleep 1"
;;                   "echo 2; sleep 2; echo 3"
;;                   "sleep 1; date; sleep 2; date; sleep 2; date"
;;                   "ls /")

(defun execute-commands (buffer &rest commands)
  "Execute a list of shell commands sequentially"
  (with-current-buffer buffer
    (set (make-local-variable 'commands-list) commands)
    (start-next-command)))

(defun start-next-command ()
  "Run the first command in the list"
  (if (null commands-list)
      (insert "\nDone.")
    (let ((command  (car commands-list)))
      (setq commands-list (cdr commands-list))
      (insert (format ">>> %s\n" command))
      (let ((process (start-process-shell-command command (current-buffer) command)))
        (set-process-sentinel process 'sentinel)))))

(defun sentinel (p e)
  "After a process exited, call `start-next-command' again"
  (let ((buffer (process-buffer p)))
    (when (not (null buffer))
      (with-current-buffer buffer
        (start-next-command)))))

(provide 'execute-commands)
