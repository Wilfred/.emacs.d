;;; helm-sys.el --- System related functions for helm. -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2016 Thierry Volpiatto <thierry.volpiatto@gmail.com>

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

;;; Code:

(require 'cl-lib)
(require 'helm)
(require 'helm-help)
(require 'helm-utils)


(defgroup helm-sys nil
  "System related helm library."
  :group 'helm)

(defface helm-top-columns
    '((t :inherit helm-header))
  "Face for helm help string in minibuffer."
  :group 'helm-sys)


(defcustom helm-top-command
  (cl-case system-type
    (darwin "env COLUMNS=%s ps -axo pid,user,pri,nice,ucomm,tty,start,vsz,%%cpu,%%mem,etime,command")
    (t      "env COLUMNS=%s top -b -n 1"))
  "Top command used to display output of top.
To use top command, a version supporting batch mode (-b option) is needed.
On Mac OSX top command doesn't support this, so ps command
is used by default instead.
If you modify this the number and order of elements displayed
should be the same as top command to have the sort commands
working properly, that is 12 elements with the 2 first being
PID and USER and the last 4 being %CPU, %MEM, TIME and COMMAND.
A format string where %s will be replaced with `frame-width'."
  :group 'helm-sys
  :type 'string)

(defcustom helm-top-poll-delay 1.5
  "Helm top poll after this dealy when `helm-top-poll-mode' is enabled.
The minimal delay allowed is 1.5, if less than this helm-top will use 1.5."
  :group 'helm-sys
  :type  'float)

(defcustom helm-top-poll-delay-post-command 1.0
  "Helm top stop polling during this delay.
This delay is additioned to `helm-top-poll-delay' after emacs stop
being idle."
  :group 'helm-sys
  :type 'float)

(defcustom helm-top-poll-preselection 'linum
  "Stay on same line or follow candidate when `helm-top-poll' update display.
Possible values are 'candidate or 'linum."
  :group'helm-sys
  :type '(radio :tag "Preferred preselection action for helm-top"
          (const :tag "Follow candidate" candidate)
          (const :tag "Stay on same line" linum)))

;;; Top (process)
;;
;;
(defvar helm-top-sort-fn nil)
(defvar helm-top-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-P")   'helm-top-run-sort-by-cpu)
    (define-key map (kbd "M-C")   'helm-top-run-sort-by-com)
    (define-key map (kbd "M-M")   'helm-top-run-sort-by-mem)
    (define-key map (kbd "M-U")   'helm-top-run-sort-by-user)
    map))

(defvar helm-top-after-init-hook nil
  "Local hook for helm-top.")

(defvar helm-top--poll-timer nil)

(defun helm-top-poll (&optional no-update delay)
  (when helm-top--poll-timer
    (cancel-timer helm-top--poll-timer))
  (condition-case nil
      (progn
        (when (and (helm-alive-p) (null no-update))
          ;; Fix quitting while process is running
          ;; by binding `with-local-quit' in init function
          ;; Issue #1521.
          (helm-force-update
           (cl-ecase helm-top-poll-preselection
             (candidate (replace-regexp-in-string
                         "[0-9]+" "[0-9]+"
                         (regexp-quote (helm-get-selection nil t))))
             (linum `(lambda ()
                       (goto-char (point-min))
                       (forward-line ,(helm-candidate-number-at-point)))))))
        (setq helm-top--poll-timer
              (run-with-idle-timer
               (helm-aif (current-idle-time)
                   (time-add it (seconds-to-time
                                 (or delay (helm-top--poll-delay))))
                 (or delay (helm-top--poll-delay)))
               nil
               'helm-top-poll)))
    (quit (cancel-timer helm-top--poll-timer))))

(defun helm-top--poll-delay ()
  (max 1.5 helm-top-poll-delay))

(defun helm-top-poll-no-update ()
  (helm-top-poll t (+ (helm-top--poll-delay)
                      helm-top-poll-delay-post-command)))

(defun helm-top-initialize-poll-hooks ()
  ;; When emacs is idle during say 20s
  ;; the idle timer will run in 20+1.5 s.
  ;; This is fine when emacs stays idle, because the next timer
  ;; will run at 21.5+1.5 etc... so the display will be updated
  ;; at every 1.5 seconds.
  ;; But as soon as emacs looses its idleness, the next update
  ;; will occur at say 21+1.5 s, so we have to reinitialize
  ;; the timer at 0+1.5.
  (add-hook 'post-command-hook 'helm-top-poll-no-update)
  (add-hook 'focus-in-hook 'helm-top-poll-no-update))

;;;###autoload
(define-minor-mode helm-top-poll-mode
    "Refresh automatically helm top buffer once enabled."
  :group 'helm-top
  :global t
  (if helm-top-poll-mode
      (progn
        (add-hook 'helm-top-after-init-hook 'helm-top-poll-no-update)
        (add-hook 'helm-top-after-init-hook 'helm-top-initialize-poll-hooks))
      (remove-hook 'helm-top-after-init-hook 'helm-top-poll-no-update)
      (remove-hook 'helm-top-after-init-hook 'helm-top-initialize-poll-hooks)))

(defvar helm-source-top
  (helm-build-in-buffer-source "Top"
    :header-name (lambda (name)
                   (concat name (if helm-top-poll-mode
                                    " (auto updating)"
                                    " (Press C-c C-u to refresh)")))
    :init #'helm-top-init
    :after-init-hook 'helm-top-after-init-hook
    :cleanup (lambda ()
               (when helm-top--poll-timer
                 (cancel-timer helm-top--poll-timer))
               (remove-hook 'post-command-hook 'helm-top-poll-no-update)
               (remove-hook 'focus-in-hook 'helm-top-poll-no-update))
    :nomark t
    :display-to-real #'helm-top-display-to-real
    :persistent-action #'helm-top-sh-persistent-action
    :persistent-help "SIGTERM"
    :help-message 'helm-top-help-message
    :follow 'never
    :keymap helm-top-map
    :filtered-candidate-transformer #'helm-top-sort-transformer
    :action-transformer #'helm-top-action-transformer))

(defvar helm-top--line nil)
(defun helm-top-transformer (candidates _source)
  "Transformer for `helm-top'.
Return empty string for non--valid candidates."
  (cl-loop for disp in candidates collect
        (cond ((string-match "^ *[0-9]+" disp) disp)
              ((string-match "^ *PID" disp)
               (setq helm-top--line (cons (propertize disp 'face 'helm-top-columns) "")))
              (t (cons disp "")))
        into lst
        finally return (or (member helm-top--line lst)
                           (cons helm-top--line lst))))

(defun helm-top--skip-top-line ()
  (let ((src-name (assoc-default 'name (helm-get-current-source))))
    (helm-aif (and (stringp src-name)
                   (string= src-name "Top")
                   (helm-get-selection nil t))
        (when (string-match-p "^ *PID" it)
          (helm-next-line)))))

(defun helm-top-action-transformer (actions _candidate)
  "Action transformer for `top'.
Show actions only on line starting by a PID."
  (let ((disp (helm-get-selection nil t)))
    (cond ((string-match "^ *[0-9]+" disp)
           (list '("kill (SIGTERM)" . (lambda (pid) (helm-top-sh "TERM" pid)))
                 '("kill (SIGKILL)" . (lambda (pid) (helm-top-sh "KILL" pid)))
                 '("kill (SIGINT)" .  (lambda (pid) (helm-top-sh "INT" pid)))
                 '("kill (Choose signal)"
                   . (lambda (pid)
                       (helm-top-sh
                        (helm-comp-read (format "Kill [%s] with signal: " pid)
                                        '("ALRM" "HUP" "INT" "KILL" "PIPE" "POLL"
                                          "PROF" "TERM" "USR1" "USR2" "VTALRM"
                                          "STKFLT" "PWR" "WINCH" "CHLD" "URG"
                                          "TSTP" "TTIN" "TTOU" "STOP" "CONT"
                                          "ABRT" "FPE" "ILL" "QUIT" "SEGV"
                                          "TRAP" "SYS" "EMT" "BUS" "XCPU" "XFSZ")
                                        :must-match t)
                        pid)))))
          (t actions))))

(defun helm-top-sh (sig pid)
  "Run kill shell command with signal SIG on PID for `helm-top'."
  (let ((cmd (format "kill -%s %s" sig pid)))
    (message "Executed %s\n%s" cmd (shell-command-to-string cmd))))

(defun helm-top-sh-persistent-action (pid)
  (delete-other-windows)
  (helm-top-sh "TERM" pid)
  (helm-force-update))

(defun helm-top-init ()
  "Insert output of top command in candidate buffer."
  (with-local-quit
    (unless helm-top-sort-fn (helm-top-set-mode-line "CPU"))
    (with-current-buffer (helm-candidate-buffer 'global)
      (call-process-shell-command
       (format helm-top-command (frame-width))
       nil (current-buffer)))))

(defun helm-top-display-to-real (line)
  "Return pid only from LINE."
  (car (split-string line)))

;; Sort top command

(defun helm-top-set-mode-line (str)
  (if (string-match "Sort:\\[\\(.*\\)\\] " helm-top-mode-line)
      (setq helm-top-mode-line (replace-match str nil nil helm-top-mode-line 1))
    (setq helm-top-mode-line (concat (format "Sort:[%s] " str) helm-top-mode-line))))

(defun helm-top-sort-transformer (candidates source)
  (helm-top-transformer
   (if helm-top-sort-fn
       (cl-loop for c in candidates
                if (string-match "^ *[0-9]+" c)
                collect c into pid-cands
                else collect c into header-cands
                finally return (append
                                header-cands
                                (sort pid-cands helm-top-sort-fn)))
       candidates)
   source))

(defun helm-top-sort-by-com (s1 s2)
  (let* ((split-1 (split-string s1))
         (split-2 (split-string s2))
         (com-1 (nth 11 split-1))
         (com-2 (nth 11 split-2)))
    (string< com-1 com-2)))

(defun helm-top-sort-by-mem (s1 s2)
  (let* ((split-1 (split-string s1))
         (split-2 (split-string s2))
         (mem-1 (string-to-number (nth 9 split-1)))
         (mem-2 (string-to-number (nth 9 split-2))))
    (> mem-1 mem-2)))

(defun helm-top-sort-by-user (s1 s2)
  (let* ((split-1 (split-string s1))
         (split-2 (split-string s2))
         (user-1 (nth 1 split-1))
         (user-2 (nth 1 split-2)))
    (string< user-1 user-2)))

(defun helm-top-run-sort-by-com ()
  (interactive)
  (helm-top-set-mode-line "COM")
  (setq helm-top-sort-fn 'helm-top-sort-by-com)
  (helm-update (replace-regexp-in-string
                "[0-9]+" "[0-9]+"
                (regexp-quote (helm-get-selection nil t)))))

(defun helm-top-run-sort-by-cpu ()
  (interactive)
  (helm-top-set-mode-line "CPU")
  (setq helm-top-sort-fn nil)
  (helm-update (replace-regexp-in-string
                "[0-9]+" "[0-9]+"
                (regexp-quote (helm-get-selection nil t)))))

(defun helm-top-run-sort-by-mem ()
  (interactive)
  (helm-top-set-mode-line "MEM")
  (setq helm-top-sort-fn 'helm-top-sort-by-mem)
  (helm-update (replace-regexp-in-string
                "[0-9]+" "[0-9]+"
                (regexp-quote (helm-get-selection nil t)))))

(defun helm-top-run-sort-by-user ()
  (interactive)
  (helm-top-set-mode-line "USER")
  (setq helm-top-sort-fn 'helm-top-sort-by-user)
  (helm-update (replace-regexp-in-string
                "[0-9]+" "[0-9]+"
                (regexp-quote (helm-get-selection nil t)))))


;;; X RandR resolution change
;;
;;
;;; FIXME I do not care multi-display.

(defun helm-xrandr-info ()
  "Return a pair with current X screen number and current X display name."
  (with-temp-buffer
    (call-process "xrandr" nil (current-buffer) nil
                  "--current")
    (let (screen output)
      (goto-char (point-min))
      (save-excursion
        (when (re-search-forward "\\(^Screen \\)\\([0-9]\\):" nil t)
          (setq screen (match-string 2))))
      (when (re-search-forward "^\\(.*\\) connected" nil t)
        (setq output (match-string 1)))
      (list screen output))))

(defun helm-xrandr-screen ()
  "Return current X screen number."
  (car (helm-xrandr-info)))

(defun helm-xrandr-output ()
  "Return current X display name."
  (cadr (helm-xrandr-info)))

(defvar helm-source-xrandr-change-resolution
  '((name . "Change Resolution")
    (candidates
     . (lambda ()
         (with-temp-buffer
           (call-process "xrandr" nil (current-buffer) nil
                         "--screen" (helm-xrandr-screen) "-q")
           (goto-char 1)
           (cl-loop with modes = nil
                 while (re-search-forward "   \\([0-9]+x[0-9]+\\)" nil t)
                 for mode = (match-string 1)
                 unless (member mode modes)
                 collect mode into modes
                 finally return modes))))
    (action
     ("Change Resolution"
      . (lambda (mode)
          (call-process "xrandr" nil nil nil
                        "--screen" (helm-xrandr-screen)
                        "--output" (helm-xrandr-output)
                        "--mode" mode))))))


;;; Emacs process
;;
;;
(defvar helm-source-emacs-process
  '((name . "Emacs Process")
    (init . (lambda () (list-processes--refresh)))
    (candidates . (lambda () (mapcar #'process-name (process-list))))
    (persistent-action . (lambda (elm)
                           (delete-process (get-process elm))
                           (helm-delete-current-selection)))
    (persistent-help . "Kill Process")
    (action ("Kill Process" . (lambda (elm)
                                (delete-process (get-process elm)))))))


;;;###autoload
(defun helm-top ()
  "Preconfigured `helm' for top command."
  (interactive)
  (add-hook 'helm-after-update-hook 'helm-top--skip-top-line)
  (save-window-excursion
    (unless helm-alive-p (delete-other-windows))
    (unwind-protect
         (helm :sources 'helm-source-top
               :buffer "*helm top*" :full-frame t
               :candidate-number-limit 9999
               :preselect "^\\s-*[0-9]+")
      (remove-hook 'helm-after-update-hook 'helm-top--skip-top-line))))

;;;###autoload
(defun helm-list-emacs-process ()
  "Preconfigured `helm' for emacs process."
  (interactive)
  (helm-other-buffer 'helm-source-emacs-process "*helm process*"))

;;;###autoload
(defun helm-xrandr-set ()
  "Preconfigured helm for xrandr."
  (interactive)
  (helm :sources 'helm-source-xrandr-change-resolution
        :buffer "*helm xrandr*"))

(provide 'helm-sys)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-sys.el ends here
