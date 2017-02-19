;;; eval-in-repl-ruby.el --- ESS-like eval for ruby  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-  Kazuki YOSHIDA

;; Author: Kazuki YOSHIDA <kazukiyoshida@mail.harvard.edu>
;; Keywords: tools, convenience
;; URL: https://github.com/kaz-yos/eval-in-repl
;; Version: 0.9.4

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

;; ruby.el-specific file for eval-in-repl
;; See below for configuration
;; https://github.com/kaz-yos/eval-in-repl/


;;; Code:

;;;
;;; Require dependencies
(require 'eval-in-repl)
(require 'ruby-mode)
(require 'inf-ruby)


;;;
;;; RUBY-MODE RELATED
;;; eir-send-to-ruby
(defalias 'eir-send-to-ruby
  (apply-partially 'eir-send-to-repl
                   ;; fun-change-to-repl
                   #'run-ruby
                   ;; fun-execute
                   #'comint-send-input)
  "Send expression to *ruby* and have it evaluated.")


;;; eir-eval-in-ruby
;; http://www.reddit.com/r/emacs/comments/1h4hyw/selecting_regions_rubyel/
;;;###autoload
(defun eir-eval-in-ruby ()
  "eval-in-repl for Ruby."
  (interactive)
  ;; Define local variables
  (let* (;; Save current point
	 (initial-point (point)))
    ;;
    (eir-repl-start "\\*ruby\\*" #'run-ruby t)

    ;; Check if selection is present
    (if (and transient-mark-mode mark-active)
	;; If selected, send region
	(eir-send-to-ruby (buffer-substring-no-properties (point) (mark)))

      ;; If not selected, do all the following
      ;; Move to the beginning of line
      (beginning-of-line)
      ;; Set mark at current position
      (set-mark (point))
      ;; Go to the end of line
      (end-of-line)
      ;; Send region if not empty
      (if (not (equal (point) (mark)))
	  (eir-send-to-ruby (buffer-substring-no-properties (point) (mark)))
	;; If empty, deselect region
	(setq mark-active nil))

      ;; Move to the next statement code if jumping
      (if eir-jump-after-eval
          (eir-next-code-line)
        ;; Go back to the initial position otherwise
        (goto-char initial-point)))))


(provide 'eval-in-repl-ruby)
;;; eval-in-repl-ruby.el ends here

