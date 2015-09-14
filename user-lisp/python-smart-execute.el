;;; python-smart-execute.el --- Send blocks of code to a Python process

;; Copyright (C) 2015 Tom Bowles
;;
;; Created: 14 August 2015
;; Version: 0.1

;;; Commentary:

;; This package allows you to send lines, blocks or whole functions to
;; an inferior Python process.
;;
;; Suggested keybindings:
;;
;; (define-key python-mode-map (kbd "<f1>") #'python-smart-execute)
;; (define-key python-mode-map (kbd "<S-return>") #'python-smart-execute-next-line)

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

;;; code:
(require 'python)

;; We could just use `eval-when-compile' here, but it's nice to make
;; these values available at runtime so `describe-function' etc work.
(eval-and-compile
  (when (< emacs-major-version 25)
    ;; Emacs 24.X defines python-rx and python-rx-constituents in a way
    ;; that can't be used by other packages. See
    ;; http://emacs.stackexchange.com/a/14728/304 for discussion. We
    ;; copy-paste the definitions from 24.5.
    (defconst python-rx-constituents
      `((block-start          . ,(rx symbol-start
                                     (or "def" "class" "if" "elif" "else" "try"
                                         "except" "finally" "for" "while" "with")
                                     symbol-end))
        (dedenter            . ,(rx symbol-start
                                    (or "elif" "else" "except" "finally")
                                    symbol-end))
        (block-ender         . ,(rx symbol-start
                                    (or
                                     "break" "continue" "pass" "raise" "return")
                                    symbol-end))
        (decorator            . ,(rx line-start (* space) ?@ (any letter ?_)
                                     (* (any word ?_))))
        (defun                . ,(rx symbol-start (or "def" "class") symbol-end))
        (if-name-main         . ,(rx line-start "if" (+ space) "__name__"
                                     (+ space) "==" (+ space)
                                     (any ?' ?\") "__main__" (any ?' ?\")
                                     (* space) ?:))
        (symbol-name          . ,(rx (any letter ?_) (* (any word ?_))))
        (open-paren           . ,(rx (or "{" "[" "(")))
        (close-paren          . ,(rx (or "}" "]" ")")))
        (simple-operator      . ,(rx (any ?+ ?- ?/ ?& ?^ ?~ ?| ?* ?< ?> ?= ?%)))
        ;; FIXME: rx should support (not simple-operator).
        (not-simple-operator  . ,(rx
                                  (not
                                   (any ?+ ?- ?/ ?& ?^ ?~ ?| ?* ?< ?> ?= ?%))))
        ;; FIXME: Use regexp-opt.
        (operator             . ,(rx (or "+" "-" "/" "&" "^" "~" "|" "*" "<" ">"
                                         "=" "%" "**" "//" "<<" ">>" "<=" "!="
                                         "==" ">=" "is" "not")))
        ;; FIXME: Use regexp-opt.
        (assignment-operator  . ,(rx (or "=" "+=" "-=" "*=" "/=" "//=" "%=" "**="
                                         ">>=" "<<=" "&=" "^=" "|=")))
        (string-delimiter . ,(rx (and
                                  ;; Match even number of backslashes.
                                  (or (not (any ?\\ ?\' ?\")) point
                                      ;; Quotes might be preceded by a escaped quote.
                                      (and (or (not (any ?\\)) point) ?\\
                                           (* ?\\ ?\\) (any ?\' ?\")))
                                  (* ?\\ ?\\)
                                  ;; Match single or triple quotes of any kind.
                                  (group (or  "\"" "\"\"\"" "'" "'''")))))
        (coding-cookie . ,(rx line-start ?# (* space)
                              (or
                               ;; # coding=<encoding name>
                               (: "coding" (or ?: ?=) (* space) (group-n 1 (+ (or word ?-))))
                               ;; # -*- coding: <encoding name> -*-
                               (: "-*-" (* space) "coding:" (* space)
                                  (group-n 1 (+ (or word ?-))) (* space) "-*-")
                               ;; # vim: set fileencoding=<encoding name> :
                               (: "vim:" (* space) "set" (+ space)
                                  "fileencoding" (* space) ?= (* space)
                                  (group-n 1 (+ (or word ?-))) (* space) ":")))))
      "Additional Python specific sexps for `python-rx'")

    (defmacro python-rx (&rest regexps)
      "Python mode specialized rx macro.
This variant of `rx' supports common Python named REGEXPS."
      (let ((rx-constituents (append python-rx-constituents rx-constituents)))
        (cond ((null regexps)
               (error "No regexp"))
              ((cdr regexps)
               (rx-to-string `(and ,@regexps) t))
              (t
               (rx-to-string (car regexps) t)))))))

(defun pse--start-indent-at-0 (string)
  "Given python code STRING, unindent it so the minimum indent is zero."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (forward-to-indentation 0)
    (when (> (current-column) 0)
      (indent-rigidly (point-min) (point-max) (- (current-column))))
    (buffer-substring (point-min) (point-max))))

(defun pse--shell-send-region (start end &rest unused)
  "Send the region delimited by START and END to inferior Python process.
Equivalent to `python-shell-send-region' but robustly handles
single line regions, and a regions including multiple
blocks (e.g. if block followed by else block).

`python-shell-send-region' has a number of issues when sending
single-line regions that are indented. We end up sending just
\"if True:\", which is a syntax error. This is fixed in Emacs 25,
as of commit 1fcc552ac27503c."
  (interactive "r")
  (python-shell-send-string
   (pse--start-indent-at-0
    (buffer-substring-no-properties start end))))

(defun pse--related-block-p ()
  "Return non-nil if the current block is a related block.
These are blocks that can only occur after another block, such as
an elif after an if."
  (save-excursion
    (python-nav-beginning-of-statement)
    (looking-at (rx symbol-start
                    (or "elif" "else" "except" "finally")
                    symbol-end))))

(defun pse--nav-end-of-related-blocks ()
  "Move to end of current block.
If there are related blocks, move to the end of them instead."
  (interactive "^")
  (when (python-nav-beginning-of-block)
    (let ((block-indentation (current-indentation)))
      (python-nav-end-of-statement)
      (while (and (forward-line 1)
                  (not (eobp))
                  (or (and (> (current-indentation) block-indentation)
                           (or (python-nav-end-of-statement) t))
                      (pse--related-block-p)
                      (python-info-current-line-comment-p)
                      (python-info-current-line-empty-p))))
      (python-util-forward-comment -1)
      (point-marker))))

(defun pse--shell-send-block ()
  "Send the current block to the interpreter.
If the current block has relevant blocks afterwards (e.g. else,
except), then send that too."
  (interactive)
  (save-excursion
    (let (beg end)
      (python-nav-beginning-of-block)
      (beginning-of-line)
      (setq beg (point-marker))

      (pse--nav-end-of-related-blocks)
      (setq end (point-marker))

      (pse--shell-send-region beg end))))

(defun pse--statement-starts-defun-p ()
  "Return non-nil if current statement opens a defun."
  (save-excursion
    (python-nav-beginning-of-statement)
    (looking-at (python-rx defun))))

(defun pse--shell-send-statement ()
  "Send current Python statement to inferior Python process"
  (interactive)
  (save-excursion
    (let (beg end)
      (python-nav-beginning-of-statement)
      (setq beg (point))
      (python-nav-end-of-statement)
      (setq end (point))
      (pse--shell-send-region beg end))))

(defun pse--shell-send-dwim ()
  "Send the current function, block or line of code to the current Python process."
  ;; make sure we're at the end of the python shell
  (with-current-buffer (process-buffer (python-shell-get-process))
    (goto-char (point-max)))
  
  (cond 
   ;; If the region is active, send that.
   ((region-active-p)
    (pse--shell-send-region (region-beginning) (region-end))
    (deactivate-mark))
   ;; If we're on a comment or empty line, don't send anything.
   ((or
     (python-info-current-line-comment-p)
     (python-info-current-line-empty-p)))
   ;; If we're on a the first line of a function, send the whole
   ;; function.
   ((pse--statement-starts-defun-p)
    (python-shell-send-defun nil)
    (python-nav-end-of-defun)
    (backward-char))
   ;; If we're on a block (for, while, if etc), send that whole block
   ;; and relevant blocks after (elif, finally etc).
   ((python-info-statement-starts-block-p)
    (pse--shell-send-block)
    (pse--nav-end-of-related-blocks))
   ;; Otherwise, send the current statement.
   (t
    (pse--shell-send-statement)
    (python-nav-end-of-statement))))

;;;###autoload
(defun python-smart-execute ()
  "Send the Python code at point to the current Python process.
This may be a line, a block of code, or a whole function.
If the region is active, send that instead.

See also `python-smart-execute-next-line'.  and
`python-smart-execute-no-move'."
  (interactive)
  (pse--shell-send-dwim)
  (python-nav-forward-statement))

;;;###autoload
(defun python-smart-execute-next-line ()
  "Move to the next line, even if blank, after executing.
Handy for incrementally developing functions."
  (interactive)
  (pse--shell-send-dwim)
  (forward-line)
  (python-nav-beginning-of-statement))

;;;###autoload
(defun python-smart-execute-no-move ()
  "Execute, but don't move point."
  (interactive)
  (pse--shell-send-dwim))

(provide 'python-smart-execute)
;;; python-smart-execute.el ends here
