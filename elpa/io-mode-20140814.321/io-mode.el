;;; io-mode.el --- Major mode to edit Io language files in Emacs

;; Copyright (C) 2010 Sergei Lebedev

;; Version: 20100405
;; Package-Version: 20140814.321
;; Keywords: languages, io
;; Author: Sergei Lebedev <superbobry@gmail.com>
;; URL: https://github.com/superbobry/io-mode

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; No documentation is availible at the moment, but a nice and clean
;; README is soon to come.

;;; Installation

;; In your shell:

;;     $ cd ~/.emacs.d/packges
;;     $ git clone git://github.com/superbobry/io-mode.git

;; In your Emacs config:

;;     (add-to-list 'load-path "~/.emacs.d/packages/io-mode")
;;     (require 'io-mode)

;;; Thanks

;; Major thanks to defunkt's coffee-mode, which helped me to get over most
;; of the tought parts of writing a major mode.
;;
;; Sources:
;; http://renormalist.net/Renormalist/EmacsLanguageModeCreationTutorial
;;


;;; Code:

(require 'comint)
(require 'font-lock)
(require 'hideshow)
(require 'newcomment)

;;
;; Customizable Variables
;;

(defconst io-mode-version "20100405"
  "The version of this `io-mode'.")

(defgroup io nil
  "A major mode for editing Io language."
  :group 'languages)

(defcustom io-debug-mode nil
  "Whether to run in debug mode or not.  Logs to `*Messages*'."
  :type 'boolean
  :group 'io)

(defcustom io-cleanup-whitespace t
  "Should we `delete-trailing-whitespace' on save? Probably."
  :type 'boolean
  :group 'io)

(defcustom io-tab-width tab-width
  "The tab width to use when indenting."
  :type 'integer
  :group 'io)

(defcustom io-command "io"
  "The Io command used for evaluating code. Must be in your path."
  :type 'string
  :group 'io)

(defvar io-mode-hook nil
  "A hook for you to run your own code when the mode is loaded.")

(defvar io-mode-map (make-keymap)
  "Keymap for Io major mode.")


;;
;; Macros
;;

(defun io-debug (string &rest args)
  "Print a message when in debug mode."
  (when io-debug-mode
      (apply 'message (append (list string) args))))

(defmacro io-line-as-string ()
  "Return the current line as a string."
  `(buffer-substring (point-at-bol) (point-at-eol)))

;;
;; Define Language Syntax
;;

;; Special? Self and call objects are :)
(defvar io-special-re (regexp-opt '("self" "thisContext" "call") 'symbols))

;; Operators (not all of them are present in the Io core,
;; but you can still define them, if you need it)
;; a) normal
(defvar io-operators-re
  (regexp-opt
   '("*" "/" "%" "^" "+" "-" ">>" "++" "--"
     "<<" ">" "<" "<=" ">=" "==" "!=" "&"
     "^" ".." "|" "&&" "||" "!=" "+=" "-="
     "*=" "/=" "<<=" ">>=" "&=" "|=" "%="
     "=" ":=" "<-" "<->" "->")))

;; b) special
(defvar io-operators-special-re "@\\{1,2\\}\\|?")

;; Booleans
(defvar io-boolean-re "\\b\\(true\\|false\\|nil\\)\\b")

;; Prototypes
(defvar io-prototypes-re "\\b[A-Z]+\\w*\\b")

;; Messages
(defvar io-messages-re
  (regexp-opt
   '("activate" "activeCoroCount" "and" "asString"
     "block" "break" "catch" "clone" "collectGarbage"
     "compileString" "continue" "do" "doFile" "doMessage"
     "doString" "else" "elseif" "exit" "for" "foreach"
     "foreachReversed" "forward" "getSlot" "getEnvironmentVariable"
     "hasSlot" "if" "ifFalse" "ifNil" "ifTrue"
     "isActive" "isNil" "isResumable" "list"
     "message" "method" "or" "parent" "pass" "pause"
     "perform" "performOn" "performWithArgList" "print"
     "println" "proto" "raise" "raiseResumable" "removeSlot"
     "resend" "resume" "return" "schedulerSleepSeconds"
     "sender" "setSchedulerSleepSeconds" "setSlot"
     "shallowCopy" "slotNames" "super" "system"
     "then" "thisBlock" "thisMessage" "try" "type"
     "uniqueId" "updateSlot" "wait" "while" "write"
     "writeln" "yield")
   'symbols))

;; Comments
(defvar io-comments-re "\\(\\(#\\|//\\).*$\\|/\\*\\(.\\|[\r\n]\\)*?\\*/\\)")

;; Methods
(defvar io-method-declaration-name-re "\\(\\sw+\\)\s*:=\s*\\(method\\)")

;; Variables
(defvar io-variable-declaration-name-re "\\(\\sw+\\)\s*:=\s*\\(\\sw+\\)")

;; Create the list for font-lock. Each class of keyword is given a
;; particular face.
(defvar io-font-lock-keywords
  ;; Note: order here matters!
  `((,io-special-re . font-lock-variable-name-face)
    (,io-method-declaration-name-re (1 font-lock-function-name-face))
    (,io-variable-declaration-name-re (1 font-lock-variable-name-face))
    (,io-operators-re . font-lock-builtin-face)
    (,io-operators-special-re . font-lock-warning-face)
    (,io-boolean-re . font-lock-constant-face)
    (,io-prototypes-re . font-lock-type-face)
    (,io-messages-re . font-lock-keyword-face)
    (,io-comments-re . font-lock-comment-face)))

(defvar io-string-delimiter-re
  (rx (group (or  "\"" "\"\"\""))))

(defun io-syntax-stringify ()
  "Put `syntax-table' property correctly on single/triple quotes."
  (let* ((num-quotes (length (match-string-no-properties 1)))
         (ppss (prog2
                   (backward-char num-quotes)
                   (syntax-ppss)
                 (forward-char num-quotes)))
         (string-start (and (not (nth 4 ppss)) (nth 8 ppss)))
         (quote-starting-pos (- (point) num-quotes))
         (quote-ending-pos (point))
         (num-closing-quotes
          (and string-start
               (python-syntax-count-quotes
                (char-before) string-start quote-starting-pos))))
    (cond ((and string-start (= num-closing-quotes 0))
           nil)
          ((not string-start)
           (put-text-property quote-starting-pos (1+ quote-starting-pos)
                              'syntax-table (string-to-syntax "|")))
          ((= num-quotes num-closing-quotes)
           (put-text-property (1- quote-ending-pos) quote-ending-pos
                              'syntax-table (string-to-syntax "|")))
          ((> num-quotes num-closing-quotes)
           (put-text-property quote-starting-pos quote-ending-pos
                              'syntax-table (string-to-syntax "|"))))))

;;
;; REPL
;;

(defun io-normalize-sexp (str)
  "Normalize a given Io code string, removing all newline characters."
  ;; Oddly enough, Io interpreter doesn't allow newlines anywhere,
  ;; including multiline strings and method calls, we need to make
  ;; a flat string from a code block, before it's passed to the
  ;; interpreter. Obviously, this isn't a good solution, since
  ;;   a := """Cool multiline
  ;;   string!"""
  ;; would become
  ;;   a := """Cool multiline string!"""
  ;; ...
  (replace-regexp-in-string
   ;; ... and finally strip the remaining newlines.
   "[\r\n]+" "; " (replace-regexp-in-string
                   ;; ... then strip multiple whitespaces ...
                   "\s+" " "
                   (replace-regexp-in-string
                    ;; ... then remove all newline characters near brackets
                    ;; and comas ...
                    "\\([(,]\\)[\n\r\s]+\\|[\n\r\s]+\\()\\)" "\\1\\2"
                    ;; This should really be read bottom-up, start by removing
                    ;; all comments ...
                    (replace-regexp-in-string io-comments-re "" str)))))

(defun io-repl ()
  "Launch an Io REPL using `io-command' as an inferior mode."
  (interactive)
  (let ((io-repl-buffer (get-buffer "*Io*")))
    (unless (comint-check-proc io-repl-buffer)
      (setq io-repl-buffer
            (apply 'make-comint "Io" io-command nil)))
    (pop-to-buffer io-repl-buffer)))

(defun io-repl-sexp (str)
  "Send the expression to an Io REPL."
  (interactive "sExpression: ")
  (let ((io-repl-buffer (io-repl)))
    (save-current-buffer
      (set-buffer io-repl-buffer)
      (comint-goto-process-mark)
      (insert (io-normalize-sexp str))
      ;; Probably ARTIFICIAL value should be made an option,
      ;; like `io-repl-display-sent'.
      (comint-send-input))))

(defun io-repl-sregion (beg end)
  "Send the region to an Io REPL."
  (interactive "r")
  (io-repl-sexp (buffer-substring beg end)))

(defun io-repl-sbuffer ()
  "Send the content of the buffer to an Io REPL."
  (interactive)
  (io-repl-sregion (point-min) (point-max)))

;;
;; Helper Functions
;;

(defun io-before-save ()
  "Hook run before file is saved. Deletes whitespace if `io-cleanup-whitespace' is non-nil."
  (when io-cleanup-whitespace
    (delete-trailing-whitespace)))


;;
;; Indentation
;;

(defun io-indent-line ()
  "Indent current line as Io source."
  (interactive)

  (if (= (point) (point-at-bol))
      (insert-tab)
    (save-excursion
      (let ((prev-indent 0) (cur-indent 0))
        ;; Figure out the indentation of the previous
        ;; and current lines.
        (setq prev-indent (io-previous-indent)
              cur-indent (current-indentation))

        ;; Shift one column to the left.
        (beginning-of-line)
        (insert-tab)

        (when (= (point-at-bol) (point))
          (forward-char io-tab-width))

        ;; We're too far, remove all indentation.
        (when (> (- (current-indentation) prev-indent) io-tab-width)
          (backward-to-indentation 0)
          (delete-region (point-at-bol) (point)))))))

(defun io-previous-indent ()
  "Returns the indentation level of the previous non-blank line."
  (save-excursion
    (forward-line -1)
    (if (bobp)
        0
      (progn
        (while (io-line-empty-p) (forward-line -1))
        (current-indentation)))))

(defun io-line-empty-p ()
  "Is this line empty? Returns non-nil if so, nil if not."
  (or (bobp)
      (string-match "^\\s-*$" (io-line-as-string))))

(defun io-newline-and-indent ()
  "Inserts a newline and indents it to the same level as the previous line."
  (interactive)

  ;; Remember the current line indentation level,
  ;; insert a newline, and indent the newline to the same
  ;; level as the previous line.
  (let ((prev-indent (current-indentation)) (indent-next nil))
    (newline)
    (insert-tab (/ prev-indent io-tab-width)))

  ;; Last line was a comment so this one should probably be,
  ;; too. Makes it easy to write multi-line comments (like the
  ;; one I'm writing right now).
  (when (io-previous-line-is-comment)
    ;; Using `match-string' is probably not obvious, but current
    ;; implementation of `io-previous-is-comment' is using `looking-at',
    ;; which modifies match-data variables.
    (insert (match-string 0))))


;;
;; Comments
;;

(defun io-previous-line-is-comment ()
  "Returns `t' if previous line is a comment."
  (save-excursion
    (forward-line -1)
    (io-line-is-comment)))

(defun io-line-is-comment ()
  "Returns `t' if current line is a comment."
  (save-excursion
    (backward-to-indentation 0)
    ;; No support for multi line comments yet.
    (looking-at "\\(#\\|//\\)+\s*")))


;;
;; Define Major Mode
;;

;;;###autoload
(define-derived-mode io-mode fundamental-mode
  "Io"
  "Major mode for editing Io language..."

  (define-key io-mode-map (kbd "C-m") 'io-newline-and-indent)
  (define-key io-mode-map (kbd "C-c <SPC>") 'io-repl)
  (define-key io-mode-map (kbd "C-c C-c") 'io-repl-sbuffer)
  (define-key io-mode-map (kbd "C-c C-r") 'io-repl-sregion)
  (define-key io-mode-map (kbd "C-c C-e") 'io-repl-sexp)
  (define-key io-mode-map (kbd "C-c ;") 'comment-dwim)

  ;; code for syntax highlighting
  (set (make-local-variable 'font-lock-defaults) '((io-font-lock-keywords)))

  ;; comments
  ;; a) python style
  (modify-syntax-entry ?# "< b" io-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" io-mode-syntax-table)
  ;; b) c style
  (modify-syntax-entry ?/ ". 124b" io-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" io-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" io-mode-syntax-table)

  (set (make-local-variable 'syntax-propertize-function)
       (syntax-propertize-rules
        (io-string-delimiter-re
         (0 (ignore (io-syntax-stringify))))))

  (setq comment-start "# "
        comment-start-skip "# *"
        comment-end ""
        comment-column 40
        comment-style 'indent)

  ;; strings
  (modify-syntax-entry ?\' "\"" io-mode-syntax-table)
  (modify-syntax-entry ?\" "\"" io-mode-syntax-table)

  ;; indentation
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'io-indent-line
        io-tab-width tab-width ;; just in case...
        indent-tabs-mode nil)  ;; tabs are evil..

  ;; hideshow
  (unless (assq 'io-mode hs-special-modes-alist)
    (add-to-list 'hs-special-modes-alist
                 '(io-mode "(" ")" "\\(?:#\\|/[*/]\\)")))

  ;; hooks
  (set (make-local-variable 'before-save-hook) 'io-before-save))

;;
;; On Load
;;

;; Run io-mode for files ending in .io.
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.io$" . io-mode))


(provide 'io-mode)

;;; io-mode.el ends here
