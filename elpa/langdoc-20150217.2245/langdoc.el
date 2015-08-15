;;; langdoc.el --- Help to define help document mode for various languages

;; Copyright (C) 2013-2015  by Tomoya Tanjo

;; Author: Tomoya Tanjo <ttanjo@gmail.com>
;; URL: https://github.com/tom-tan/langdoc/
;; Package-Version: 20150217.2245
;; Package-Requires: ((cl-lib "0.2"))
;; Keywords: convenience, eldoc

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

;; This library helps you to define help document mode for various languages.
;; `langdoc-define-help-mode' makes a major mode for help document
;; and a function to show a description of a symbol.  It takes at least six
;; arguments.
;;
;; First three arguments are to define the help document mode.
;; * MODE-PREFIX
;;   Symbol to make a help mode name and a function name.
;;   `langdoc-define-help-mode' makes a major mode named MODE-PREFIX-mode
;;   and a function named MODE-PREFIX-describe-symbol.
;; * DESCRIPTION
;;   Description for MODE-PREFIX-mode.
;; * HELPBUF-NAME
;;   Buffer name for MODE-PREFIX-mode
;;
;; Next three arguments are to define MODE-PREFIX-describe-symbol.
;; * POINTED-SYM-FN
;;   Function name which returns the string pointed by
;;   the cursor.  This function takes no arguments.
;; * SYMBOL
;;   List of strings which is used to complete words.
;; * MAKE-DOCUMENT-FN
;;   Function name which takes the word as a string
;;   and returns the help string.
;;
;; Rest of the arguments is to make links in help buffers.
;; * LINK-REGEXP
;;   Regexp string to make links.
;;   If nil, MODE-PREFIX-describe-symbol does not make any links in help buffers.
;; * LINKED-STR-FN
;;   Function name which takes substrings matched in LINK-REGEXP
;;   and returns the string to be linked.
;; * MAKE-LINK-FN
;;   Function name which takes same arguments as LINKED-STR-FN
;;   and returns a string or a cons pair (SYM . FUN).
;;   SYM is a link to other document and FUN is the function to jump to the help buffer for SYM.
;;   If it returns a string, MODE-PREFIX-describe-symbol is used to jump to SYM.
;; * PREFIX-STR, SUFFIX-STR
;;   Prefix and suffix of the string returned from LINKED-STR-FN.
;;
;; If you need a concrete example, see brainfuck-mode.el (https://github.com/tom-tan/brainfuck-mode/).

;;; Code:

(require 'button)
(require 'view)
(require 'cl-lib)
(eval-when-compile
  (require 'cl))

(defun langdoc-call-fun (b)
  (funcall (button-get b 'fun) (button-get b 'link)))

(defun langdoc-insert-link (str to fun)
  (insert-text-button str
                      'follow-link t
                      'help-echo (concat "mouse-1, RET: describe this symbol")
                      'fun fun
                      'action #'langdoc-call-fun
                      'link to))

(defmacro langdoc-if-let (lst then &rest else)
  (lexical-let ((value (car lst))
                (cnd   (cadr lst)))
    `(lexical-let ((,value ,cnd))
       (if ,value
           ,then
           ,@else))))

(defmacro langdoc-while-let (lst &rest body)
  `(while (langdoc-if-let ,lst
                          (progn ,@body t))))

(defun langdoc-matched-strings ()
  "Return a list of strings parenthesized expression in the last regexp search."
  (let ((i 0) ret)
    (langdoc-while-let (str (match-string-no-properties i))
                       (cl-incf i)
                       (add-to-list 'ret str t (lambda (a b) nil)))
    ret))

;;;###autoload
(defmacro langdoc-define-help-mode (mode-prefix description helpbuf-name
                                    pointed-sym-fn symbols make-document-fn
                                    &optional link-regexp linked-str-fn
                                      make-link-fn prefix-str suffix-str)
  "Define help-mode and describe-symbol functions.
It defines MODE-PREFIX-mode which is a major mode to show help strings,
and defines MODE-PREFIX-describe-symbol to show help strings in
MODE-PREFIX-mode.  MODE-PREFIX-describe-symbol takes a string to show
a full documentation in a help buffer.  DESCRIPTION is a description
of MODE-PREFIX-mode.  HELPBUF-NAME is a buffer name for
MODE-PREFIX-mode.

POINTED-SYM-FN is a function which recieves no arguments and returns a
string pointed by the cursor.  MODE-PREFIX-describe-symbol uses
POINTED-SYM-FN when it is interactively called.  SYMBOLS is a list of
strings to complete the argument of MODE-PREFIX-describe-symbol.
MAKE-DOCUMENT-FN is a function which takes a string and returns the
string which is a full document of the argument.

LINK-REGEXP is a regexp to make links for MODE-PREFIX-describe-symbol.
If NIL, MODE-PREFIX-describe-symbol does not make any links in help
buffers.  LINKED-STR-FN is a function which takes substrings matched
in LINK-REGEXP and returns a string to be linked.  MAKE-LINK-FN is a
function which takes same arguments as LINKED-STR-FN and returns a
string which is a link to other document.  PREFIX-STR and SUFFIX-STR
are the prefix and suffix of the return value of LINKED-STR-FN
respectively.

For instance, let LINK-REGEXP be \"`\\\\(.+\\\\)'\", LINKED-STR-FN
be (lambda (a b) (concat \"[\" b \"]\")), MAKE-LINK-FN
be (lambda (a b) b), and PREFIX-STR and SUFFIX-STR are \"`\" and
\"'\" respectively.

In this case, a string \"`linked-str'\" becomes \"`[linked-str]'\"
with a link to \"linked-str\" in help buffer ."

  (lexical-let ((mode (intern (concat (symbol-name mode-prefix) "-mode")))
                (setup (intern (concat (symbol-name mode-prefix) "-setup")))
                (desc-fn (intern (concat (symbol-name mode-prefix) "-describe-symbol"))))
    `(progn

       (define-generic-mode ,mode
         nil nil nil nil
         '(,setup)
         ,description)

       ,(when link-regexp
              `(defun ,setup ()
                 (goto-char (point-min))
                 (while (re-search-forward ,link-regexp nil t)
                   (lexical-let ((beg (match-beginning 0))
                                 (args (langdoc-matched-strings)))
                     (replace-match "" nil nil)
                     (goto-char beg)
                     (lexical-let ((str (apply ,linked-str-fn args))
                                   (link (apply ,make-link-fn args)))
                       ,(when prefix-str `(insert ,prefix-str))
                       (langdoc-insert-link str
                                            (if (consp link) (car link) link)
                                            (if (consp link)
                                                (cdr link) (quote ,desc-fn)))
                       ,(when suffix-str `(insert ,suffix-str)))))))

       (defun ,desc-fn (sym)
         (interactive
          (let* ((s (funcall ,pointed-sym-fn))
                 (enable-recursive-minibuffers t)
                 (val (completing-read
                       (if s
                           (format "Describe symbol (default %s): " s)
                           "Describe symbol: ")
                       ,symbols 'stringp nil nil nil s)))
            (list (if (equal val "") s val))))
         (if (null sym)
             (message "You didn't specify a symbol")
             (lexical-let ((buf (get-buffer-create ,helpbuf-name)))
               (with-current-buffer buf
                 (setq buffer-read-only nil)
                 (let ((doc (funcall ,make-document-fn sym)))
                   (erase-buffer)
                   (insert doc)
                   (,mode)
                   (goto-char (point-min))
                   (view-mode t)))
               (unless (equal (buffer-name) ,helpbuf-name)
                 (display-buffer buf))))))))

(provide 'langdoc)
;;; langdoc.el ends here
