;;; hack-mode.el --- Major mode for the Hack programming language -*- lexical-binding: t -*-

;; Copyright (c) Facebook, Inc. and its affiliates. All Rights Reserved.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Author: John Allen <jallen@fb.com>
;; Version: 1.0.0
;; Package-Version: 20181107.1748
;; Package-Requires: ((emacs "25.1") (s "1.11.0"))
;; URL: https://github.com/hhvm/hack-mode

;;; Commentary:
;;
;; Implements `hack-mode' for the Hack programming language.  This
;; includes basic support for highlighting and indentation.
;;

;;; Code:
(require 's)

(defgroup hack nil
  "Major mode `hack-mode' for editing Hack code."
  :prefix "hack-"
  :group 'languages)

(defcustom hack-client-program-name "hh_client"
  "The command to run to communicate with hh_server."
  :type 'string
  :group 'hack-mode)

(defcustom hack-indent-offset 2
  "Indentation amount (in spaces) for Hack code."
  :safe #'integerp
  :type 'integer
  :group 'hack-mode)

(defvar hack-font-lock-keywords
  `(
    ;; <?hh must be on the first line. The only thing it may be preceded
    ;; by is a shebang. See hphp.ll.
    ;; TODO: handle //strict, //decl.
    (,(rx
       (group
        (or
         buffer-start
         (seq "#!" (* not-newline) "\n")))
       (group "<?hh"))
     (1 font-lock-comment-face)
     (2 font-lock-keyword-face))
    ;; Keywords, based on hphp.ll.
    ;; TODO: what about ... and ?? tokens?
    (,(regexp-opt
       '("exit"
         "die"
         "const"
         "return"
         "yield"
         "from"
         "try"
         "catch"
         "finally"
         "throw"
         "using"
         "if"
         "else"
         "endif"
         "while"
         "endwhile"
         "do"
         "for"
         "endfor"
         "foreach"
         "endforeach"
         "declare"
         "enddeclare"
         "instanceof"
         "as"
         "super"
         "switch"
         "endswitch"
         "case"
         "default"
         "break"
         "continue"
         "goto"
         "echo"
         "print"
         "class"
         "interface"
         "trait"
         "insteadof"
         "extends"
         "implements"
         "enum"
         "attribute"
         "category"
         "children"
         "required"
         "function"
         "new"
         "clone"
         "var"
         "callable"
         "eval"
         "include"
         "include_once"
         "require"
         "require_once"
         "namespace"
         "use"
         "global"
         "isset"
         "empty"
         "__halt_compiler"
         "__compiler_halt_offset__"
         "static"
         "abstract"
         "final"
         "private"
         "protected"
         "public"
         "unset"
         "==>"
         "list"
         "array"
         "OR"
         "AND"
         "XOR"
         "dict"
         "keyset"
         "shape"
         "type"
         "newtype"
         "where"
         "await"
         "vec"
         "varray"
         "darray"
         "inout"
         "async"
         "tuple"
         "__CLASS__"
         "__TRAIT__"
         "__FUNCTION__"
         "__METHOD__"
         "__LINE__"
         "__FILE__"
         "__DIR__"
         "__NAMESPACE__"
         "__COMPILER_FRONTEND__")
       'symbols)
     . font-lock-keyword-face)
    ;; Type definitions.
    (,(rx
       (? "?")
       symbol-start
       (or
        ;; Built-in types, based on naming_special_names.ml.
        "void"
        "resource"
        "num"
        "arraykey"
        "noreturn"
        "mixed"
        "nonnull"
        "this"
        "dynamic"
        "int"
        "bool"
        "float"
        "string"
        "array"
        "darray"
        "varray"
        "varray_or_darray"
        "integer"
        "boolean"
        "double"
        "real"
        "callable"
        "object"
        "unset"
        ;; User-defined type.
        (seq
         (0+ "_")
         (any upper)
         (* (or (syntax word) (syntax symbol)))))
       symbol-end)
     . font-lock-type-face)
    ;; We also highlight _ as a type, but don't highlight ?_.
    (,(regexp-opt '("_") 'symbols)
     . font-lock-type-face)
    (,(regexp-opt
       '("null"
         "true"
         "false")
       'symbols)
     . font-lock-constant-face)

    ;; $$ is a special variable used with the pipe operator
    ;; |>. Highlight the entire string, to avoid confusion with $s.
    (,(rx symbol-start "$$")
     . font-lock-variable-name-face)
    ;; Highlight variables that start with $, e.g. $foo. Don't
    ;; highlight the $, to make the name easier to read (consistent with php-mode).
    (,(rx symbol-start "$" (group (+ (or (syntax word) (syntax symbol)))) symbol-end)
     1 font-lock-variable-name-face)

    ;; Constants are all in upper case, and cannot start with a
    ;; digit. We use font-lock-variable-name-face for consistency with
    ;; c-mode.
    (,(rx symbol-start
          (any upper "_")
          (+ (any upper "_" digit))
          symbol-end)
     . font-lock-variable-name-face)

    ;; Highlight function names.
    (,(rx symbol-start
          "function"
          symbol-end
          (+ space)
          (group
           symbol-start
           (+ (or (syntax word) (syntax symbol)))
           symbol-end))
     1 font-lock-function-name-face)
    ;; FALLTHROUGH and UNSAFE comments, based on full_fidelity_lexer.ml.
    ;; TODO: UNSAFE_EXPR according to https://docs.hhvm.com/hack/typechecker/special
    (,(rx "//"
          (+ space)
          (or "FALLTHROUGH" "UNSAFE"))
     . font-lock-function-name-face)))

(defvar hack-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; - and < are not symbol constituents.
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)

    ;; Treat + as punctuation.
    (modify-syntax-entry ?+ "." table)

    ;; Treat & as punctuation.
    (modify-syntax-entry ?& "." table)

    ;; Treat \ as punctuation, so we can navigate between different
    ;; parts of a namespace reference.
    (modify-syntax-entry ?\\ "." table)

    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?' "\"" table)
    ;; Comments
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?\^m "> b" table)

    table))

(defun hack--comment-prefix (s)
  "Extract the leading '* ' from '* foo'."
  (when (string-match
         (rx bol (0+ space) (? "/") "*" (1+ space))
         s)
    (match-string 0 s)))

(defun hack--wrap-comment-inner (s)
  "Given a string of the form:

* a very long sentence here...

wrap it to:

* a very long
* sentence here..."
  (let* ((prefix (hack--comment-prefix s))
         (lines (s-lines s))
         (stripped-lines
          (mapcar
           (lambda (line) (s-chop-prefix prefix line))
           lines)))
    (with-temp-buffer
      (insert (s-join "\n" stripped-lines))
      (goto-char (point-min))
      (fill-paragraph)

      (let* ((wrapped-lines (s-lines (buffer-string)))
             (prefixed-lines
              (mapcar
               (lambda (line) (concat prefix line))
               wrapped-lines)))
        (s-join "\n" prefixed-lines)))))

(defun hack--fill-paragraph-star ()
  "Fill paragraph when point is inside a /* comment."
  (let* ((line-start-pos (line-beginning-position))
         (comment-start (nth 8 (syntax-ppss)))
         (comment-end nil))
    ;; Put a property on the text at point, so we can put point back afterwards.
    (put-text-property (point) (1+ (point)) 'hack-fill-start-pos t)
    (save-excursion
      (search-forward "*/")
      (setq comment-end (point)))
    (save-excursion
      (save-restriction
        ;; Narrow to the comment, to ensure we don't move beyond the end.
        (narrow-to-region comment-start comment-end)

        ;; Move over all the non-empty comment lines, considering * to
        ;; be an empty line.
        (while (and
                (not (eobp))
                (not (looking-at
                      (rx
                       bol
                       (? "/")
                       (0+ space) (? "*")
                       (0+ space)
                       eol))))
          (forward-line))

        (if (eobp)
            ;; Don't add the */ to our wrapped comment.
            (progn
              (forward-line -1)
              (end-of-line))
          ;; Exclude the trailing newline.
          (backward-char))

        (let ((contents (buffer-substring line-start-pos (point))))
          (delete-region line-start-pos (point))
          (insert (hack--wrap-comment-inner contents)))))
    (while (and (not (eobp))
                (not (get-text-property (point) 'hack-fill-start-pos)))
      (forward-char))
    (when (get-text-property (point) 'hack-fill-start-pos)
      (remove-text-properties (point) (1+ (point)) '(hack-fill-start-pos nil)))))

(defun hack-fill-paragraph (&optional _justify)
  "Fill the paragraph at point."
  (let* ((ppss (syntax-ppss))
         (in-comment-p (nth 4 ppss))
         (comment-start (nth 8 ppss))
         (comment-end nil)
         (in-star-comment-p nil))
    (when in-comment-p
      (save-excursion
        (goto-char comment-start)
        (when (looking-at (rx "/*"))
          (setq in-star-comment-p t))))
    (if in-star-comment-p
        (progn
          (hack--fill-paragraph-star)
          t)
      ;; Returning nil means that `fill-paragraph' will run, which is
      ;; sufficient for // comments.
      nil)))

(defvar hack-xhp-indent-debug-on nil)

(defvar hack-xhp-start-regex
  (rx (or
       (seq symbol-start "return" symbol-end)
       bol
       "==>"
       "?"
       "="
       "(")
      (* space)
      "<"
      (not (any ?< ?\\)))
  "The regex used to match the start of an XHP expression.")

(defun hack-xhp-indent-debug (&rest args)
  "Log ARGS if ‘hack-xhp-indent-debug-on’ is set."
  (if hack-xhp-indent-debug-on
      (apply 'message args)))

(defun hack-xhp-in-code-p ()
  "Return non-nil if point is currently in code,
i.e. not in a comment or string."
  (let* ((ppss (syntax-ppss))
         (in-string (nth 3 ppss))
         (in-comment (nth 4 ppss)))
    (and (not in-comment)
         (not in-string))))

(defun hack-xhp-indent-previous-semi (limit)
  "Search backward from point for the last semicolon.
Return nil if no semicolons were found before we reached position LIMIT.
Ignore semicolons in strings and comments."
  (if (not limit)
      (setq limit (point-min)))
  (when (> (point) limit)
    (let (res
          (keep-going t))
      (save-excursion
        (while keep-going
          (setq keep-going nil)

          (when (search-backward ";" limit t)
            (if (hack-xhp-in-code-p)
                (setq keep-going t)
              ;; semi found, done.
              (setq res (point)))))
        res))))

(defun hack-xhp-backward-whitespace ()
  "Move backwards until point is not on whitespace."
  (catch 'done
    (while t
      (when (bobp)
        (throw 'done nil))

      (let ((prev-char (char-after (1- (point))))
            (prev-syntax (syntax-after (1- (point)))))
        (unless (or (eq prev-char ?\n)
                    ;; 0 is the syntax code for whitespace.
                    (eq 0 (car-safe prev-syntax)))
          (throw 'done nil)))

      (backward-char))))

(defun hack-xhp-enclosing-brace-pos ()
  "Return the position of the innermost enclosing brace before point."
  (nth 1 (syntax-ppss)))

(defun hack-xhp-indent-offset ()
  "If point is inside an XHP expression, return the correct indentation amount.
Return nil otherwise."
  (let* ((start-pos (point))
         (min-brace
          (save-excursion
            ;; get out of anything being typed that might confuse the parsing
            (beginning-of-line)
            (hack-xhp-enclosing-brace-pos)))
         (min (save-excursion
                (or
                 (hack-xhp-indent-previous-semi min-brace)
                 min-brace
                 ;; skip past <?php
                 (+ (point-min) 5))))
         base-indent)
    ;; STEP 1: find a previous xhp element, and derive the normal
    ;; indentation from it.
    (save-excursion
      (if (and
           (> (point) min)
           (re-search-backward hack-xhp-start-regex min t)
           (hack-xhp-in-code-p))
          (setq
           base-indent
           ;; decide from this context if indentation should
           ;; be initially adjusted.
           (+
            ;; start with the indentation at this elt
            (current-indentation)
            ;; at the matched xhp element, figure out if the
            ;; indentation should be modified
            ;; TODO(abrady) too lazy to parse forward properly, these
            ;; work fine for now.
            (cond
             ;; CASE 1: matched elt is closed or self-closing e.g. <br />
             ;; or a 1-line enclosed stmt: <fbt:param>foo</fbt:param>
             ((save-excursion
                (beginning-of-line)
                (or
                 (re-search-forward "</" (line-end-position) t)
                 (re-search-forward "/> *$" start-pos t)
                 (re-search-forward "--> *$" start-pos t)))
              0)
             ;; DEFAULT: increase indent
             (t hack-indent-offset))))))
    ;; STEP 2: indentation adjustment based on what user has typed so far
    (if base-indent
        ;; STEP 2.1: we found indentation to adjust. use the current
        ;; context to determine how it should be adjusted
        (cond
         ;; CASE 0: indenting an attribute
         ((looking-at "^ *[a-zA-Z_-]+")
          base-indent)
         ;; CASE 1: Terminating a multiline php block is a special
         ;; case where we should default to php indentation as if we
         ;; were inside the braces
         ;; e.g. <div class={foo($a
         ;;                      $b)}>
         ((save-excursion
            (and
             (not (re-search-forward "^ *<" (line-end-position) t))
             (re-search-forward "}> *$" (line-end-position) t)))
          (hack-xhp-indent-debug "terminating php block")
          nil)
         ;; CASE 2: user is indenting a closing block, so out-dent
         ;; e.g.
         ;; <div>
         ;; </div>
         ((save-excursion
            (beginning-of-line)
            (re-search-forward "^ *</" (line-end-position) t))
          (+ base-indent (- hack-indent-offset)))
         ;; CASE 3: if this happens to be /> on its own
         ;; line, reduce indent (coding standard)
         ((save-excursion
            (goto-char start-pos)
            (re-search-forward "^ */> *" (line-end-position) t))
          (+ base-indent (- hack-indent-offset)))
         ;; CASE 4: close of xhp passed to a function, e.g.
         ;; foo(
         ;;   <xhp>
         ;; );
         ((save-excursion
            (re-search-forward "^ *);" (line-end-position) t))
          (+ base-indent (- hack-indent-offset)))
         ;; DEFAULT: no modification.
         (t base-indent))
      ;; STEP 2.2: FIRST STATEMENT AFTER XHP. if we're after
      ;; the close of an xhp statement it still messes up the php
      ;; indentation, so check that here and override
      (cond
       ;; CASE 1: multiline self-enclosing tag or closing tag
       ;; e.g.
       ;; <div
       ;;   foo="bar"
       ;; />;
       ;; - or -
       ;; <div>
       ;;  ...
       ;; </div>;
       ((save-excursion
          (hack-xhp-backward-whitespace)
          (and
           (looking-back "\\(/>\\|</.*>\\);" nil)
           ;; don't match single-line xhp $foo = <x:frag />;
           (not (re-search-backward "^ *\\$" (line-beginning-position) t))))
        ;; previous statement IS xhp. check what user has typed so
        ;; far
        (+
         (save-excursion (hack-xhp-backward-whitespace) (current-indentation))
         (cond
          ;; CASE 0: user typed a brace. outdent even more
          ((looking-at ".*}") (* -2 hack-indent-offset))
          ;; CASE 1: close of case in a switch stmt, e.g. case FOO:
          ((looking-at ".*: *$") (* -2 hack-indent-offset))
          ;; DEFAULT
          (t (- hack-indent-offset)))))
       ;; DEFAULT: not first stmt after xhp, indent normally
       (t nil)))))

(defun hack-xhp-indent-preserve-point (offset)
  "Indent the current line by OFFSET spaces.
Ensure point is still on the same part of the line afterwards."
  (let ((point-offset (- (current-column) (current-indentation))))
    (indent-line-to offset)

    ;; Point is now at the beginning of indentation, restore it
    ;; to its original position (relative to indentation).
    (when (>= point-offset 0)
      (move-to-column (+ (current-indentation) point-offset)))))

(defun hack-xhp-indent ()
  "Perform XHP indentation if appropriate."
  (interactive)
  (let ((indent (hack-xhp-indent-offset)))
    (when indent
      (hack-xhp-indent-debug "xhp indent!!!")
      (hack-xhp-indent-preserve-point indent))
    indent))

(defun hack-indent-line ()
  "Indent the current line of Hack code.
Preserves point position in the line where possible."
  (interactive)
  (let* ((point-offset (- (current-column) (current-indentation)))
         (ppss (syntax-ppss (line-beginning-position)))
         (paren-depth (nth 0 ppss))
         (current-paren-pos (nth 1 ppss))
         (text-after-paren
          (when current-paren-pos
            (save-excursion
              (goto-char current-paren-pos)
              (buffer-substring
               (1+ current-paren-pos)
               (line-end-position)))))
         (in-multiline-comment-p (nth 4 ppss))
         (current-line (buffer-substring (line-beginning-position) (line-end-position))))
    ;; If the current line is just a closing paren, unindent by one level.
    (when (and
           (not in-multiline-comment-p)
           (string-match-p (rx bol (0+ space) (or ")" "}")) current-line))
      (setq paren-depth (1- paren-depth)))
    (cond
     ;; In multiline comments, ensure the leading * is indented by one
     ;; more space. For example:
     ;; /*
     ;;  * <- this line
     ;;  */
     (in-multiline-comment-p
      ;; Don't modify lines that don't start with *, to avoid changing the indentation of commented-out code.
      (when (or (string-match-p (rx bol (0+ space) "*") current-line)
                (string= "" current-line))
        (hack-xhp-indent-preserve-point (1+ (* hack-indent-offset paren-depth)))))
     ;; Indent according to the last paren position, if there is text
     ;; after the paren. For example:
     ;; foo(bar,
     ;;     baz, <- this line
     ((and
       text-after-paren
       (not (string-match-p (rx bol (0+ space) eol) text-after-paren)))
      (let (open-paren-column)
        (save-excursion
          (goto-char current-paren-pos)
          (setq open-paren-column (current-column)))
        (hack-xhp-indent-preserve-point (1+ open-paren-column))))
     ;; Indent according to the amount of nesting.
     (t
      (hack-xhp-indent-preserve-point (* hack-indent-offset paren-depth))))
    ;; Point is now at the beginning of indentation, restore it
    ;; to its original position (relative to indentation).
    (when (>= point-offset 0)
      (move-to-column (+ (current-indentation) point-offset)))))

(defun hack-xhp-indent-line ()
  "Indent current line."
  (interactive)
  (if (not (hack-xhp-indent))
      (hack-indent-line)))

;; hh_server can choke if you symlink your www root
(setq find-file-visit-truename t)

;; Ensure that we use `hack-mode' for .php files, but put the
;; association at the end of the list so `php-mode' wins (if installed).
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.php$" . hack-mode) t)

;; These extensions are hack-specific.
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.hhi$" . hack-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.hack$" . hack-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.hck$" . hack-mode))

;;;###autoload
(define-derived-mode hack-mode prog-mode "Hack"
  "Major mode for editing Hack code.

\\{hack-mode-map\\}"
  (setq-local font-lock-defaults '(hack-font-lock-keywords))
  (setq-local compile-command (concat hack-client-program-name " --from emacs"))
  (setq-local indent-line-function #'hack-xhp-indent-line)
  (setq-local comment-start "// ")
  (setq-local fill-paragraph-function #'hack-fill-paragraph)
  (setq imenu-generic-expression
        ;; TODO: distinguish functions from methods.
        `(("Function"
           ,(rx
             line-start
             (? (* space) (seq symbol-start (or "private" "protected" "public") symbol-end))
             (? (* space) (seq symbol-start "static" symbol-end))
             (? (* space) (seq symbol-start "async" symbol-end))
             (* space)
             symbol-start "function" symbol-end
             (+ space)
             (group (seq symbol-start (+? any) symbol-end)))
           1)
          ("Class"
           ,(rx
             line-start
             (? (* space) (seq symbol-start "abstract" symbol-end))
             (? (* space) (seq symbol-start "final" symbol-end))
             (* space)
             symbol-start "class" symbol-end
             (+ space)
             (group (seq symbol-start (+? any) symbol-end)))
           1)
          ("Interface"
           ,(rx symbol-start "interface" symbol-end
                (+ space)
                (group (seq symbol-start (+? any) symbol-end)))
           1)
          ("Trait"
           ,(rx symbol-start "trait" symbol-end
                (+ space)
                (group (seq symbol-start (+? any) symbol-end)))
           1))))

(provide 'hack-mode)
;;; hack-mode.el ends here
