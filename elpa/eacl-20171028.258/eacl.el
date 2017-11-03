;;; eacl.el --- Auto-complete line(s) by grepping project

;; Copyright (C) 2017 Chen Bin
;;
;; Version: 1.0.2
;; Package-Version: 20171028.258
;; Author: Chen Bin <chenbin DOT sh AT gmail DOT com>
;; URL: http://github.com/redguardtoo/eacl
;; Package-Requires: ((emacs "24.3") (ivy "0.9.1"))
;; Keywords: abbrev, convenience, matching

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Multiple commands are provided to grep files in the project to get
;; auto complete candidates.
;; The keyword to grep is text from line beginning to current cursor.
;; Project is *automatically* detected if Git/Mercurial/Subversion is used.
;; You can override the project root by setting `eacl-project-root',
;;
;; List of commands,
;;
;; `eacl-complete-line' complete line.  You could assign key binding
;; "C-x C-l" to this command.
;;
;; `eacl-complete-statement' completes statement which ends with ";".
;; For example, input "import" and run this command.
;;
;; `eacl-complete-snippet' completes snippets which ends with "}".
;; For example, input "if" and run this command.
;;
;; `eacl-complete-tag' completes HTML tag which ends with ">".
;; For example, input "<div" and run this command.
;;
;; GNU Grep, Emacs 24.3 and counsel (https://github.com/abo-abo/swiper)
;; are required.
;;
;; Please use HomeBrew (https://brew.sh/) to install GNU Grep on macOS.
;; Then insert `(setq eacl-grep-program "ggrep")' into "~/.emacs".
;; The bundled "BSD Grep" on macOS is too outdated to use.


;;; Code:
(require 'ivy)
(require 'grep)
(require 'cl-lib)

(defgroup eacl nil
  "Emacs auto-complete line(s) by grepping project."
  :group 'tools)

(defcustom eacl-grep-program "grep"
  "GNU Grep program."
  :type 'string
  :group 'eacl)

(defcustom eacl-project-root nil
  "Project root.  If it's nil project root is detected automatically."
  :type 'string
  :group 'eacl)

(defcustom eacl-project-file '(".svn" ".hg" ".git")
  "The file/directory used to locate project root."
  :type '(repeat sexp)
  :group 'eacl)

(defcustom eacl-project-root-callback 'eacl-get-project-root
  "The callback to get project root directory.
The callback is expected to return the path of project root."
  :type 'function
  :group 'eacl)

(defvar eacl-keyword-start nil
  "The start position of multi-line keyword.  Internal variable.")

;;;###autoload
(defun eacl-get-project-root ()
  "Get project root."
  (or eacl-project-root
      (cl-some (apply-partially 'locate-dominating-file
                                default-directory)
               eacl-project-file)))

;;;###autoload
(defun eacl-current-line ()
  "Current line."
  (buffer-substring-no-properties (line-beginning-position)
                                  (point)))

(defun eacl-trim-left (s)
  "Remove whitespace at the beginning of S."
  (if (string-match "\\`[ \t\n\r]+" s) (replace-match "" t t s) s))

(defun eacl-encode(s)
  "Encode S."
  (setq s (regexp-quote s))
  ;; Be generic about quotes. Most scrip languages could use either double quotes
  ;; or single quote to wrap string.
  ;; In this case, we don't care, we just want to get mores candidates for
  ;; code completion
  ;; For example, in javascript, `import { Button } from "react-bootstrap` and
  ;; `import { Button } from 'react-bootstrap';` are same.
  (setq s (replace-regexp-in-string "'" "." s))
  (setq s (replace-regexp-in-string "\"" "." s))
  s)

(defun eacl-shell-quote-argument (argument)
  "Try `shell-quote-argument' ARGUMENT and process special characters."
  (cond
   ((eq system-type 'ms-dos)
    (shell-quote-argument argument))
   (t
    ;; We only use GNU Grep from Cygwin/MSYS2 even on Windows.
    ;; So we can safely assume the Linux Shell is available.
    ;; Below code is copied from `shell-quote-argument'.
    (if (equal argument "")
        "''"
      (replace-regexp-in-string "[^-0-9a-zA-Z<>{}[]:_./\n]" "\\\\\\&"
                                (replace-regexp-in-string
                                 "[\n\r\t ]" "[[:space:]]"
                                 argument))))))

(defun eacl-grep-exclude-opts ()
  "Create grep exclude options."
  (concat (mapconcat (lambda (e) (format "--exclude-dir='%s'" e))
                     grep-find-ignored-directories " ")
          " "
          (mapconcat (lambda (e) (format "--exclude='%s'" e))
                     grep-find-ignored-files " ")))

;;;###autoload
(defun eacl-get-keyword (cur-line)
  "Get trimmed keyword from CUR-LINE."
  (let* ((keyword (replace-regexp-in-string "^[ \t]*"
                                            ""
                                            cur-line)))
    (eacl-encode keyword)))

(defun eacl-replace-text (content start is-multiline)
  "Insert CONTENT from START to current point if IS-MULTILINE is t."
  (delete-region start (if is-multiline (point) (line-end-position)))
  (insert content))

(defun eacl-create-candidate-summary (s)
  "If S is too wide to fit into the screen, return pair summary and S."
  (let* ((w (frame-width))
         ;; display kill ring item in one line
         (key (replace-regexp-in-string "[ \t]*[\n\r]+[ \t]*" "\\\\n" s)))
    ;; strip the whitespace
    (setq key (replace-regexp-in-string "^[ \t]+" "" key))
    ;; fit to the minibuffer width
    (if (> (length key) w)
        (setq key (concat (substring key 0 (- w 4)) "...")))
    (cons key s)))

(defun eacl-complete-line-or-statement (regex cur-line keyword start)
  "Complete line or statement according to REGEX.
If REGEX is nil, we only complete current line.
CUR-LINE and KEYWORD are also required.  START is position we insert
next text.
If REGEX is not nil, complete statement."
  (let* ((default-directory (or (funcall eacl-project-root-callback) default-directory))
         (quoted-keyword (eacl-shell-quote-argument keyword))
         (cmd (format (if regex "%s -rshzoI %s -- \"%s\" *" "%s -rshI %s -- \"%s\" *")
                      eacl-grep-program
                      (eacl-grep-exclude-opts)
                      (if regex (concat quoted-keyword regex) quoted-keyword)))
         ;; Please note grep's "-z" will output null character at the end of each candidate
         (sep (if regex "\x0" "[\r\n]+"))
         (collection (split-string (shell-command-to-string cmd) sep t "[ \t\r\n]+"))
         (rlt t))
    ;; (message "keyword=%s" keyword)
    ;; (message "quoted keyword=%s" quoted-keyword)
    ;; (message "cmd=%s collection length=%s sep=%s" cmd (length collection) sep)
    (when collection
      (setq collection (delq nil (delete-dups collection)))
      (cond
       ((= 1 (length collection))
        ;; insert only candidate
        (cond
         ((string= (car collection) (buffer-substring-no-properties start (point)))
          (setq rlt nil))
         (t
          (eacl-replace-text (car collection) start regex))))
       ((> (length collection) 1)
        ;; uniq
        (when regex
          (setq collection (mapcar 'eacl-create-candidate-summary collection)))
        (ivy-read "candidates:"
                  collection
                  :action (lambda (l)
                            (if (consp l) (setq l (cdr l)))
                            (eacl-replace-text l start regex))))))
    (unless collection (setq rlt nil))
    rlt))

(defun eacl-line-beginning-position ()
  "Get line beginning position."
  (save-excursion (back-to-indentation) (point)))


;;;###autoload
(defun eacl-complete-multi-lines-internal (regex)
  "Complete multi-lines.  REGEX is used to match the lines."
  (let* ((cur-line (eacl-current-line))
         (keyword (eacl-get-keyword cur-line))
         (start (eacl-line-beginning-position))
         (continue t))
    (while continue
      (unless (eacl-complete-line-or-statement regex cur-line keyword start)
        (message "Auto-completion done!")
        (setq continue nil))
      (cond
       ((and continue (yes-or-no-p "Continue?"))
        (setq keyword (eacl-encode (eacl-trim-left (buffer-substring-no-properties start (point))))))
       (t
        (setq continue nil))))))

;;;###autoload
(defun eacl-complete-line ()
  "Complete line by grepping project."
  (interactive)
  (let* ((cur-line (eacl-current-line))
         (start (eacl-line-beginning-position))
         (keyword (eacl-get-keyword cur-line)))
    (eacl-complete-line-or-statement nil cur-line keyword start)))

;;;###autoload
(defun eacl-complete-statement ()
  "Complete statement which ends with \";\" by grepping project."
  (interactive)
  (eacl-complete-multi-lines-internal "[^;]*;"))

;;;###autoload
(defun eacl-complete-snippet ()
  "Complete snippet which ends with \"}\" by grepping in project."
  (interactive)
  (eacl-complete-multi-lines-internal "[^}]*}"))

;;;###autoload
(defun eacl-complete-tag ()
  "Complete snippet which ends with \">\" by grepping in project."
  (interactive)
  (eacl-complete-multi-lines-internal "[^>]*>"))

(provide 'eacl)
;;; eacl.el ends here

