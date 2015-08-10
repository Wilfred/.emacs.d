;;; helm-match-plugin.el --- Multiple regexp matching methods for helm -*- lexical-binding: t -*-

;; Original Author: rubikitch

;; Copyright (C) 2008 ~ 2011 rubikitch
;; Copyright (C) 2011 ~ 2015 Thierry Volpiatto <thierry.volpiatto@gmail.com>

;; Author: Thierry Volpiatto <thierry.volpiatto@gmail.com>
;; URL: http://github.com/emacs-helm/helm

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

(require 'helm)
(require 'cl-lib)


(defgroup helm-match-plugin nil
  "Helm match plugin."
  :group 'helm)

(defcustom helm-mp-matching-method 'multi3
  "Matching method for helm match plugin.
You can set here different methods to match candidates in helm.
Here are the possible value of this symbol and their meaning:
- multi1: Respect order, prefix of pattern must match.
- multi2: Same but with partial match.
- multi3: The best, multiple regexp match, allow negation.
- multi3p: Same but prefix must match.
Default is multi3."
  :type  '(radio :tag "Matching methods for helm"
           (const :tag "Multiple regexp 1 ordered with prefix match"         multi1)
           (const :tag "Multiple regexp 2 ordered with partial match"        multi2)
           (const :tag "Multiple regexp 3 matching no order, partial, best." multi3)
           (const :tag "Multiple regexp 3p matching with prefix match"       multi3p))
  :group 'helm-match-plugin)


;; Internal
(defconst helm-mp-default-match-functions
  '(helm-mp-exact-match helm-mp-match))
(defconst helm-mp-default-search-functions
  '(helm-mp-exact-search helm-mp-search))


;;; Build regexps
;;
;;
(defvar helm-mp-space-regexp "[\\ ] "
  "Regexp to represent space itself in multiple regexp match.")

(defun helm-mp-split-pattern (pattern)
  "Split PATTERN if it contain spaces and return resulting list.
If spaces in PATTERN are escaped, don't split at this place.
i.e \"foo bar\"=> (\"foo\" \"bar\")
but \"foo\ bar\"=> (\"foobar\")."
  (if (string= pattern "")
      '("")
    (cl-loop for s in (split-string
                       (replace-regexp-in-string helm-mp-space-regexp
                                                 "\000\000" pattern)
                       " " t)
          collect (replace-regexp-in-string "\000\000" " " s))))

(defun helm-mp-1-make-regexp (pattern)
  "Replace spaces in PATTERN with \"\.*\"."
  (mapconcat 'identity (helm-mp-split-pattern pattern) ".*"))


;;; Exact match.
;;
;;
;; Internal.
(defvar helm-mp-exact-pattern-str nil)
(defvar helm-mp-exact-pattern-real nil)

(defun helm-mp-exact-get-pattern (pattern)
  (unless (equal pattern helm-mp-exact-pattern-str)
    (setq helm-mp-exact-pattern-str pattern
          helm-mp-exact-pattern-real (concat "\n" pattern "\n")))
  helm-mp-exact-pattern-real)


(defun helm-mp-exact-match (str &optional pattern)
  (string= str (or pattern helm-pattern)))

(defun helm-mp-exact-search (pattern &rest _ignore)
  (and (search-forward (helm-mp-exact-get-pattern pattern) nil t)
       (forward-line -1)))


;;; Prefix match
;;
;;
;; Internal
(defvar helm-mp-prefix-pattern-str nil)
(defvar helm-mp-prefix-pattern-real nil)

(defun helm-mp-prefix-get-pattern (pattern)
  (unless (equal pattern helm-mp-prefix-pattern-str)
    (setq helm-mp-prefix-pattern-str pattern
          helm-mp-prefix-pattern-real (concat "\n" pattern)))
  helm-mp-prefix-pattern-real)

(defun helm-mp-prefix-match (str &optional pattern)
  (setq pattern (or pattern helm-pattern))
  (let ((len (length pattern)))
    (and (<= len (length str))
         (string= (substring str 0 len) pattern ))))

(defun helm-mp-prefix-search (pattern &rest _ignore)
  (search-forward (helm-mp-prefix-get-pattern pattern) nil t))


;;; Multiple regexp patterns 1 (order is preserved / prefix).
;;
;;
;; Internal
(defvar helm-mp-1-pattern-str nil)
(defvar helm-mp-1-pattern-real nil)

(defun helm-mp-1-get-pattern (pattern)
  (unless (equal pattern helm-mp-1-pattern-str)
    (setq helm-mp-1-pattern-str pattern
          helm-mp-1-pattern-real
          (concat "^" (helm-mp-1-make-regexp pattern))))
  helm-mp-1-pattern-real)

(cl-defun helm-mp-1-match (str &optional (pattern helm-pattern))
  (string-match (helm-mp-1-get-pattern pattern) str))

(defun helm-mp-1-search (pattern &rest _ignore)
  (re-search-forward (helm-mp-1-get-pattern pattern) nil t))


;;; Multiple regexp patterns 2 (order is preserved / partial).
;;
;;
;; Internal
(defvar helm-mp-2-pattern-str nil)
(defvar helm-mp-2-pattern-real nil)

(defun helm-mp-2-get-pattern (pattern)
  (unless (equal pattern helm-mp-2-pattern-str)
    (setq helm-mp-2-pattern-str pattern
          helm-mp-2-pattern-real
          (concat "^.*" (helm-mp-1-make-regexp pattern))))
  helm-mp-2-pattern-real)

(cl-defun helm-mp-2-match (str &optional (pattern helm-pattern))
  (string-match (helm-mp-2-get-pattern pattern) str))

(defun helm-mp-2-search (pattern &rest _ignore)
  (re-search-forward (helm-mp-2-get-pattern pattern) nil t))


;;; Multiple regexp patterns 3 (permutation).
;;
;;
;; Internal
(defvar helm-mp-3-pattern-str nil)
(defvar helm-mp-3-pattern-list nil)

(defun helm-mp-3-get-patterns (pattern)
  "Return `helm-mp-3-pattern-list', a list of predicate/regexp cons cells.
e.g ((identity . \"foo\") (identity . \"bar\")).
This is done only if `helm-mp-3-pattern-str' is same as PATTERN."
  (unless (equal pattern helm-mp-3-pattern-str)
    (setq helm-mp-3-pattern-str pattern
          helm-mp-3-pattern-list
          (helm-mp-3-get-patterns-internal pattern)))
  helm-mp-3-pattern-list)

(defun helm-mp-3-get-patterns-internal (pattern)
  "Return a list of predicate/regexp cons cells.
e.g ((identity . \"foo\") (identity . \"bar\"))."
  (unless (string= pattern "")
    (cl-loop for pat in (helm-mp-split-pattern pattern)
          collect (if (string= "!" (substring pat 0 1))
                      (cons 'not (substring pat 1))
                    (cons 'identity pat)))))

(cl-defun helm-mp-3-match (str &optional (pattern helm-pattern))
  "Check if PATTERN match STR.
When PATTERN contain a space, it is splitted and matching is done
with the several resulting regexps against STR.
e.g \"bar foo\" will match \"foobar\" and \"barfoo\".
Argument PATTERN, a string, is transformed in a list of
cons cell with `helm-mp-3-get-patterns' if it contain a space.
e.g \"foo bar\"=>((identity . \"foo\") (identity . \"bar\")).
Then each predicate of cons cell(s) is called with regexp of same
cons cell against STR (a candidate).
i.e (identity (string-match \"foo\" \"foo bar\")) => t."
  (let ((pat (helm-mp-3-get-patterns pattern)))
    (cl-loop for (predicate . regexp) in pat
             always (funcall predicate
                             (condition-case _err
                                 ;; FIXME: Probably do nothing when
                                 ;; using fuzzy leaving the job
                                 ;; to the fuzzy fn.
                                 (string-match regexp str)
                               (invalid-regexp nil))))))

(defun helm-mp-3-search-base (pattern searchfn1 searchfn2)
  "Try to find PATTERN in `helm-buffer' with SEARCHFN1 and SEARCHFN2.
This is the search function for `candidates-in-buffer' enabled sources.
Use the same method as `helm-mp-3-match' except it search in buffer
instead of matching on a string.
i.e (identity (re-search-forward \"foo\" (point-at-eol) t)) => t."
  (cl-loop with pat = (if (stringp pattern)
                          (helm-mp-3-get-patterns pattern)
                          pattern)
           when (eq (caar pat) 'not) return
           ;; Pass the job to `helm-search-match-part'.
           (prog1 (list (point-at-bol) (point-at-eol))
             (forward-line 1))
           while (condition-case _err
                     (funcall searchfn1 (or (cdar pat) "") nil t)
                   (invalid-regexp nil))
           for bol = (point-at-bol)
           for eol = (point-at-eol)
           if (cl-loop for (pred . str) in (cdr pat) always
                       (progn (goto-char bol)
                              (funcall pred (condition-case _err
                                                (funcall searchfn2 str eol t)
                                              (invalid-regexp nil)))))
           do (goto-char eol) and return t
           else do (goto-char eol)
           finally return nil))

(defun helm-mp-3-search (pattern &rest _ignore)
  (when (stringp pattern)
    (setq pattern (helm-mp-3-get-patterns pattern)))
  (helm-mp-3-search-base
   pattern 're-search-forward 're-search-forward))


;;; mp-3p- (multiple regexp pattern 3 with prefix search)
;;
;;
(defun helm-mp-3p-match (str &optional pattern)
  "Check if PATTERN match STR.
Same as `helm-mp-3-match' but more strict, matching against prefix also.
e.g \"bar foo\" will match \"barfoo\" but not \"foobar\" contrarily to
`helm-mp-3-match'."
  (let* ((pat (helm-mp-3-get-patterns (or pattern helm-pattern)))
         (first (car pat)))
    (and (funcall (car first) (helm-mp-prefix-match str (cdr first)))
         (cl-loop for (predicate . regexp) in (cdr pat)
               always (funcall predicate (string-match regexp str))))))

(defun helm-mp-3p-search (pattern &rest _ignore)
  (when (stringp pattern)
    (setq pattern (helm-mp-3-get-patterns pattern)))
  (helm-mp-3-search-base
   pattern 'helm-mp-prefix-search 're-search-forward))


;;; Generic multi-match/search functions
;;
;;
(cl-defun helm-mp-match (str &optional (pattern helm-pattern))
  (let ((fun (cl-ecase helm-mp-matching-method
               (multi1 #'helm-mp-1-match)
               (multi2 #'helm-mp-2-match)
               (multi3 #'helm-mp-3-match)
               (multi3p #'helm-mp-3p-match))))
    (funcall fun str pattern)))

(defun helm-mp-search (pattern &rest _ignore)
  (let ((fun (cl-ecase helm-mp-matching-method
               (multi1 #'helm-mp-1-search)
               (multi2 #'helm-mp-2-search)
               (multi3 #'helm-mp-3-search)
               (multi3p #'helm-mp-3p-search))))
    (funcall fun pattern)))


;;; source compiler
;;  This is used only in old sources defined without helm-source.
;;
(defun helm-compile-source--match-plugin (source)
  (if (assoc 'no-matchplugin source)
      source
    (let* ((searchers        helm-mp-default-search-functions)
           (defmatch         (helm-aif (assoc-default 'match source)
                                 (helm-mklist it)))
           (defmatch-strict  (helm-aif (assoc-default 'match-strict source)
                                 (helm-mklist it)))
           (defsearch        (helm-aif (assoc-default 'search source)
                                 (helm-mklist it)))
           (defsearch-strict (helm-aif (assoc-default 'search-strict source)
                                 (helm-mklist it)))
           (matchfns         (cond (defmatch-strict)
                                   (defmatch
                                    (append helm-mp-default-match-functions defmatch))
                                   (t helm-mp-default-match-functions)))
           (searchfns        (cond (defsearch-strict)
                                   (defsearch
                                    (append searchers defsearch))
                                   (t searchers))))
      `(,(if (assoc 'candidates-in-buffer source)
             `(search ,@searchfns) `(match ,@matchfns))
         ,@source))))


;; Enable match-plugin by default in old sources.
(add-to-list 'helm-compile-source-functions 'helm-compile-source--match-plugin)

(provide 'helm-match-plugin)


;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-match-plugin.el ends here
