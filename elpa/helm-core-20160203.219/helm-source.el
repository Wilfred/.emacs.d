;;; helm-source.el --- Helm source creation. -*- lexical-binding: t -*-

;; Copyright (C) 2015  Thierry Volpiatto <thierry.volpiatto@gmail.com>

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

;;; Commentary:

;; Interface to create helm sources easily.
;; Actually the eieo objects are transformed in alist for compatibility.
;; In the future this package should allow creating source as eieo objects
;; without conversion to alist, teaching helm to read such a structure.
;; The compatibility with alists would be kept.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'helm-lib)

(defvar helm-fuzzy-sort-fn)
(defvar helm-fuzzy-match-fn)
(defvar helm-fuzzy-search-fn)

(declare-function helm-init-candidates-in-buffer "helm.el")
(declare-function helm-interpret-value "helm.el")
(declare-function helm-fuzzy-highlight-matches "helm.el")


(defgeneric helm--setup-source (source)
  "Prepare slots and handle slot errors before creating a helm source.")

(defgeneric helm-setup-user-source (source)
  "Allow users modifying slots in SOURCE just before creation.")


;;; Classes for sources
;;
;;
(defclass helm-source ()
  ((name
    :initarg :name
    :initform nil
    :custom string
    :documentation
    "  The name of the source.
  A string which is also the heading which appears
  above the list of matches from the source. Must be unique.")

   (header-name
    :initarg :header-name
    :initform nil
    :custom function
    :documentation
    "  A function returning the display string of the header.
  Its argument is the name of the source. This attribute is useful to
  add an additional information with the source name.
  It doesn't modify the name of the source.")

   (init
    :initarg :init
    :initform nil
    :custom function
    :documentation
    "  Function called with no parameters when helm is started.
  It is useful for collecting current state information which can be
  used to create the list of candidates later.
  Initialization of `candidates-in-buffer' is done here
  with `helm-init-candidates-in-buffer'.")

   (candidates
    :initarg :candidates
    :initform nil
    :custom (choice function list)
    :documentation
    "  Specifies how to retrieve candidates from the source.
  It can either be a variable name, a function called with no parameters
  or the actual list of candidates.

  The list must be a list whose members are strings, symbols
  or (DISPLAY . REAL) pairs.

  In case of (DISPLAY . REAL) pairs, the DISPLAY string is shown
  in the Helm buffer, but the REAL one is used as action
  argument when the candidate is selected. This allows a more
  readable presentation for candidates which would otherwise be,
  for example, too long or have a common part shared with other
  candidates which can be safely replaced with an abbreviated
  string for display purposes.

  Note that if the (DISPLAY . REAL) form is used then pattern
  matching is done on the displayed string, not on the real
  value.")

   (update
    :initarg :update
    :initform nil
    :custom function
    :documentation
    "  Function called with no parameters at before \"init\" function
  when `helm-force-update' is called.")

   (cleanup
    :initarg :cleanup
    :initform nil
    :custom function
    :documentation
    "  Function called with no parameters when *helm* buffer is
  closed. It is useful for killing unneeded candidates buffer.

  Note that the function is executed BEFORE performing action.")

   (delayed
    :initarg :delayed
    :initform nil
    :custom (choice null integer)
    :documentation
    "  Candidates from the source are shown only if the user stops
  typing and is idle for `helm-idle-delay' seconds.
  If a value is given to delayed attr, this value is used instead only
  if it is > to `helm-idle-delay'.")

   (keymap
    :initarg :keymap
    :initform nil
    :custom sexp
    :documentation
    "  Specific keymap for this source.
  It is useful to have a keymap per source when using more than
  one source.  Otherwise, a keymap can be set per command with
  `helm' argument KEYMAP.  NOTE: when a source have `helm-map' as
  keymap attr, the global value of `helm-map' will override the
  actual local one.")

   (action
    :initarg :action
    :initform 'identity
    :custom (alist :key-type string
                   :value-type function)
    :documentation
    "  An alist of (DISPLAY . FUNCTION) pairs, a variable name  or a function.
  FUNCTION is called with one parameter: the selected candidate.

  An action other than the default can be chosen from this list
  of actions for the currently selected candidate (by default
  with TAB). The DISPLAY string is shown in the completions
  buffer and the FUNCTION is invoked when an action is
  selected. The first action of the list is the default.

  You should use `helm-make-actions' to build this alist easily.")

   (persistent-action
    :initarg :persistent-action
    :initform nil
    :custom function
    :documentation
    "  Can be a either a Function called with one parameter (the
  selected candidate) or a cons cell where first element is this
  same function and second element a symbol (e.g never-split)
  that inform `helm-execute-persistent-action'to not split his
  window to execute this persistent action.")

   (persistent-help
    :initarg :persistent-help
    :initform nil
    :custom string
    :documentation
    "  A string to explain persistent-action of this source. It also
  accepts a function or a variable name.
  It will be displayed in `header-line'.
  Have no effect when `helm-echo-input-in-header-line' is non--nil.")

   (help-message
    :initarg :help-message
    :initform nil
    :custom (choice string function)
    :documentation
    "  Help message for this source.
  If not present, `helm-help-message' value will be used.")

   (multiline
    :initarg :multiline
    :initform nil
    :custom boolean
    :documentation
    "  Enable to selection multiline candidates.")

   (requires-pattern
    :initarg :requires-pattern
    :initform nil
    :custom integer
    :documentation
    "  If present matches from the source are shown only if the
  pattern is not empty. Optionally, it can have an integer
  parameter specifying the required length of input which is
  useful in case of sources with lots of candidates.")

   (candidate-transformer
    :initarg :candidate-transformer
    :initform nil
    :custom (choice function list)
    :documentation
    "  It's a function or a list of functions called with one argument
  when the completion list from the source is built. The argument
  is the list of candidates retrieved from the source. The
  function should return a transformed list of candidates which
  will be used for the actual completion.  If it is a list of
  functions, it calls each function sequentially.

  This can be used to transform or remove items from the list of
  candidates.

  Note that `candidates' is run already, so the given transformer
  function should also be able to handle candidates with (DISPLAY
  . REAL) format.")

   (filtered-candidate-transformer
    :initarg :filtered-candidate-transformer
    :initform nil
    :custom (choice function list)
    :documentation
    "  It has the same format as `candidate-transformer', except the
  function is called with two parameters: the candidate list and
  the source.

  This transformer is run on the candidate list which is already
  filtered by the current pattern. While `candidate-transformer'
  is run only once, it is run every time the input pattern is
  changed.

  It can be used to transform the candidate list dynamically, for
  example, based on the current pattern.

  In some cases it may also be more efficent to perform candidate
  transformation here, instead of with `candidate-transformer'
  even if this transformation is done every time the pattern is
  changed.  For example, if a candidate set is very large then
  `candidate-transformer' transforms every candidate while only
  some of them will actually be displayed due to the limit
  imposed by `helm-candidate-number-limit'.

  Note that `candidates' and `candidate-transformer' is run
  already, so the given transformer function should also be able
  to handle candidates with (DISPLAY . REAL) format.")

   (filter-one-by-one
    :initarg :filter-one-by-one
    :initform nil
    :custom (choice function list)
    :documentation
    "  A transformer function that treat candidates one by one.
  It is called with one arg the candidate.
  It is faster than `filtered-candidate-transformer' or
  `candidates-transformer', but should be used only in sources
  that recompute constantly their candidates, e.g `helm-source-find-files'.
  Filtering happen early and candidates are treated
  one by one instead of re-looping on the whole list.
  If used with `filtered-candidate-transformer' or `candidates-transformer'
  these functions should treat the candidates transformed by the
  `filter-one-by-one' function in consequence.")

   (display-to-real
    :initarg :display-to-real
    :initform nil
    :custom function
    :documentation
    "  Function called with one parameter; the selected candidate.

  The function transforms the selected candidate, and the result
  is passed to the action function.  The display-to-real
  attribute provides another way to pass to action other string than
  the one shown in Helm buffer.

  Traditionally, it is possible to make candidates,
  candidate-transformer or filtered-candidate-transformer
  function return a list with (DISPLAY . REAL) pairs. But if REAL
  can be generated from DISPLAY, display-to-real is more
  convenient and faster.

  NOTE: This is deprecated and you have better time using `filter-one-by-one'.")

   (real-to-display
    :initarg :real-to-display
    :initform nil
    :custom function
    :documentation
    "  Function called with one parameter; the selected candidate.
  The real value of candidates will be shown in display.
  See `display-to-real'.")

   (action-transformer
    :initarg :action-transformer
    :initform nil
    :custom (choice function list)
    :documentation
    "  It's a function or a list of functions called with two
  arguments when the action list from the source is
  assembled. The first argument is the list of actions, the
  second is the current selection.  If it is a list of functions,
  it calls each function sequentially.

  The function should return a transformed action list.

  This can be used to customize the list of actions based on the
  currently selected candidate.")

   (pattern-transformer
    :initarg :pattern-transformer
    :initform nil
    :custom (choice function list)
    :documentation
    "  It's a function or a list of functions called with one argument
  before computing matches. Its argument is `helm-pattern'.
  Functions should return transformed `helm-pattern'.

  It is useful to change interpretation of `helm-pattern'.")

   (candidate-number-limit
    :initarg :candidate-number-limit
    :initform nil
    :custom integer
    :documentation
    "  Override `helm-candidate-number-limit' only for this source.")

   (volatile
    :initarg :volatile
    :initform nil
    :custom boolean
    :documentation
    "  Indicates the source assembles the candidate list dynamically,
  so it shouldn't be cached within a single Helm
  invocation. It is only applicable to synchronous sources,
  because asynchronous sources are not cached.")

   (match
    :initarg :match
    :initform nil
    :custom (choice function list)
    :documentation
    "  List of functions called with one parameter: a candidate. The
  function should return non-nil if the candidate matches the
  current pattern (see variable `helm-pattern').

  When using `candidates-in-buffer' its default value is `identity' and
  don't have to be changed, use the `search' slot instead.

  This attribute allows the source to override the default
  pattern matching based on `string-match'. It can be used, for
  example, to implement a source for file names and do the
  pattern matching on the basename of files, since it's more
  likely one is typing part of the basename when searching for a
  file, instead of some string anywhere else in its path.

  If the list contains more than one function then the list of
  matching candidates from the source is constructed by appending
  the results after invoking the first function on all the
  potential candidates, then the next function, and so on. The
  matching candidates supplied by the first function appear first
  in the list of results and then results from the other
  functions, respectively.

  This attribute has no effect for asynchronous sources (see
  attribute `candidates'), since they perform pattern matching
  themselves.

  Note that FUZZY-MATCH slot will overhide value of this slot.")

   (fuzzy-match
    :initarg :fuzzy-match
    :initform nil
    :custom boolean
    :documentation
    "  Enable fuzzy matching in this source.
  This will overwrite settings in MATCH slot, and for
  sources built with child class `helm-source-in-buffer' the SEARCH slot.
  This is an easy way of enabling fuzzy matching, but you can use the MATCH
  or SEARCH slots yourself if you want something more elaborated, mixing
  different type of match (See `helm-source-buffers' class for example).")

   (nomark
    :initarg :nomark
    :initform nil
    :custom boolean
    :documentation
    "  Don't allow marking candidates when this attribute is present.")

   (nohighlight
    :initarg :nohighlight
    :initform nil
    :custom boolean
    :documentation
    "  Disable highlighting matches in this source.
  This will disable generic highlighting of matches,
  but some specialized highlighting can be done from elsewhere,
  i.e from `filtered-candidate-transformer' or `filter-one-by-one' slots.
  So use this to either disable completely highlighting in your source,
  or to disable highlighting and use a specialized highlighting matches
  function for this source.
  Remember that this function should run AFTER all filter functions if those
  filter functions are modifying face properties, though it is possible to
  avoid this by using new `add-face-text-property' in your filter functions.")

   (allow-dups
    :initarg :allow-dups
    :initform nil
    :custom boolean
    :documentation
    "  Allow helm collecting duplicates candidates.")

   (history
    :initarg :history
    :initform nil
    :custom symbol
    :documentation
    "  Allow passing history variable to helm from source.
  It should be a quoted symbol.")

   (coerce
    :initarg :coerce
    :initform nil
    :custom function
    :documentation
    "  It's a function called with one argument: the selected candidate.
  This function is intended for type convertion. In normal case,
  the selected candidate (string) is passed to action
  function. If coerce function is specified, it is called just
  before action function.

  Example: converting string to symbol
    (coerce . intern)")

   (mode-line
    :initarg :mode-line
    :initform nil
    :custom (choice string sexp)
    :documentation
    "  Source local `helm-mode-line-string' (included in
  `mode-line-format'). It accepts also variable/function name.")

   (header-line
    :initarg :header-line
    :initform nil
    :custom (choice string function)
    :documentation
    "  Source local `header-line-format'.
  Have no effect when `helm-echo-input-in-header-line' is non--nil.
  It accepts also variable/function name.")

   (resume
    :initarg :resume
    :initform nil
    :custom function
    :documentation
    "  Function called with no parameters at end of initialization
  when `helm-resume' is started.
  If this function try to do something against `helm-buffer', \(e.g updating,
  searching etc...\) probably you should run it in a timer to ensure
  `helm-buffer' is ready.")

   (follow
    :initarg :follow
    :initform nil
    :custom integer
    :documentation
    "  Enable `helm-follow-mode' for this source only.
  You must give it a value of 1 or -1, though giving a -1 value
  is surely not what you want, e.g: (follow . 1)

  See `helm-follow-mode' for more infos.")

   (follow-delay
    :initarg :follow-delay
    :initform nil
    :custom integer
    :documentation
    "  `helm-follow-mode' will execute persistent-action after this delay.
  Otherwise value of `helm-follow-input-idle-delay' is used if non--nil,
  If none of these are found fallback to `helm-input-idle-delay'.")

   (dont-plug
    :initarg :dont-plug
    :initform '(helm-compile-source--persistent-help)
    :custom list
    :documentation
    "  A list of compile functions plugin to ignore.")

   (matchplugin
    :initarg :matchplugin
    :initform t
    :custom boolean)

   (match-part
    :initarg :match-part
    :initform nil
    :custom function
    :documentation
    "  Allow matching only one part of candidate.
  If source contain match-part attribute, match is computed only
  on part of candidate returned by the call of function provided
  by this attribute. The function should have one arg, candidate,
  and return only a specific part of candidate.")

   (before-init-hook
    :initarg :before-init-hook
    :initform nil
    :custom symbol
    :documentation
    "  A local hook that run at beginning of initilization of this source.
  i.e Before the creation of `helm-buffer'.

  Should be a variable (defined with defvar).
  Can be also an anonymous function or a list of functions
  directly added to slot, this is not recommended though.")

   (after-init-hook
    :initarg :after-init-hook
    :initform nil
    :custom symbol
    :documentation
    "  A local hook that run at end of initilization of this source.
  i.e After the creation of `helm-buffer'.

  Should be a variable.
  Can be also an anonymous function or a list of functions
  directly added to slot, this is not recommended though."))

  "Main interface to define helm sources."
  :abstract t)

(defclass helm-source-sync (helm-source)
  ((candidates
    :initform '("ERROR: You must specify the `candidates' slot, either with a list or a function"))

   (dont-plug
    :initform '(helm-compile-source--multi-match
                helm-compile-source--persistent-help
                ;; Ensure this will not be plugged
                ;; if user have somewhere old helm-migemo.el.
                helm-compile-source--migemo))

   (migemo
    :initarg :migemo
    :initform nil
    :custom boolean
    :documentation
    "  Enable migemo.
  When multimatch is disabled, you can give the symbol 'nomultimatch as value
  to force not using generic migemo matching function.
  In this case you have to provide your own migemo matching funtion
  that kick in when `helm-migemo-mode' is enabled.
  Otherwise it will be available for this source once `helm-migemo-mode'
  is enabled when non-nil.")

   (match-strict
    :initarg :match-strict
    :initform nil
    :custom function
    :documentation
    "  When specifying a match function within a source and
  helm-multi-match is enabled, the result of all matching
  functions will be concatened, which in some cases is not what
  is wanted. When using `match-strict' only this or these
  functions will be used. You can specify those functions as a
  list of functions or a single symbol function.

  NOTE: This have the same effect as using :MATCHPLUGIN nil."))

  "Use this class to make helm sources using a list of candidates.
This list should be given as a normal list, a variable handling a list
or a function returning a list.
Matching is done basically with `string-match' against each candidate.")

(defclass helm-source-async (helm-source)
  ((candidates-process
    :initarg :candidates-process
    :initform nil
    :custom function
    :documentation
    "  This attribute is used to define a process as candidate.
  The value must be a process.

  NOTE:
  When building the source at runtime you can give directly a process
  as value, otherwise wrap the process call into a function.
  The process buffer should be nil, otherwise, if you use
  `helm-buffer' give to the process a sentinel.")

   (matchplugin :initform nil)
   (dont-plug :initform '(helm-compile-source--multi-match
                          helm-compile-source--persistent-help)))

  "Use this class to define a helm source calling an external process.
The :candidates slot is not allowed even if described because this class
inherit from `helm-source'.")

(defclass helm-source-in-buffer (helm-source)
  ((init
    :initform 'helm-default-init-source-in-buffer-function)

   (data
    :initarg :data
    :initform nil
    :custom (choice list string)
    :documentation
    "  A string or a list that will be used to feed the `helm-candidates-buffer'.
  This data will be passed in a function added to the init slot and
  the buffer will be build with `helm-init-candidates-in-buffer'.
  This is an easy and fast method to build a `candidates-in-buffer' source.")

   (dont-plug
    :initform '(helm-compile-source--candidates-in-buffer
                helm-compile-source--multi-match
                helm-compile-source--persistent-help
                helm-compile-source--migemo))

   (migemo
    :initarg :migemo
    :initform nil
    :custom boolean
    :documentation
    "  Enable migemo.
  When multimatch is disabled, you can give the symbol 'nomultimatch as value
  to force not using generic migemo matching function.
  In this case you have to provide your own migemo matching funtion
  that kick in when `helm-migemo-mode' is enabled.
  Otherwise it will be available for this source once `helm-migemo-mode'
  is enabled when non-nil.")

   (candidates
    :initform 'helm-candidates-in-buffer)

   (volatile
    :initform t)

   (match
    :initform '(identity))

   (get-line
    :initarg :get-line
    :initform 'buffer-substring-no-properties
    :custom function
    :documentation
    "  A function like `buffer-substring-no-properties' or `buffer-substring'.
  This function converts point of line-beginning and point of line-end,
  which represents a candidate computed by `helm-candidates-in-buffer'.
  By default, `helm-candidates-in-buffer' uses
  `buffer-substring-no-properties'.")

   (search
    :initarg :search
    :initform '(helm-candidates-in-buffer-search-default-fn)
    :custom (choice function list)
    :documentation
    "  List of functions like `re-search-forward' or `search-forward'.
  Buffer search function used by `helm-candidates-in-buffer'.
  By default, `helm-candidates-in-buffer' uses `re-search-forward'.
  The function should take one arg PATTERN.
  If your search function needs to handle negation like matchplugin,
  this function should returns in such case a cons cell of two integers defining
  the beg and end positions to match in the line previously matched by
  `re-search-forward' or similar, and move point to next line
  (See how the `helm-mm-3-search-base' and `helm-fuzzy-search' functions are working).

  NOTE: FUZZY-MATCH slot will overhide value of this slot.")

   (search-strict
    :initarg :search-strict
    :initform nil
    :custom function
    :documentation
    "  When specifying a search function within a source and
  helm-multi-match is enabled, the result of all searching
  functions will be concatened, which in some cases is not what
  is wanted. When using `search-strict' only this or these
  functions will be used. You can specify those functions as a
  list of functions or a single symbol function.

  NOTE: This have the same effect as using a nil value for
        :MATCHPLUGIN slot."))

  "Use this source to make helm sources storing candidates inside a buffer.
Contrarily to `helm-source-sync' candidates are matched using a function
like `re-search-forward', see below documentation of :search slot.")

(defclass helm-source-dummy (helm-source)
  ((candidates
    :initform '("dummy"))

   (filtered-candidate-transformer
    :initform 'helm-dummy-candidate)

   (matchplugin
    :initform nil)

   (accept-empty
    :initarg :accept-empty
    :initform t
    :custom boolean
    :documentation
    "  Allow exiting with an empty string.
  You should keep the default value.")

   (match
    :initform 'identity)

   (volatile
    :initform t)))

(defclass helm-source-in-file (helm-source-in-buffer)
  ((init :initform (lambda ()
                     (let ((file (helm-attr 'candidates-file)))
                       (with-current-buffer (helm-candidate-buffer 'global)
                         (insert-file-contents file)))))
   (candidates-file
    :initarg :candidates-file
    :initform nil
    :custom string
    :documentation "A filename."))

  "The contents of the file will be used as candidates in buffer.")


;;; Error functions
;;
;;
(defun helm-default-init-source-in-buffer-function ()
  (helm-init-candidates-in-buffer 'global
    '("ERROR: No buffer handling your data, use either the `init' slot or the `data' slot.")))


;;; Internal Builder functions.
;;
;;
(defun helm--create-source (object)
  "[INTERNAL] Build a helm source from OBJECT.
Where OBJECT is an instance of an eieio class."
  (cl-loop for s in (object-slots object)
           for slot-val = (slot-value object s)
           when slot-val
           collect (cons s (unless (eq t slot-val) slot-val))))

(defun helm-make-source (name class &rest args)
  "Build a `helm' source named NAME with ARGS for CLASS.
Argument NAME is a string which define the source name, so no need to use
the keyword :name in your source, NAME will be used instead.
Argument CLASS is an eieio class object.
Arguments ARGS are keyword value pairs as defined in CLASS."
  (declare (indent 2))
  (let ((source (apply #'make-instance class name args)))
    (set-slot-value source 'name name)
    (helm--setup-source source)
    (helm-setup-user-source source)
    (helm--create-source source)))

(defun helm-make-type (class &rest args)
  (let ((source (apply #'make-instance class args)))
    (set-slot-value source 'name nil)
    (helm--setup-source source)
    (helm--create-source source)))

(defvar helm-mm-default-search-functions)
(defvar helm-mm-default-match-functions)

(defun helm-source-mm-get-search-or-match-fns (source method)
  (let ((defmatch         (helm-aif (slot-value source 'match)
                              (helm-mklist it)))
        (defmatch-strict  (helm-aif (and (eq method 'match)
                                         (slot-value source 'match-strict))
                              (helm-mklist it)))
        (defsearch        (helm-aif (and (eq method 'search)
                                         (slot-value source 'search))
                              (helm-mklist it)))
        (defsearch-strict (helm-aif (and (eq method 'search-strict)
                                         (slot-value source 'search-strict))
                              (helm-mklist it)))
        (migemo           (slot-value source 'migemo)))
    (cl-case method
      (match (cond (defmatch-strict)
                   (migemo
                    (append helm-mm-default-match-functions
                            defmatch '(helm-mm-3-migemo-match)))
                   (defmatch
                    (append helm-mm-default-match-functions defmatch))
                   (t helm-mm-default-match-functions)))
      (search (cond (defsearch-strict)
                    (migemo
                     (append helm-mm-default-search-functions
                             defsearch '(helm-mm-3-migemo-search)))
                    (defsearch
                     (append helm-mm-default-search-functions defsearch))
                    (t helm-mm-default-search-functions))))))


;;; Modifiers
;;
(cl-defun helm-source-add-action-to-source-if (name fn source predicate
                                                    &optional (index 4))
  "Same as `helm-add-action-to-source-if' but for SOURCE defined as eieio object.
You can use this inside a `helm--setup-source' method for a SOURCE defined as
an eieio class."
  (let* ((actions     (slot-value source 'action))
         (action-transformers (slot-value source 'action-transformer))
         (new-action  (list (cons name fn)))
         (transformer `(lambda (actions candidate)
                         (cond ((funcall (quote ,predicate) candidate)
                                (helm-append-at-nth
                                 actions (quote ,new-action) ,index))
                               (t actions)))))
    (if (functionp actions)
        (set-slot-value source 'action (list (cons "Default action" actions)))
        (set-slot-value source 'action (helm-interpret-value actions source)))
    (when (or (symbolp action-transformers) (functionp action-transformers))
      (setq action-transformers (list action-transformers)))
    (set-slot-value
     source
     'action-transformer
     (delq nil (append (list transformer) action-transformers)))))


;;; Methods to build sources.
;;
;;
(defun helm-source--persistent-help-string (string source)
  (substitute-command-keys
   (concat "\\<helm-map>\\[helm-execute-persistent-action]: "
           (or (format "%s (keeping session)" string)
               (slot-value source 'header-line)))))

(defun helm-source--header-line (source)
  (substitute-command-keys
   (concat "\\<helm-map>\\[helm-execute-persistent-action]: "
           (helm-aif (or (slot-value source 'persistent-action)
                         (slot-value source 'action))
               (cond ((and (symbolp it)
                           (functionp it)
                           (eq it 'identity))
                      "Do Nothing")
                     ((and (symbolp it)
                           (boundp it)
                           (listp (symbol-value it))
                           (stringp (caar (symbol-value it))))
                      (caar (symbol-value it)))
                     ((or (symbolp it) (functionp it))
                      (helm-symbol-name it))
                     ((listp it)
                      (let ((action (car it)))
                        ;; It comes from :action ("foo" . function).
                        (if (stringp (car action))
                            (car action)
                            ;; It comes from :persistent-action
                            ;; (function . 'nosplit) Fix Issue #788.
                            (if (or (symbolp action)
                                    (functionp action))
                                (helm-symbol-name action)))))
                     (t ""))
             "")
           " (keeping session)")))

(defmethod helm--setup-source :primary ((_source helm-source)))

(defmethod helm--setup-source :before ((source helm-source))
  (helm-aif (slot-value source 'keymap)
      (and (symbolp it) (set-slot-value source 'keymap (symbol-value it))))
  (helm-aif (slot-value source 'persistent-help)
      (set-slot-value source 'header-line
                      (helm-source--persistent-help-string it source))
    (set-slot-value source 'header-line (helm-source--header-line source)))
  (helm-aif (slot-value source 'candidate-number-limit)
      (and (symbolp it) (set-slot-value
                         source 'candidate-number-limit (symbol-value it))))
  (when (and (slot-value source 'fuzzy-match) helm-fuzzy-sort-fn)
    (set-slot-value source 'filtered-candidate-transformer
                    (helm-aif (slot-value source 'filtered-candidate-transformer)
                        (append (helm-mklist it)
                                (list helm-fuzzy-sort-fn))
                      (list helm-fuzzy-sort-fn))))
  (unless (slot-value source 'nohighlight)
    (set-slot-value source 'filtered-candidate-transformer
                    (helm-aif (slot-value source 'filtered-candidate-transformer)
                        (append (helm-mklist it)
                                (list #'helm-fuzzy-highlight-matches))
                      (list #'helm-fuzzy-highlight-matches)))))

(defmethod helm-setup-user-source ((_source helm-source)))

(defmethod helm--setup-source ((source helm-source-sync))
  (when (slot-value source 'fuzzy-match)
    (helm-aif (slot-value source 'match)
        (set-slot-value source 'match (append (helm-mklist it)
                                              (list helm-fuzzy-match-fn)))
      (set-slot-value source 'match helm-fuzzy-match-fn)))
  (when (slot-value source 'matchplugin)
    (set-slot-value source 'match
                    (helm-source-mm-get-search-or-match-fns source 'match)))
  (helm-aif (and (null (slot-value source 'matchplugin))
                 (slot-value source 'migemo))
      (unless (eq it 'nomultimatch) ; Use own migemo fn.
        (set-slot-value source 'match
                        (append (helm-mklist (slot-value source 'match))
                                '(helm-mm-3-migemo-match))))))

(defmethod helm--setup-source ((source helm-source-in-buffer))
  (let ((cur-init (slot-value source 'init)))
    (helm-aif (slot-value source 'data)
        (set-slot-value
         source
         'init (delq
                nil
                (list
                 (and (null (eq 'helm-default-init-source-in-buffer-function
                                cur-init))
                      cur-init)
                 (lambda ()
                   (helm-init-candidates-in-buffer
                       'global
                     (if (functionp it) (funcall it) it))))))))
  (when (slot-value source 'fuzzy-match)
    (helm-aif (slot-value source 'search)
        (set-slot-value source 'search (append (helm-mklist it)
                                               (list helm-fuzzy-search-fn)))
      (set-slot-value source 'search (list helm-fuzzy-search-fn))))
  (when (slot-value source 'matchplugin)
    (set-slot-value
     source 'search (helm-source-mm-get-search-or-match-fns source 'search)))
  (helm-aif (and (null (slot-value source 'matchplugin))
                 (slot-value source 'migemo))
      (unless (eq it 'nomultimatch)
        (set-slot-value source 'search
                        (append (helm-mklist (slot-value source 'search))
                                '(helm-mm-3-migemo-search)))))
  (let ((mtc (slot-value source 'match)))
    (cl-assert (or (equal '(identity) mtc)
                   (eq 'identity mtc))
               nil "Invalid slot value for `match'")
    (cl-assert (eq (slot-value source 'volatile) t)
               nil "Invalid slot value for `volatile'")))

(defmethod helm--setup-source ((source helm-source-async))
  (cl-assert (null (slot-value source 'candidates))
             nil "Incorrect use of `candidates' use `candidates-process' instead")
  (cl-assert (null (slot-value source 'matchplugin))
             nil "`matchplugin' not allowed in async sources."))

(defmethod helm--setup-source ((source helm-source-dummy))
  (let ((mtc (slot-value source 'match)))
    (cl-assert (or (equal '(identity) mtc)
                   (eq 'identity mtc))
               nil "Invalid slot value for `match'")
    (cl-assert (eq (slot-value source 'volatile) t)
               nil "Invalid slot value for `volatile'")
    (cl-assert (equal (slot-value source 'candidates) '("dummy"))
               nil "Invalid slot value for `candidates'")
    (cl-assert (eq (slot-value source 'accept-empty) t)
               nil "Invalid slot value for `accept-empty'")))


;;; User functions
;;
;;  Sources
(defmacro helm-build-sync-source (name &rest args)
  "Build a synchronous helm source with name NAME.
Args ARGS are keywords provided by `helm-source-sync'."
  (declare (indent 1))
  `(helm-make-source ,name 'helm-source-sync ,@args))

(defmacro helm-build-async-source (name &rest args)
  "Build a asynchronous helm source with name NAME.
Args ARGS are keywords provided by `helm-source-async'."
  (declare (indent 1))
  `(helm-make-source ,name 'helm-source-async ,@args))

(defmacro helm-build-in-buffer-source (name &rest args)
  "Build a helm source with name NAME using `candidates-in-buffer' method.
Args ARGS are keywords provided by `helm-source-in-buffer'."
  (declare (indent 1))
  `(helm-make-source ,name 'helm-source-in-buffer ,@args))

(defmacro helm-build-dummy-source (name &rest args)
  "Build a helm source with name NAME using `dummy' method.
Args ARGS are keywords provided by `helm-source-dummy'."
  (declare (indent 1))
  `(helm-make-source ,name 'helm-source-dummy ,@args))

(defmacro helm-build-in-file-source (name file &rest args)
  "Build a helm source with NAME name using `candidates-in-files' method.
Arg FILE is a filename, the contents of this file will be
used as candidates in buffer.
Args ARGS are keywords provided by `helm-source-in-file'."
  (declare (indent 1))
  `(helm-make-source ,name 'helm-source-in-file
     :candidates-file ,file ,@args))


(provide 'helm-source)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-source ends here
