;;; lsp-mode.el --- LSP mode                              -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Vibhav Pant, Ivan Yonchovski

;; Author: Vibhav Pant, Fangrui Song, Ivan Yonchovski
;; Keywords: languages
;; Package-Requires: ((emacs "25.1") (dash "2.14.1") (dash-functional "2.14.1") (f "0.20.0") (ht "2.0") (spinner "1.7.3") (markdown-mode "2.3"))
;; Version: 6.0

;; URL: https://github.com/emacs-lsp/lsp-mode
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(unless (version< emacs-version "26")
  (require 'project)
  (require 'flymake))

(require 'cl-lib)
(require 'compile)
(require 'dash)
(require 'dash-functional)
(require 'em-glob)
(require 'f)
(require 'filenotify)
(require 'files)
(require 'ht)
(require 'imenu)
(require 'inline)
(require 'json)
(require 'network-stream)
(require 'pcase)
(require 'seq)
(require 'spinner)
(require 'subr-x)
(require 'url-parse)
(require 'url-util)
(require 'widget)
(require 'xref)
(require 'tree-widget)
(require 'markdown-mode)
(require 'ewoc)

(declare-function company-mode "company")
(declare-function flycheck-mode "flycheck")
(declare-function lsp-ui-flycheck-enable "lsp-ui")
(declare-function evil-set-command-property "evil")
(declare-function projectile-project-root "projectile")

(defconst lsp--message-type-face
  `((1 . ,compilation-error-face)
    (2 . ,compilation-warning-face)
    (3 . ,compilation-message-face)
    (4 . ,compilation-info-face)))

(defconst lsp--errors
  '((-32700 "Parse Error")
    (-32600 "Invalid Request")
    (-32601 "Method not Found")
    (-32602 "Invalid Parameters")
    (-32603 "Internal Error")
    (-32099 "Server Start Error")
    (-32000 "Server End Error")
    (-32002 "Server Not Initialized")
    (-32001 "Unknown Error Code")
    (-32800 "Request Cancelled"))
  "Alist of error codes to user friendly strings.")

(defconst lsp--completion-item-kind
  [nil
   "Text"
   "Method"
   "Function"
   "Constructor"
   "Field"
   "Variable"
   "Class"
   "Interface"
   "Module"
   "Property"
   "Unit"
   "Value"
   "Enum"
   "Keyword"
   "Snippet"
   "Color"
   "File"
   "Reference"
   "Folder"
   "EnumMember"
   "Constant"
   "Struct"
   "Event"
   "Operator"
   "TypeParameter"])

(defcustom lsp-print-io nil
  "If non-nil, print all messages to and from the language server to *lsp-log*."
  :group 'lsp-mode
  :type 'boolean)

(defcustom lsp-print-performance nil
  "If non-nil, print performance info in the logs."
  :group 'lsp-mode
  :type 'boolean)

(defcustom lsp-use-native-json t
  "If non-nil, use native json parsing if available."
  :group 'lsp-mode
  :type 'boolean)

(defcustom lsp-json-use-lists nil
  "If non-nil, use lists instead of vectors when doing json deserialization."
  :group 'lsp-mode
  :type 'boolean)

(defcustom lsp-log-max message-log-max
  "Maximum number of lines to keep in the log buffer.
If nil, disable message logging.  If t, log messages but don’t truncate
the buffer when it becomes large."
  :group 'lsp-mode
  :type '(choice (const :tag "Disable" nil)
                 (integer :tag "lines")
                 (const :tag "Unlimited" t)))

(defcustom lsp-io-messages-max t
  "Maximum number of messages that can be locked in a `lsp-io' buffer."
  :group 'lsp-mode
  :type '(choice (const :tag "Unlimited" t)
                 (integer :tag "Messages")))

(defcustom lsp-report-if-no-buffer t
  "If non nil the errors will be reported even when the file is not open."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-keep-workspace-alive t
  "If non nil keep workspace alive when the last workspace buffer is closed."
  :group 'lsp-mode
  :type 'boolean)

(defcustom lsp-enable-snippet t
  "Enable/disable snippet completion support."
  :group 'lsp-mode
  :type 'boolean)

(defcustom lsp-enable-folding t
  "Enable/disable code folding support."
  :group 'lsp-mode
  :type 'boolean)

(defcustom lsp-folding-range-limit nil
  "The maximum number of folding ranges to receive from the language server."
  :group 'lsp-mode
  :type '(choice (const :tag "No limit." nil)
                 (integer :tag "Number of lines.")))

(defcustom lsp-folding-line-folding-only nil
  "If non-nil, only fold complete lines."
  :group 'lsp-mode
  :type 'boolean)

(defcustom lsp-auto-require-clients t
  "Auto require lsp-clients."
  :group 'lsp-mode
  :type 'boolean)

(defvar-local lsp--cur-workspace nil)

(defvar-local lsp--cur-version nil)

(defvar lsp--uri-file-prefix (pcase system-type
                               (`windows-nt "file:///")
                               (_ "file://"))
  "Prefix for a file-uri.")

(defvar-local lsp-buffer-uri nil
  "If set, return it instead of calculating it using `buffer-file-name'.")

(define-error 'lsp-error "Unknown lsp-mode error")
(define-error 'lsp-empty-response-error
  "Empty response from the language server" 'lsp-error)
(define-error 'lsp-timed-out-error
  "Timed out while waiting for a response from the language server" 'lsp-error)
(define-error 'lsp-capability-not-supported
  "Capability not supported by the language server" 'lsp-error)
(define-error 'lsp-file-scheme-not-supported
  "Unsupported file scheme" 'lsp-error)

(defcustom lsp-auto-guess-root nil
  "Automatically guess the project root using projectile/project."
  :group 'lsp-mode
  :type 'boolean)

(defcustom lsp-restart 'interactive
  "Defines how server exited event must be handled."
  :group 'lsp-mode
  :type '(choice (const interactive)
                 (const auto-restart)
                 (const ignore)))

(defcustom lsp-session-file (expand-file-name (locate-user-emacs-file ".lsp-session-v1"))
  "Automatically guess the project root using projectile/project."
  :group 'lsp-mode
  :type 'file)

(defcustom lsp-auto-configure t
  "Auto configure `lsp-mode'.
When set to t `lsp-mode' will auto-configure `lsp-ui' and `company-lsp'."
  :group 'lsp-mode
  :type 'boolean)

(defvar lsp-clients (make-hash-table :test 'eql)
  "Hash table server-id -> client.
It contains all of the clients that are currently registered.")

(defvar lsp-enabled-clients nil
  "List of clients allowed to be used for projects.
When nil, all registered clients are considered candidates.")

(defvar lsp-last-id 0
  "Last request id.")

(defcustom lsp-before-initialize-hook nil
  "List of functions to be called before a Language Server has been initialized for a new workspace."
  :type 'hook
  :group 'lsp-mode)

(defcustom lsp-after-initialize-hook nil
  "List of functions to be called after a Language Server has been initialized for a new workspace."
  :type 'hook
  :group 'lsp-mode)

(defcustom lsp-before-open-hook nil
  "List of functions to be called before a new file with LSP support is opened."
  :type 'hook
  :group 'lsp-mode)

(defcustom lsp-after-open-hook nil
  "List of functions to be called after a new file with LSP support is opened."
  :type 'hook
  :group 'lsp-mode)

(defcustom lsp-enable-file-watchers t
  "If non-nil lsp-mode will watch the files in the workspace if
the server has requested that."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-file-watch-ignored '(".idea"
                                    ".ensime_cache"
                                    ".eunit"
                                    "node_modules"
                                    ".git"
                                    ".hg"
                                    ".fslckout"
                                    "_FOSSIL_"
                                    ".bzr"
                                    "_darcs"
                                    ".tox"
                                    ".svn"
                                    ".stack-work"
                                    ".bloop"
                                    ".metals"
                                    "target")
  "List of directories which won't be monitored when creating file watches."
  :group 'lsp-mode
  :type '(repeat string))

(defcustom lsp-after-uninitialized-hook nil
  "List of functions to be called after a Language Server has been uninitialized."
  :type 'hook
  :group 'lsp-mode)

(defvar lsp--sync-methods
  '((0 . none)
    (1 . full)
    (2 . incremental)))

(defcustom lsp-debounce-full-sync-notifications t
  "If non-nil debounce full sync events.
This flag affects only server which do not support incremental update."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-debounce-full-sync-notifications-interval 1.0
  "Time to wait before sending full sync synchronization after buffer modication."
  :type 'float
  :group 'lsp-mode)

(defvar lsp--delayed-requests nil)
(defvar lsp--delay-timer nil)

(defvar-local lsp--server-sync-method nil
  "Sync method recommended by the server.")

(defgroup lsp-mode nil
  "Language Server Protocol client."
  :group 'tools
  :tag "Language Server")

(defgroup lsp-faces nil
  "Faces."
  :group 'lsp-mode
  :tag "Faces")

(defcustom lsp-document-sync-method nil
  "How to sync the document with the language server."
  :type '(choice (const :tag "Documents should not be synced at all." 'none)
                 (const :tag "Documents are synced by always sending the full content of the document." 'full)
                 (const :tag "Documents are synced by always sending incremental changes to the document." 'incremental)
                 (const :tag "Use the method recommended by the language server." nil))
  :group 'lsp-mode)

(defcustom lsp-auto-execute-action t
  "Auto-execute single action."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-enable-links t
  "If non-nil, all references to links in a file will be made clickable, if supported by the language server."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-links-check-internal 0.1
  "The interval for updating document links."
  :group 'lsp-mode
  :type 'float)

(defcustom lsp-eldoc-enable-hover t
  "If non-nil, eldoc will display hover info when it is present."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-eldoc-enable-signature-help t
  "If non-nil, eldoc will display signature help when it is present."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-eldoc-prefer-signature-help t
  "If non-nil, eldoc will display signature help when both hover and signature help are present."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-eldoc-render-all nil
  "Define whether all of the returned by document/onHover will be displayed.
If `lsp-eldoc-render-all' is set to nil `eldoc' will show only
the symbol information."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-signature-render-all t
  "Define whether all of the returned by textDocument/signatureHelp will be displayed.
If `lsp-signature-render-all' is set to nil `eldoc' will show only
the active signature."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-enable-completion-at-point t
  "Enable `completion-at-point' integration."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-enable-symbol-highlighting t
  "Highlight references of the symbol at point."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-enable-xref t
  "Enable xref integration."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-enable-indentation t
  "Indent regions using the file formatting functionality provided by the language server."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-enable-on-type-formatting t
  "Enable `textDocument/onTypeFormatting' integration."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-before-save-edits t
  "If non-nil, `lsp-mode' will apply edits suggested by the language server before saving a document."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-after-diagnostics-hook nil
  "Hooks to run after diagnostics are received."
  :type 'hook
  :group 'lsp-mode)

(defconst lsp--sync-type
  `((0 . "None")
    (1 . "Full Document")
    (2 . "Incremental Changes")))

(defcustom lsp-workspace-folders-changed-hook nil
  "Hooks to run after the folders has changed.
The hook will receive two parameters list of added and removed folders."
  :type 'hook
  :group 'lsp-mode)

(defcustom lsp-on-hover-hook nil
  "The hooks that run after on hover and signature information has been loaded.
The hook is called with two params: the signature information and hover data."
  :type 'hook
  :group 'lsp-mode)

(defcustom lsp-eldoc-hook '(lsp-hover)
  "Hooks to run for eldoc."
  :type 'hook
  :group 'lsp-mode)

(defgroup lsp-imenu nil
  "Imenu."
  :group 'lsp-mode
  :tag "Imenu")

(defcustom lsp-imenu-show-container-name t
  "Display the symbol's container name in an imenu entry."
  :type 'boolean
  :group 'lsp-imenu)

(defcustom lsp-imenu-container-name-separator "/"
  "Separator string to use to separate the container name from the symbol while displaying imenu entries."
  :type 'string
  :group 'lsp-imenu)

(defcustom lsp-imenu-sort-methods '(kind name)
  "How to sort the imenu items.

The value is a list of `kind' `name' or `position'. Priorities
are determined by the index of the element."
  :type '(repeat (choice (const name)
                         (const position)
                         (const kind))))

;; vibhavp: Should we use a lower value (5)?
(defcustom lsp-response-timeout 10
  "Number of seconds to wait for a response from the language server before timing out."
  :type 'number
  :group 'lsp-mode)

(defconst lsp--imenu-compare-function-alist
  (list (cons 'name #'lsp--imenu-compare-name)
        (cons 'kind #'lsp--imenu-compare-kind)
        (cons 'position #'lsp--imenu-compare-position))
  "An alist of (METHOD . FUNCTION).
METHOD is one of the symbols accepted by
`lsp-imenu-sort-methods'.

FUNCTION takes two hash tables representing DocumentSymbol. It
returns a negative number, 0, or a positive number indicating
whether the first parameter is less than, equal to, or greater
than the second parameter.")

(defcustom lsp-prefer-flymake t
  "Auto-configure to prefer `flymake' over `lsp-ui' if both are present.
If set to `:none' neither of two will be enabled."
  :type '(choice (const :tag "Prefer flymake" t)
                 (const :tag "Prefer lsp-ui" nil)
                 (const :tag "Use neither flymake nor lsp-ui" :none))
  :group 'lsp-mode)

(defvar-local lsp--flymake-report-fn nil)

(defvar lsp-language-id-configuration '((".*.vue" . "vue")
                                        (".*.tsx" . "typescriptreact")
                                        (java-mode . "java")
                                        (python-mode . "python")
                                        (lsp--render-markdown . "markdown")
                                        (rust-mode . "rust")
                                        (rustic-mode . "rust")
                                        (kotlin-mode . "kotlin")
                                        (css-mode . "css")
                                        (less-mode . "less")
                                        (less-css-mode . "less")
                                        (sass-mode . "sass")
                                        (scss-mode . "scss")
                                        (xml-mode . "xml")
                                        (c-mode . "c")
                                        (c++-mode . "cpp")
                                        (objc-mode . "objective-c")
                                        (web-mode . "html")
                                        (html-mode . "html")
                                        (sgml-mode . "html")
                                        (mhtml-mode . "html")
                                        (go-mode . "go")
                                        (haskell-mode . "haskell")
                                        (php-mode . "php")
                                        (json-mode . "json")
                                        (rjsx-mode . "javascript")
                                        (js2-mode . "javascript")
                                        (typescript-mode . "typescript")
                                        (reason-mode . "reason")
                                        (caml-mode . "ocaml")
                                        (tuareg-mode . "ocaml")
                                        (swift-mode . "swift")
                                        (elixir-mode . "elixir")
                                        (conf-javaprop-mode . "spring-boot-properties")
                                        (yaml-mode . "spring-boot-properties-yaml")
                                        (ruby-mode . "ruby")
                                        (enh-ruby-mode . "ruby")
                                        (f90-mode . "fortran"))
  "Language id configuration.")

(defvar lsp-method-requirements
  '(("textDocument/onTypeFormatting" :capability "documentOnTypeFormattingProvider")
    ("workspace/executeCommand"
     :capability "executeCommandProvider"
     :registered-capability "workspace/executeCommand")
    ("textDocument/hover" :capability "hoverProvider")
    ("textDocument/documentSymbol" :capability "documentSymbolProvider")
    ("textDocument/documentHighlight" :capability "documentHighlightProvider")
    ("textDocument/definition" :capability "definitionProvider")
    ("workspace/symbol" :capability "workspaceSymbolProvider")
    ("textDocument/prepareRename"
     :check-command (lambda (workspace)
                      (with-lsp-workspace workspace
                        (let ((table (lsp--capability "renameProvider")))
                          (and (hash-table-p table)
                               (gethash "prepareProvider" table)))))))

  "Contain method to requirements mapping.
It is used by send request functions to determine which server
must be used for handling a particular message.")

(defconst lsp--file-change-type
  `((created . 1)
    (changed . 2)
    (deleted . 3)))

(defface lsp-face-highlight-textual
  '((t :inherit highlight))
  "Face used for textual occurances of symbols."
  :group 'lsp-faces)

(defface lsp-face-highlight-read
  '((t :inherit highlight :underline t))
  "Face used for highlighting symbols being read."
  :group 'lsp-faces)

(defface lsp-face-highlight-write
  '((t :inherit highlight :italic t))
  "Face used for highlighting symbols being written to."
  :group 'lsp-faces)

(defcustom lsp-lens-check-interval 0.1
  "The interval for checking for changes in the buffer state."
  :group 'lsp-mode
  :type 'boolean)

(defcustom lsp-lens-debounce-interval 0.7
  "Debounce interval for loading lenses."
  :group 'lsp-mode
  :type 'number)

(defcustom lsp-symbol-highlighting-skip-current nil
  "If non-nil skip current symbol when setting symbol highlights."
  :group 'lsp-mode
  :type 'boolean)

(defcustom lsp-document-highlight-delay 0.2
  "Seconds of idle time to wait before showing symbol highlight."
  :type 'number
  :group 'lsp-mode)

(defvar lsp-custom-markup-modes
  '((rust-mode "no_run" "rust,no_run" "rust,ignore" "rust,should_panic"))
  "Mode to uses with markdown code blocks.
They are added to `markdown-code-lang-modes'")

(defface lsp-lens-mouse-face
  '((t :height 0.8 :inherit link))
  "The face used for code lens overlays."
  :group 'lsp-mode)

(defface lsp-lens-face
  '((t :height 0.8 :inherit shadow))
  "The face used for code lens overlays."
  :group 'lsp-mode)

(defvar-local lsp--lens-overlays nil
  "Current lenses.")

(defvar-local lsp--lens-page nil
  "Pair of points which holds the last window location the lenses were loaded.")

(defvar-local lsp--lens-last-count nil
  "The number of lenses the last time they were rendered.")

(defvar lsp-lens-backends '(lsp-lens-backend)
  "Backends providing lenses.")

(defvar-local lsp--lens-refresh-timer nil
  "Refresh timer for the lenses.")

(defvar-local lsp--lens-idle-timer  nil
  "Lens idle timer.")

(defvar-local lsp--lens-data nil
  "Pair of points which holds the last window location the lenses were loaded.")

(defvar-local lsp--lens-backend-cache nil)

(defvar-local lsp--buffer-workspaces ()
  "List of the buffer workspaces.")

(defvar-local lsp--link-overlays nil
  "A list of overlays that display document links.")

(defvar-local lsp--links-idle-timer nil)

(defvar lsp--session nil
  "Contain the `lsp-session' for the current Emacs instance.")

(defvar lsp--tcp-port 10000)

(defvar-local lsp--document-symbols nil
  "The latest document symbols.")

(defvar-local lsp--document-symbols-request-async nil
  "If non-nil, request document symbols asynchronously.")

(defvar-local lsp--document-symbols-tick -1
  "The value of `buffer-chars-modified-tick' when document
  symbols were last retrieved.")

;; Buffer local variable for storing number of lines.
(defvar lsp--log-lines)

(cl-defgeneric lsp-execute-command (server command arguments)
  "Ask SERVER to execute COMMAND with ARGUMENTS.")

(defun lsp-elt (sequence n)
  "Return Nth element of SEQUENCE or nil if N is out of range."
  (if (listp sequence) (elt sequence n)
    (and (> (length sequence) n) (elt sequence n))))

;; define seq-first and seq-rest for older emacs
(defun seq-first (sequence)
  "Return the first element of SEQUENCE."
  (lsp-elt sequence 0))

(defun seq-rest (sequence)
  "Return a sequence of the elements of SEQUENCE except the first one."
  (seq-drop sequence 1))

(defun lsp--info (format &rest args)
  "Display lsp info message with FORMAT with ARGS."
  (message "%s :: %s" (propertize "LSP" 'face 'success) (apply #'format format args)))

(defun lsp--warn (format &rest args)
  "Display lsp warn message with FORMAT with ARGS."
  (message "%s :: %s" (propertize "LSP" 'face 'warning) (apply #'format format args)))

(defun lsp--error (format &rest args)
  "Display lsp error message with FORMAT with ARGS."
  (message "%s :: %s" (propertize "LSP" 'face 'error) (apply #'format format args)))

(defun lsp--eldoc-message (&optional msg)
  "Show MSG in eldoc."
  (run-with-idle-timer 0 nil (lambda () (eldoc-message msg))))

(defun lsp-log (format &rest args)
  "Log message to the ’*lsp-log*’ buffer.

FORMAT and ARGS i the same as for `message'."
  (when lsp-log-max
    (let ((log-buffer (get-buffer "*lsp-log*"))
          (inhibit-read-only t))
      (unless log-buffer
        (setq log-buffer (get-buffer-create "*lsp-log*"))
        (with-current-buffer log-buffer
          (view-mode 1)
          (set (make-local-variable 'lsp--log-lines) 0)))
      (with-current-buffer log-buffer
        (save-excursion
          (let* ((message (apply 'format format args))
                 ;; Count newlines in message.
                 (newlines (1+ (cl-loop with start = 0
                                        for count from 0
                                        while (string-match "\n" message start)
                                        do (setq start (match-end 0))
                                        finally return count))))
            (goto-char (point-max))

            ;; in case the buffer is not empty insert before last \n to preserve
            ;; the point position(in case it is in the end)
            (if (eq (point) (point-min))
                (progn
                  (insert "\n")
                  (backward-char))
              (backward-char)
              (insert "\n"))
            (insert message)

            (setq lsp--log-lines (+ lsp--log-lines newlines))

            (when (and (integerp lsp-log-max) (> lsp--log-lines lsp-log-max))
              (let ((to-delete (- lsp--log-lines lsp-log-max)))
                (goto-char (point-min))
                (forward-line to-delete)
                (delete-region (point-min) (point))
                (setq lsp--log-lines lsp-log-max)))))))))

(defalias 'lsp-message 'lsp-log)

(defalias 'lsp-ht 'ht)

;; `file-local-name' was added in Emacs 26.1.
(defalias 'lsp-file-local-name
  (if (fboundp 'file-local-name)
      'file-local-name
    (lambda (file)
      "Return the local name component of FILE."
      (or (file-remote-p file 'localname) file))))

(defun lsp--merge-results (results method)
  "Merge RESULTS by filtering the empty hash-tables and merging the lists.
METHOD is the executed method so the results could be merged
depending on it."
  (pcase (--map (if (vectorp it) (append it nil) it) (-filter 'identity results))
    (`() ())
    ;; only one result - simply return it
    (`(,fst) fst)
    ;; multiple results merge it based on stragegy
    (results
     (pcase method
       ("textDocument/hover" (let ((results (seq-filter
                                             (-compose #'not #'hash-table-empty-p)
                                             results)))
                               (if (not (cdr results))
                                   (car results)
                                 (let ((merged (make-hash-table :test 'equal)))
                                   (seq-each
                                    (lambda (it)
                                      (let ((to-add (gethash "contents" it)))
                                        (puthash "contents"
                                                 (append
                                                  (if (and (sequencep to-add)
                                                           (not (stringp to-add)))
                                                      to-add
                                                    (list to-add))
                                                  (gethash "contents" merged))
                                                 merged)))
                                    results)
                                   merged))))
       ("textDocument/completion"
        (ht
         ;; any incomplete
         ("isIncomplete" (seq-some
                          (-andfn #'ht? (-partial 'gethash "isIncomplete"))
                          results))
         ("items" (apply 'append (--map (append (if (ht? it)
                                                    (gethash "items" it)
                                                  it)
                                                nil)
                                        results)))))
       (_ (apply 'append (seq-map (lambda (it)
                                    (if (seqp it)
                                        it
                                      (list it)))
                                  results)))))))
(defun lsp--spinner-start ()
  "Start spinner indication."
  (condition-case _err (spinner-start 'progress-bar-filled) (error)))

(defun lsp--propertize (str type)
  "Propertize STR as per TYPE."
  (propertize str 'face (alist-get type lsp--message-type-face)))

(defun lsp-workspaces ()
  "Return the lsp workspaces associated with the current project."
  (if lsp--cur-workspace (list lsp--cur-workspace) lsp--buffer-workspaces))

(defun lsp--completing-read (prompt collection transform-fn &optional predicate
                                    require-match initial-input
                                    hist def inherit-input-method)
  "Wrap `completing-read' to provide tranformation function.

TRANSFORM-FN will be used to transform each of the items before displaying.

PROMPT COLLECTION PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF
INHERIT-INPUT-METHOD will be proxied to `completing-read' without changes."
  (let* ((result (--map (cons (funcall transform-fn it) it) collection))
         (completion (completing-read prompt (-map 'cl-first result)
                                      predicate require-match initial-input hist
                                      def inherit-input-method)))
    (cdr (assoc completion result))))

(cl-defstruct lsp--parser
  (headers '()) ;; alist of headers
  (body nil) ;; message body
  (reading-body nil) ;; If non-nil, reading body
  (body-length nil) ;; length of current message body
  (body-received 0) ;; amount of current message body currently stored in 'body'
  (leftovers nil) ;; Leftover data from previous chunk; to be processed
  (workspace nil))

;; A ‘lsp--client’ object describes the client-side behavior of a language
;; server.  It is used to start individual server processes, each of which is
;; represented by a ‘lsp--workspace’ object.  Client objects are normally
;; created using ‘lsp-define-stdio-client’ or ‘lsp-define-tcp-client’.  Each
;; workspace refers to exactly one client, but there can be multiple workspaces
;; for a single client.
(cl-defstruct lsp--client
  ;; ‘language-id’ is a function that receives a buffer as a single argument
  ;; and should return the language identifier for that buffer.  See
  ;; https://microsoft.github.io/language-server-protocol/specification#textdocumentitem
  ;; for a list of language identifiers.  Also consult the documentation for
  ;; the language server represented by this client to find out what language
  ;; identifiers it supports or expects.
  (language-id nil :read-only t)

  ;; ‘add-on?’ when set to t the server will be started no matter whether there
  ;; is another server hadling the same mode.
  (add-on? nil :read-only t)
  ;; ‘new-connection’ is a function that should start a language server process
  ;; and return a cons (COMMAND-PROCESS . COMMUNICATION-PROCESS).
  ;; COMMAND-PROCESS must be a process object representing the server process
  ;; just started.  COMMUNICATION-PROCESS must be a process (including pipe and
  ;; network processes) that ‘lsp-mode’ uses to communicate with the language
  ;; server using the language server protocol.  COMMAND-PROCESS and
  ;; COMMUNICATION-PROCESS may be the same process; in that case
  ;; ‘new-connection’ may also return that process as a single
  ;; object. ‘new-connection’ is called with two arguments, FILTER and
  ;; SENTINEL.  FILTER should be used as process filter for
  ;; COMMUNICATION-PROCESS, and SENTINEL should be used as process sentinel for
  ;; COMMAND-PROCESS.
  (new-connection nil :read-only t)

  ;; ‘ignore-regexps’ is a list of regexps.  When a data packet from the
  ;; language server matches any of these regexps, it will be ignored.  This is
  ;; intended for dealing with language servers that output non-protocol data.
  (ignore-regexps nil :read-only t)

  ;; ‘ignore-messages’ is a list of regexps.  When a message from the language
  ;; server matches any of these regexps, it will be ignored.  This is useful
  ;; for filtering out unwanted messages; such as servers that send nonstandard
  ;; message types, or extraneous log messages.
  (ignore-messages nil :read-only t)

  ;; ‘notification-handlers’ is a hash table mapping notification method names
  ;; (strings) to functions handling the respective notifications.  Upon
  ;; receiving a notification, ‘lsp-mode’ will call the associated handler
  ;; function passing two arguments, the ‘lsp--workspace’ object and the
  ;; deserialized notification parameters.
  (notification-handlers (make-hash-table :test 'equal) :read-only t)

  ;; ‘request-handlers’ is a hash table mapping request method names
  ;; (strings) to functions handling the respective notifications.  Upon
  ;; receiving a request, ‘lsp-mode’ will call the associated handler function
  ;; passing two arguments, the ‘lsp--workspace’ object and the deserialized
  ;; request parameters.
  (request-handlers (make-hash-table :test 'equal) :read-only t)

  ;; ‘response-handlers’ is a hash table mapping integral JSON-RPC request
  ;; identifiers for pending asynchronous requests to functions handling the
  ;; respective responses.  Upon receiving a response from the language server,
  ;; ‘lsp-mode’ will call the associated response handler function with a
  ;; single argument, the deserialized response parameters.
  (response-handlers (make-hash-table :test 'eql) :read-only t)

  ;; ‘prefix-function’ is called for getting the prefix for completion.
  ;; The function takes no parameter and returns a cons (start . end) representing
  ;; the start and end bounds of the prefix. If it's not set, the client uses a
  ;; default prefix function."
  (prefix-function nil :read-only t)

  ;; Contains mapping of scheme to the function that is going to be used to load
  ;; the file.
  (uri-handlers (make-hash-table :test #'equal) :read-only t)

  ;; ‘action-handlers’ is a hash table mapping action to a handler function. It
  ;; can be used in `lsp-execute-code-action' to determine whether the action
  ;; current client is interested in executing the action instead of sending it
  ;; to the server.
  (action-handlers (make-hash-table :test 'equal) :read-only t)

  ;; major modes supported by the client.
  (major-modes)
  ;; Function that will be called to decide if this language client
  ;; should manage a particular buffer. The function will be passed
  ;; the file name and major mode to inform the decision. Setting
  ;; `activation-fn' will override `major-modes' and `remote?', if
  ;; present.
  (activation-fn)
  ;; Break the tie when major-mode is supported by multiple clients.
  (priority 0)
  ;; Unique identifier for
  (server-id)
  ;; defines whether the client supports multi root workspaces.
  (multi-root)
  ;; Initialization options or a function that returns initialization options.
  (initialization-options)
  ;; Function which returns the folders that are considered to be not projects but library files.
  ;; The function accepts one parameter currently active workspace.
  ;; See: https://github.com/emacs-lsp/lsp-mode/issues/225.
  (library-folders-fn)
  ;; function which will be called when opening file in the workspace to perfom
  ;; client specific initialization. The function accepts one parameter
  ;; currently active workspace.
  (before-file-open-fn)
  ;; Function which will be called right after a workspace has been intialized.
  (initialized-fn)
  ;; ‘remote?’ indicate whether the client can be used for LSP server over TRAMP.
  (remote? nil)

  ;; ‘completion-in-comments?’ t if the client supports completion in comments.
  (completion-in-comments? nil))

;; from http://emacs.stackexchange.com/questions/8082/how-to-get-buffer-position-given-line-number-and-column-number
(defun lsp--line-character-to-point (line character)
  "Return the point for character CHARACTER on line LINE."
  (save-excursion
    (save-restriction
      (condition-case _err
          (progn
            (widen)
            (goto-char (point-min))
            (forward-line line)
            (forward-char character)
            (point))
        (error (point))))))

(define-inline lsp--position-to-point (params)
  "Convert Position object in PARAMS to a point."
  (inline-letevals (params)
    (inline-quote
     (lsp--line-character-to-point (gethash "line" ,params)
                                   (gethash "character" ,params)))))

(define-inline lsp--range-to-region (range)
  (inline-letevals (range)
    (inline-quote
     (cons (lsp--position-to-point (gethash "start" ,range))
           (lsp--position-to-point (gethash "end" ,range))))))

(pcase-defmacro lsp-range (region)
  "Build a `pcase' pattern that matches a LSP Range object.
Elements should be of the form (START . END), where START and END are bound
to the beginning and ending points in the range correspondingly."
  `(and (pred hash-table-p)
        (app (lambda (range) (lsp--position-to-point (gethash "start" range)))
             ,(car region))
        (app (lambda (range) (lsp--position-to-point (gethash "end" range)))
             ,(cdr region))))

(defun lsp-warn (message &rest args)
  "Display a warning message made from (`format-message' MESSAGE ARGS...).
This is equivalent to `display-warning', using `lsp-mode' as the type and
`:warning' as the level."
  (display-warning 'lsp-mode (apply #'format-message message args)))

(defun lsp--get-uri-handler (scheme)
  "Get uri handler for SCHEME in the current workspace."
  (--some (gethash scheme (lsp--client-uri-handlers (lsp--workspace-client it)))
          (or (lsp-workspaces) (lsp--session-workspaces (lsp-session)))))

(defun lsp--fix-path-casing (path)
  "On windows, downcases path because the windows file system is
case-insensitive.

On other systems, returns path without change."
  (if (eq system-type 'windows-nt) (downcase path) path))

(defun lsp--uri-to-path (uri)
  "Convert URI to a file path."
  (let* ((url (url-generic-parse-url (url-unhex-string uri)))
         (type (url-type url))
         (file (decode-coding-string (url-filename url) locale-coding-system))
         (file-name (if (and type (not (string= type "file")))
                        (if-let ((handler (lsp--get-uri-handler type)))
                            (funcall handler uri)
                          (signal 'lsp-file-scheme-not-supported (list uri)))
                      ;; `url-generic-parse-url' is buggy on windows:
                      ;; https://github.com/emacs-lsp/lsp-mode/pull/265
                      (or (and (eq system-type 'windows-nt)
                               (eq (elt file 0) ?\/)
                               (substring file 1))
                          file))))

    (lsp--fix-path-casing
     (concat (-some 'lsp--workspace-host-root (lsp-workspaces)) file-name))))

(defun lsp--buffer-uri ()
  "Return URI of the current buffer."
  (or lsp-buffer-uri
      (lsp--path-to-uri
       (or buffer-file-name (ignore-errors (buffer-file-name (buffer-base-buffer)))))))

(defun lsp-register-client-capabilities (&rest _args)
  "Implemented only to make `company-lsp' happy.
DELETE when `lsp-mode.el' is deleted.")

(defun lsp--path-to-uri (path)
  "Convert PATH to a uri."
  (concat lsp--uri-file-prefix
          (url-hexify-string (expand-file-name (or (file-remote-p path 'localname t) path))
                             url-path-allowed-chars)))

(defun lsp--string-match-any (regex-list str)
  "Given a list of REGEX-LIST and STR return the first matching regex if any."
  (--first (string-match it str) regex-list))

(cl-defstruct lsp-watch
  (descriptors (make-hash-table :test 'equal) :read-only t)
  (root-directory))

(defun lsp-watch-root-folder (dir callback &optional watch)
  "Create recursive file notificaton watch in DIR.
CALLBACK is the will be called when there are changes in any of
the monitored files. WATCHES is a hash table directory->file
notification handle which contains all of the watch that
already have been created."
  (lsp-log "Creating watch for %s" dir)
  (let ((watch (or watch (make-lsp-watch :root-directory dir))))
    (condition-case err
        (progn
          (puthash
           (file-truename dir)
           (file-notify-add-watch
            dir
            '(change)
            (lambda (event)
              (let ((file-name (cl-caddr event))
                    (event-type (cadr event)))
                (cond
                 ((and (file-directory-p file-name)
                       (equal 'created event-type))

                  (lsp-watch-root-folder file-name callback watch)

                  ;; process the files that are already present in
                  ;; the directory.
                  (->> (directory-files-recursively file-name ".*" t)
                       (seq-do (lambda (f)
                                 (unless (file-directory-p f)
                                   (funcall callback (list nil 'created f)))))))
                 ((and (not (file-directory-p file-name))
                       (memq event-type '(created deleted changed)))
                  (funcall callback event))))))
           (lsp-watch-descriptors watch))
          (seq-do
           (-rpartial #'lsp-watch-root-folder callback watch)
           (seq-filter (lambda (f)
                         (and (file-directory-p f)
                              (not (gethash (file-truename f) (lsp-watch-descriptors watch)))
                              (not (lsp--string-match-any lsp-file-watch-ignored f))
                              (not (-contains? '("." "..") (f-filename f)))))
                       (directory-files dir t))))
      (error (lsp-log "Failed to create a watch for %s: message" (error-message-string err)))
      (file-missing (lsp-log "Failed to create a watch for %s: message" (error-message-string err))))
    watch))

(defun lsp-kill-watch (watch)
  "Delete WATCH."
  (-> watch lsp-watch-descriptors hash-table-values (-each #'file-notify-rm-watch))
  (ht-clear! (lsp-watch-descriptors watch)))

(defmacro with-lsp-workspace (workspace &rest body)
  "Helper macro for invoking BODY in WORKSPACE context."
  (declare (debug (form body))
           (indent 1))
  `(let ((lsp--cur-workspace ,workspace)) ,@body))

(defmacro with-lsp-workspaces (workspaces &rest body)
  "Helper macro for invoking BODY against multiple WORKSPACES."
  (declare (debug (form body))
           (indent 1))
  `(let ((lsp--buffer-workspaces ,workspaces)) ,@body))

(defmacro lsp-foreach-workspace (&rest body)
  "Execute BODY for each of the current workspaces."
  (declare (debug (form body)))
  `(--map (with-lsp-workspace it ,@body) (lsp-workspaces)))

(defmacro when-lsp-workspace (workspace &rest body)
  "Helper macro for invoking BODY in WORKSPACE context if present."
  (declare (debug (form body))
           (indent 1))
  `(when-let (lsp--cur-workspace ,workspace) ,@body))

(defun lsp--window-show-message (_workspace params)
  "Send the server's messages to log.
PARAMS - the data sent from _WORKSPACE."
  (-let (((&hash "message" "type") params))
    (funcall (cl-case type
               (1 'lsp--error)
               (2 'lsp--warn)
               (t 'lsp--info))
             "%s"
             message)))

(defun lsp--window-log-message (workspace params)
  "Send the server's messages to log.
PARAMS - the data sent from WORKSPACE."
  (ignore
   (let* ((message (gethash "message" params))
          (client (lsp--workspace-client workspace)))
     (when (or (not client)
               (cl-notany (-rpartial #'string-match-p message)
                          (lsp--client-ignore-messages client)))
       (lsp-log "%s" (lsp--propertize message (gethash "type" params)))))))

(defun lsp--window-log-message-request (params)
  "Display a message request to the user and send the user's selection back to the server."
  (let* ((type (gethash "type" params))
         (message (lsp--propertize (gethash "message" params) type))
         (choices (seq-map (-partial 'gethash "title")
                           (gethash "actions" params))))
    (if choices
        (completing-read (concat message " ") (seq-into choices 'list) nil t)
      (lsp-log message))))

(defun lsp-diagnostics ()
  "Return the diagnostics from all workspaces."
  (let ((result (make-hash-table :test 'equal)))
    (-> (lsp-session)
        (lsp--session-workspaces)
        (--each (maphash
                 (lambda (file-name diagnostics)
                   (puthash file-name
                            (append (gethash file-name result) diagnostics)
                            result))
                 (lsp--workspace-diagnostics it))))
    result))

(cl-defstruct lsp-diagnostic
  (range nil :read-only t)
  ;; range has the form (:start (:line N :column N) :end (:line N :column N) )
  ;; where N are zero-indexed numbers
  (line nil :read-only t)
  (column nil :read-only t)
  (severity nil :read-only t) ;; 1 - error, 2 - warning, 3 - information, 4 - hint
  (code nil :read-only t) ;; the diagnostic's code
  (source nil :read-only t) ;;
  (message nil :read-only t) ;; diagnostic's message
  (original nil :read-only t))

(defun lsp--make-diag (diag)
  "Make a `lsp-diagnostic' from DIAG."
  (-let* (((&hash "message" "code" "source" "severity"
                  "range" (&hash "start" (&hash "line"      start-line
                                                "character" start-character)
                                 "end"   (&hash "line"      end-line
                                                "character" end-character))) diag))
    (make-lsp-diagnostic
     :range (list :start (list :line start-line :column start-character)
                  :end   (list :line end-line   :column end-character))
     :line start-line
     :column start-character
     :severity severity
     :code code
     :source source
     :message (if source (format "%s: %s" source message) message)
     :original diag)))

(defun lsp--on-diagnostics (workspace params)
  "Callback for textDocument/publishDiagnostics.
interface PublishDiagnosticsParams {
    uri: string;
    diagnostics: Diagnostic[];
}
PARAMS contains the diagnostics data.
WORKSPACE is the workspace that contains the diagnostics."
  (let* ((file (lsp--uri-to-path (gethash "uri" params)))
         (diagnostics (gethash "diagnostics" params))
         (buffer (find-buffer-visiting file))
         (workspace-diagnostics (lsp--workspace-diagnostics workspace)))

    (if (seq-empty-p diagnostics)
        (remhash file workspace-diagnostics)
      (when (or lsp-report-if-no-buffer buffer)
        (puthash file (seq-map #'lsp--make-diag diagnostics) workspace-diagnostics)))

    (when buffer
      (save-mark-and-excursion
        (with-current-buffer buffer
          (run-hooks 'lsp-after-diagnostics-hook))))))

(with-no-warnings
  (with-eval-after-load 'flymake
    (defun lsp--flymake-setup()
      "Setup flymake."
      (setq lsp--flymake-report-fn nil)
      (flymake-mode 1)
      (add-hook 'flymake-diagnostic-functions 'lsp--flymake-backend nil t)
      (add-hook 'lsp-after-diagnostics-hook 'lsp--flymake-after-diagnostics nil t))

    (defun lsp--flymake-after-diagnostics ()
      "Handler for `lsp-after-diagnostics-hook'"
      (cond
       ((and lsp--flymake-report-fn flymake-mode)
        (lsp--flymake-update-diagnostics))
       ((not flymake-mode)
        (setq lsp--flymake-report-fn nil))))

    (defun lsp--flymake-backend (report-fn &rest _args)
      "Flymake backend."
      (let ((first-run (null lsp--flymake-report-fn)))
        (setq lsp--flymake-report-fn report-fn)
        (when first-run
          (lsp--flymake-update-diagnostics))))

    (defun lsp--flymake-update-diagnostics ()
      "Report new diagnostics to flymake."
      (funcall lsp--flymake-report-fn
               (-some->> (lsp-diagnostics)
                         (gethash buffer-file-name)
                         (--map (-let* (((&hash "message" "severity" "range") (lsp-diagnostic-original it))
                                        ((start . end) (lsp--range-to-region range)))
                                  (when (= start end)
                                    (-let* (((&hash "line" "character") (gethash "start" range))
                                            (region (flymake-diag-region (current-buffer) (1+ line) character)))
                                      (setq start (car region) end (cdr region))))
                                  (flymake-make-diagnostic (current-buffer)
                                                           start
                                                           end
                                                           (cl-case severity
                                                             (1 :error)
                                                             (2 :warning)
                                                             (t :note))
                                                           message))))
               ;; This :region keyword forces flymake to delete old diagnostics in
               ;; case the buffer hasn't changed since the last call to the report
               ;; function. See https://github.com/joaotavora/eglot/issues/159
               :region (cons (point-min) (point-max))))))

(defun lsp--ht-get (tbl &rest keys)
  "Get nested KEYS in TBL."
  (let ((val tbl))
    (while (and keys val)
      (setq val (ht-get val (cl-first keys)))
      (setq keys (cl-rest keys)))
    val))



;; textDocument/foldingRange support

(cl-defstruct lsp--folding-range
  (beg)
  (end)
  (kind)
  (children)
  (orig-folding-range))

(defvar-local lsp--cached-folding-ranges nil)
(defvar-local lsp--cached-nested-folding-ranges nil)

(define-inline lsp--folding-range-width (range)
  (inline-letevals (range)
    (inline-quote (- (lsp--folding-range-end ,range)
                     (lsp--folding-range-beg ,range)))))

(defun lsp--get-folding-ranges ()
  "Get the folding ranges for the current buffer."
  (unless (lsp--capability "foldingRangeProvider")
    (signal 'lsp-capability-not-supported (list "foldingRangeProvider")))
  (-let [(tick . ranges) lsp--cached-folding-ranges]
    (if (eq tick (buffer-chars-modified-tick))
        ranges
      (setq ranges (lsp-request "textDocument/foldingRange"
                                `(:textDocument ,(lsp--text-document-identifier))))
      (setq lsp--cached-folding-ranges
            (cons (buffer-chars-modified-tick)
                  (-> (lambda (range)
                        (-let [(&hash "startLine" start-line
                                      "startCharacter" start-character
                                      "endLine" end-line
                                      "endCharacter" end-character
                                      "kind" kind)
                               range]
                          (make-lsp--folding-range
                           :beg (lsp--line-character-to-point
                                 start-line start-character)
                           :end (lsp--line-character-to-point
                                 end-line end-character)
                           :kind kind
                           :orig-folding-range range)))
                      (seq-map ranges)
                      (seq-into 'list)
                      (delete-dups))))
      (cdr lsp--cached-folding-ranges))))

(defun lsp--get-nested-folding-ranges ()
  "Get a list of nested folding ranges for the current buffer."
  (-let [(tick . _) lsp--cached-folding-ranges]
    (if (and (eq tick (buffer-chars-modified-tick))
             lsp--cached-nested-folding-ranges)
        lsp--cached-nested-folding-ranges
      (setq lsp--cached-nested-folding-ranges
            (lsp--folding-range-build-trees (lsp--get-folding-ranges))))))

(defun lsp--folding-range-insert-into-trees (trees range)
  (unless
      (cl-block top
        (dolist (tree-node (reverse trees))
          (when (lsp--range-inside-p range tree-node)
            (setf (lsp--folding-range-children tree-node)
                  (nconc (lsp--folding-range-children tree-node)
                         (list range)))
            (cl-return-from top t))))
    (nconc trees (list range))))

(defun lsp--folding-range-build-trees (ranges)
  (setq ranges (seq-sort #'lsp--range-before-p ranges))
  (let ((trees (list (seq-first ranges))))
    (dolist (range (seq-rest ranges))
      (lsp--folding-range-insert-into-trees trees range))
    trees))

(define-inline lsp--range-inside-p (r1 r2)
  "Return non-nil if folding range R1 lies inside R2"
  (inline-letevals (r1 r2)
    (inline-quote
     (and (>= (lsp--folding-range-beg ,r1) (lsp--folding-range-beg ,r2))
          (<= (lsp--folding-range-end ,r1) (lsp--folding-range-end ,r2))))))

(define-inline lsp--range-before-p (r1 r2)
  "Return non-nil if folding range R1 ends before R2"
  (inline-letevals (r1 r2)
    (inline-quote
     ;; Ensure r1 comes before r2
     (or (< (lsp--folding-range-beg ,r1)
            (lsp--folding-range-beg ,r2))
         ;; If beg(r1) == beg(r2), make sure r2 ends first
         (and (= (lsp--folding-range-beg ,r1)
                 (lsp--folding-range-beg ,r2))
              (< (lsp--folding-range-end ,r2)
                 (lsp--folding-range-end ,r1)))))))

(define-inline lsp--point-inside-range-p (point range)
  "Return non-nil if POINT lies inside folding range RANGE."
  (inline-letevals (point range)
    (inline-quote
     (and (>= ,point (lsp--folding-range-beg ,range))
          (<= ,point (lsp--folding-range-end ,range))))))

(cl-defun lsp--get-current-innermost-folding-range (&optional (point (point)))
  "Return the innermost folding range POINT lies in."
  (let (inner)
    (seq-doseq (range (lsp--get-folding-ranges))
      (when (lsp--point-inside-range-p point range)
        (if inner
            (when (lsp--range-inside-p inner range)
              (setq inner range))
          (setq inner range))))
    inner))

(cl-defun lsp--get-current-outermost-folding-range (&optional (point (point)))
  "Return the outermost folding range POINT lies in."
  (let (outer width)
    (seq-doseq (range (lsp--get-folding-ranges))
      (when (lsp--point-inside-range-p point range)
        (setq width (lsp--folding-range-width range))
        (if outer
            (when (> width (car outer))
              (setq outer (cons width range)))
          (setq outer (cons width range)))))
    (cdr outer)))

(defun lsp--folding-range-at-point-bounds ()
  (if (and (lsp--capability "foldingRangeProvider") lsp-enable-folding)
      (if-let ((range (lsp--get-current-innermost-folding-range)))
          (cons (lsp--folding-range-beg range)
                (lsp--folding-range-end range)))
    nil))
(put 'lsp-folding-range 'bounds-of-thing-at-point
      #'lsp--folding-range-at-point-bounds)

(defun lsp--get-nearest-folding-range (&optional backward)
  (let ((point (point))
        (found nil))
    (while (not
            (or found
                (if backward
                    (<= point (point-min))
                  (>= point (point-max)))))
      (if backward (cl-decf point) (cl-incf point))
      (setq found (lsp--get-current-innermost-folding-range point)))
    found))

(defun lsp--folding-range-at-point-forward-op (n)
  (when (and (lsp--capability "foldingRangeProvider") lsp-enable-folding
             (not (zerop n)))
    (cl-block break
      (dotimes (_ (abs n))
        (if-let (range (lsp--get-nearest-folding-range (< n 0)))
            (goto-char (if (< n 0)
                           (lsp--folding-range-beg range)
                         (lsp--folding-range-end range)))
          (cl-return-from break))))))
(put 'lsp--folding-range 'forward-op
      #'lsp--folding-range-at-point-forward-op)

(defun lsp--folding-range-at-point-beginning-op ()
  (goto-char (car (lsp--folding-range-at-point-bounds))))
(put 'lsp--folding-range 'beginning-op
      #'lsp--folding-range-at-point-beginning-op)

(defun lsp--folding-range-at-point-end-op ()
  (goto-char (cdr (lsp--folding-range-at-point-bounds))))
(put 'lsp--folding-range 'end-op
      #'lsp--folding-range-at-point-end-op)

(defun lsp--range-at-point-bounds ()
  (or (lsp--folding-range-at-point-bounds)
      (if-let ((range (and
                       (lsp--capability "hoverProvider")
                       (->> (lsp--text-document-position-params)
                            (lsp-request "textDocument/hover")
                            (gethash "range")))))
          (cons (lsp--position-to-point (gethash "start" range))
                (lsp--position-to-point (gethash "end" range)))
        nil)))

;; A more general purpose "thing", useful for applications like focus.el
(put 'lsp--range 'bounds-of-thing-at-point
      #'lsp--range-at-point-bounds)


;; lenses support

(defun lsp--lens-text-width (from to)
  "Measure the width of the text between FROM and TO.
Results are meaningful only if FROM and TO are on the same line."
  ;; `current-column' takes prettification into account
  (- (save-excursion (goto-char to) (current-column))
     (save-excursion (goto-char from) (current-column))))

(defun lsp--lens-update (ov)
  "Redraw quick-peek overlay OV."
  (let ((offset (lsp--lens-text-width (save-excursion
                                        (beginning-of-visual-line)
                                        (point))
                                      (save-excursion
                                        (beginning-of-line-text)
                                        (point)))))
    (save-excursion
      (goto-char (overlay-start ov))
      (overlay-put ov
                   'before-string
                   (concat (make-string offset ?\s)
                           (overlay-get ov 'lsp--lens-contents)
                           "\n")))))

(defun lsp--lens-overlay-ensure-at (pos)
  "Find or create a lens for the line at POS."
  (or (car (cl-remove-if-not (lambda (ov) (lsp--lens-overlay-matches-pos ov pos)) lsp--lens-overlays))
      (let* ((ov (save-excursion
                   (goto-char pos)
                   (make-overlay (point-at-bol) (1+ (point-at-eol))))))
        (overlay-put ov 'lsp-lens t)
        ov)))

(defun lsp--lens-show (str pos)
  "Show STR in an inline window at POS."
  (let ((ov (lsp--lens-overlay-ensure-at pos)))
    (save-excursion
      (goto-char pos)
      (setf (overlay-get ov 'lsp--lens-contents) str)
      (lsp--lens-update ov))
    ov))

(defun lsp--lens-overlay-matches-pos (ov pos)
  "Check if OV is a lens covering POS."
  (and (overlay-get ov 'lsp-lens)
       (<= (overlay-start ov) pos)
       (< pos (overlay-end ov))))

(defun lsp--lens-after-save ()
  "Handler for `after-save-hook' for lens mode."
  (lsp--lens-schedule-refresh t))

(defun lsp--lens-idle-function (&optional buffer)
  "Create idle function for buffer BUFFER."
  (when (and (or (not buffer) (eq (current-buffer) buffer))
             (not (equal (cons (window-start) (window-end)) lsp--lens-page)))
    (lsp--lens-schedule-refresh nil)))

(defun lsp--lens-schedule-refresh (buffer-modified?)
  "Call each of the backend.
BUFFER-MODIFIED? determines whether the buffer is modified or not."
  (-some-> lsp--lens-refresh-timer cancel-timer)

  (setq-local lsp--lens-page (cons (window-start) (window-end)))
  (setq-local lsp--lens-refresh-timer
              (run-with-timer lsp-lens-debounce-interval nil 'lsp--lens-refresh buffer-modified?)))

(defun lsp--lens-keymap (command)
  (let ((map (make-sparse-keymap))
        (server-id (->> (lsp-workspaces)
                        cl-first
                        (or lsp--cur-workspace)
                        lsp--workspace-client
                        lsp--client-server-id)))
    (define-key map [mouse-1]
      (lambda ()
        (interactive)
        (lsp-execute-command server-id
                             (intern (gethash "command" command))
                             (gethash "arguments" command))))
    map))

(defun lsp--lens-display (lenses)
  "Show LENSES."
  ;; rerender only if there are lenses which are not processed or if their count
  ;; has changed(e. g. delete lens should trigger redisplay).
  (when (or (--any? (not (gethash "processed" it)) lenses) (eq (length lenses) lsp--lens-last-count))
    (setq-local lsp--lens-last-count (length lenses))
    (let ((overlays
           (->> lenses
                (--filter (gethash "command" it))
                (--map (prog1 it (puthash "processed" t it)))
                (--group-by (lsp--ht-get it "range" "start" "line"))
                (-map
                 (-lambda ((_ . lenses))
                   (let ((sorted (--sort (< (lsp--ht-get it "range" "start" "character")
                                            (lsp--ht-get other "range" "start" "character"))
                                         lenses)))
                     (list (lsp--position-to-point (lsp--ht-get (cl-first sorted) "range" "start"))
                           (s-join (propertize "|" 'face 'lsp-lens-face)
                                   (-map
                                    (-lambda ((lens &as &hash "command" (command &as &hash "title")))
                                      (propertize
                                       title
                                       'face 'lsp-lens-face
                                       'mouse-face 'lsp-lens-mouse-face
                                       'local-map (lsp--lens-keymap command)))
                                    sorted))))))
                (-map (-lambda ((position str))
                        (lsp--lens-show str position))))))
      (--each lsp--lens-overlays
        (unless (-contains? overlays it)
          (delete-overlay it)))
      (setq-local lsp--lens-overlays overlays))))

(defun lsp--lens-refresh (buffer-modified?)
  "Refresh lenses using lenses backend.
BUFFER-MODIFIED? determines whether the buffer is modified or not."
  (dolist (backend lsp-lens-backends)
    (funcall backend buffer-modified?
             (lambda (lenses version)
               (lsp--process-lenses backend lenses version)))))

(defun lsp--process-lenses (backend lenses version)
  "Process LENSES originated from BACKEND.
VERSION is the version of the file. The lenses has to be
refreshed only when all backends have reported for the same
version."
  (setq-local lsp--lens-data (or lsp--lens-data (make-hash-table)))
  (puthash backend (cons version (append lenses nil)) lsp--lens-data)

  (-let [backend-data (->> lsp--lens-data ht-values (-filter #'cl-rest))]
    (when (seq-every-p (-lambda ((version))
                         (eq version lsp--cur-version))
                       backend-data)
      ;; display the data only when the backends have reported data for the
      ;; current version of the file
      (lsp--lens-display (-flatten (-map 'cl-rest backend-data))))))

(defun lsp-lens-show ()
  "Display lenses in the buffer."
  (interactive)
  (->> (lsp-request "textDocument/codeLens"
                    `(:textDocument (:uri
                                     ,(lsp--path-to-uri buffer-file-name))))
       (seq-map (lambda (it)
                  (if (gethash "command" it)
                      it
                    (lsp-request "codeLens/resolve" it))))
       lsp--lens-display))

(defun lsp-lens-hide ()
  "Delete all lenses."
  (interactive)
  (let ((scroll-preserve-screen-position t))
    (seq-do 'delete-overlay lsp--lens-overlays)
    (setq-local lsp--lens-overlays nil)))

(defun lsp--lens-backend-not-loaded? (lens)
  "Return t if LENS has to be loaded."
  (-let (((&hash "range" (&hash "start") "command" "pending") lens))
    (and (< (window-start) (lsp--position-to-point start) (window-end))
         (not command)
         (not pending))))

(defun lsp--lens-backend-present? (lens)
  "Return t if LENS has to be loaded."
  (-let (((&hash "range" (&hash "start") "command") lens))
    (or command
        (not (< (window-start) (lsp--position-to-point start) (window-end))))))

(defun lsp--lens-backend-fetch-missing (lenses callback file-version)
  "Fetch LENSES without command in for the current window.

TICK is the buffer modified tick. If it does not match
`buffer-modified-tick' at the time of receiving the updates the
updates must be discarded..
CALLBACK - the callback for the lenses.
FILE-VERSION - the version of the file."
  (seq-each
   (lambda (it)
     (with-lsp-workspace
         (gethash "workspace" it)
       (puthash "pending" t it)
       (remhash "workspace" it)
       (lsp-request-async "codeLens/resolve" it
                          (lambda (lens)
                            (remhash "pending" it)
                            (puthash "command" (gethash "command" lens) it)
                            (when (seq-every-p #'lsp--lens-backend-present? lenses)
                              (funcall callback lenses file-version)))
                          :mode 'tick)))
   (seq-filter #'lsp--lens-backend-not-loaded? lenses)))

(defun lsp-lens-backend (modified? callback)
  "Lenses backend using `textDocument/codeLens'.
MODIFIED? - t when buffer is modified since the last invocation.
CALLBACK - callback for the lenses."
  (when (lsp--find-workspaces-for "textDocument/codeLens")
    (if modified?
        (progn
          (setq-local lsp--lens-backend-cache nil)
          (lsp-request-async "textDocument/codeLens"
                             `(:textDocument (:uri ,(lsp--path-to-uri buffer-file-name)))
                             (lambda (lenses)
                               (setq-local lsp--lens-backend-cache
                                           (seq-mapcat
                                            (-lambda ((workspace . workspace-lenses))
                                              ;; preserve the original workspace so we can later use it to resolve the lens
                                              (seq-do (-partial 'puthash "workspace" workspace) workspace-lenses)
                                              workspace-lenses)
                                            lenses))
                               (if (--every? (gethash "command" it) lsp--lens-backend-cache)
                                   (funcall callback lsp--lens-backend-cache lsp--cur-version)
                                 (lsp--lens-backend-fetch-missing lsp--lens-backend-cache callback lsp--cur-version)))
                             :mode 'tick
                             :no-merge t))
      (if (-all? #'lsp--lens-backend-present? lsp--lens-backend-cache)
          (funcall callback lsp--lens-backend-cache lsp--cur-version)
        (lsp--lens-backend-fetch-missing lsp--lens-backend-cache callback lsp--cur-version)))))

(defun lsp--lens-stop-timer ()
  "Stop `lsp--lens-idle-timer'."
  (-some-> lsp--lens-idle-timer cancel-timer)
  (setq-local lsp--lens-idle-timer nil))

(define-minor-mode lsp-lens-mode
  "toggle code-lens overlays"
  :group 'lsp-mode
  :global nil
  :init-value nil
  :lighter "Lens"
  (cond
   (lsp-lens-mode
    (setq-local lsp--lens-idle-timer (run-with-idle-timer
                                      lsp-lens-check-interval t #'lsp--lens-idle-function (current-buffer)))
    (lsp--lens-refresh t)
    (add-hook 'kill-buffer-hook #'lsp--lens-stop-timer nil t)
    (add-hook 'after-save-hook 'lsp--lens-after-save nil t))
   (t
    (lsp--lens-stop-timer)
    (lsp-lens-hide)
    (remove-hook 'kill-buffer-hook #'lsp--lens-stop-timer t)
    (remove-hook 'after-save-hook #'lsp--lens-after-save t)
    (setq-local lsp--lens-last-count nil)
    (setq-local lsp--lens-backend-cache nil))))



(defvar lsp-mode-map (make-sparse-keymap)
  "Keymap for `lsp-mode'.")

(define-minor-mode lsp-mode ""
  nil nil nil
  :keymap lsp-mode-map
  :lighter (:eval (lsp-mode-line))
  :group 'lsp-mode)

(easy-menu-define lsp-mode-menu lsp-mode-map
  "Menu for lsp-mode."
  '("LSP"
    ["Describe current LSP session" lsp-describe-session]
    ["Add folder to workspace" lsp-workspace-folders-add]
    ["Remove folder from workspace" lsp-workspace-folders-remove]
    ["Switch to another workspace folder" lsp-workspace-folders-switch]
    "--"
    ["Toggle Code Lenses" lsp-lens-mode]
    "--"
    ["Describe thing at point" lsp-describe-thing-at-point]
    ["Execute code action" lsp-execute-code-action]
    ["Format buffer" lsp-format-buffer]
    ["Format current region or line" lsp-format-region]
    ["Highlight references to symbol under point" lsp-document-highlight]
    ["Rename symbol under point" lsp-rename]
    "--"
    ["Find declarations of symbol under point" lsp-find-declaration]
    ["Find definitions of symbol" lsp-find-definition]
    ["Find implementations of symbol under point" lsp-find-implementation]
    ["Find references to symbol under point" lsp-find-references]
    ["Find type definitions of symbol under point" lsp-find-type-definition]
    "--"
    ["View IO logs for workspace" lsp-switch-to-io-log-buffer]
    ["Shutdown language server" lsp-shutdown-workspace]
    ["Restart language server" lsp-restart-workspace]))

(defun lsp-mode-line ()
  "Construct the mode line text."
  (if-let (workspaces (lsp-workspaces))
      (concat " LSP" (string-join (--map (format "[%s]" (lsp--workspace-print it))
                                         workspaces)))
    (concat " LSP" (propertize "[Disconnected]" 'face 'warning))))

(defalias 'make-lsp-client 'make-lsp--client)

(cl-defstruct lsp--registered-capability
  (id "" :type string)
  (method " " :type string)
  (options nil))

;; A ‘lsp--workspace’ object represents exactly one language server process.
(cl-defstruct lsp--workspace
  ;; ‘parser’ is a ‘lsp--parser’ object used to parse messages for this
  ;; workspace.  Parsers are not shared between workspaces.
  (parser nil :read-only t)

  ;; ‘server-capabilities’ is a hash table of the language server capabilities.
  ;; It is the hash table representation of a LSP ServerCapabilities structure;
  ;; cf. https://microsoft.github.io/language-server-protocol/specification#initialize.
  (server-capabilities nil)

  ;; ‘registered-server-capabilities’ is a list of hash tables that represent
  ;; dynamically-registered Registration objects.  See
  ;; https://microsoft.github.io/language-server-protocol/specification#client_registerCapability.
  (registered-server-capabilities nil)

  ;; ‘root’ is a directory name or a directory file name for the workspace
  ;; root.  ‘lsp-mode’ passes this directory to the ‘initialize’ method of the
  ;; language server; see
  ;; https://microsoft.github.io/language-server-protocol/specification#initialize.
  (root nil :ready-only t)

  ;; ‘client’ is the ‘lsp--client’ object associated with this workspace.
  (client nil :read-only t)

  ;; ‘host-root’ contains the host root info as derived from `file-remote-p'. It
  ;; used to deriver the file path in `lsp--uri-to-path' when using tramp
  ;; connection.
  (host-root nil)

  ;; ‘proc’ is a process object; it may represent a regular process, a pipe, or
  ;; a network connection.  ‘lsp-mode’ communicates with ‘proc’ using the
  ;; language server protocol.  ‘proc’ corresponds to the COMMUNICATION-PROCESS
  ;; element of the return value of the client’s ‘get-root’ field, which see.
  (proc nil)

  ;; ‘proc’ is a process object; it must represent a regular process, not a
  ;; pipe or network process.  It represents the actual server process that
  ;; corresponds to this workspace.  ‘cmd-proc’ corresponds to the
  ;; COMMAND-PROCESS element of the return value of the client’s ‘get-root’
  ;; field, which see.
  (cmd-proc nil)

  ;; ‘buffers’ is a list of buffers associated with this workspace.
  (buffers nil)

  ;; ‘highlight-overlays’ is a hash table mapping buffers to a list of overlays
  ;; used for highlighting the symbol under point.
  (highlight-overlays (make-hash-table :test 'eq) :read-only t)

  ;; Extra client capabilities provided by third-party packages using
  ;; `lsp-register-client-capabilities'. It's value is an alist of (PACKAGE-NAME
  ;; . CAPS), where PACKAGE-NAME is a symbol of the third-party package name,
  ;; and CAPS is either a plist of the client capabilities, or a function that
  ;; takes no argument and returns a plist of the client capabilities or nil.")
  (extra-client-capabilities nil)

  ;; Workspace status
  (status nil)

  ;; ‘metadata’ is a generic storage for workspace specific data. It is
  ;; accessed via `lsp-workspace-set-metadata' and `lsp-workspace-set-metadata'
  (metadata (make-hash-table :test 'equal))

  ;; contains all the file notification watches that have been created for the
  ;; current workspace in format filePath->file notification handle.
  (watches (make-hash-table :test 'equal))

  ;; list of workspace folders
  (workspace-folders nil)

  ;; ‘last-id’ the last request id for the current workspace.
  (last-id 0)

  ;; ‘status-string’ allows extensions to specify custom status string based on
  ;; the Language Server specific messages.
  (status-string nil)

  ;; ‘shutdown-action’ flag used to mark that workspace should not be restarted(e. g. it
  ;; was stopped).
  (shutdown-action)

  ;; ‘diagnostics’ a hashmap with workspace diagnostics.
  (diagnostics (make-hash-table :test 'equal))
  ;; the `ewoc' object for displaying I/O to and from the server
  (ewoc nil))

(cl-defstruct lsp-session
  ;; contains the folders that are part of the current session
  (folders)
  ;; contains the folders that must not be imported in the current workspace.
  (folders-blacklist)
  ;; contains the list of folders that must be imported in a project in case of
  ;; multi root LSP server.
  (server-id->folders (make-hash-table :test 'equal) :read-only t)
  ;; folder to list of the servers that are associated with the folder.
  (folder->servers (make-hash-table :test 'equal) :read-only t)
  ;; ‘metadata’ is a generic storage for workspace specific data. It is
  ;; accessed via `lsp-workspace-set-metadata' and `lsp-workspace-set-metadata'
  (metadata (make-hash-table :test 'equal)))

(defun lsp-workspace-status (status-string &optional workspace)
  "Set current workspace status to STATUS-STRING.
If WORKSPACE is not specified defaults to lsp--cur-workspace."
  (setf (lsp--workspace-status-string (or workspace lsp--cur-workspace)) status-string))

(defun lsp-session-set-metadata (key value &optional _workspace)
  "Associate KEY with VALUE in the WORKSPACE metadata.
If WORKSPACE is not provided current workspace will be used."
  (puthash key value (lsp-session-metadata (lsp-session))))

(defalias 'lsp-workspace-set-metadata 'lsp-session-set-metadata)

(defun lsp-session-get-metadata (key &optional _workspace)
  "Lookup KEY in WORKSPACE metadata.
If WORKSPACE is not provided current workspace will be used."
  (gethash key (lsp-session-metadata (lsp-session))))

(defalias 'lsp-workspace-get-metadata 'lsp-session-get-metadata)

(define-inline lsp--make-notification (method &optional params)
  "Create notification body for method METHOD and parameters PARAMS."
  (inline-quote
   (progn (cl-check-type ,method string)
          (list :jsonrpc "2.0" :method ,method :params ,params))))

(defun lsp--make-request (method &optional params)
  "Create request body for method METHOD and parameters PARAMS."
  (lsp--make-notification method params))

(defalias 'lsp-make-request 'lsp--make-request)

(defun lsp--make-response (request result)
  "Create reponse for REQUEST with RESULT."
  `(:jsonrpc "2.0" :id ,(gethash "id" request) :result ,result))

(defun lsp-make-notification (method &optional params)
  "Create notification body for method METHOD and parameters PARAMS."
  (lsp--make-notification method params))

(defun lsp--make-message (params)
  "Create a LSP message from PARAMS, after encoding it to a JSON string."
  (let* ((json-false :json-false)
         (body (if (and lsp-use-native-json
                        (fboundp 'json-serialize))
                   (with-no-warnings
                     (json-serialize params
                                     :null-object nil
                                     :false-object :json-false))
                 (json-encode params))))
    (concat "Content-Length: "
            (number-to-string (1+ (string-bytes body)))
            "\r\n\r\n"
            body
            "\n")))

(cl-defstruct lsp--log-entry
  (timestamp)
  (process-time)
  (type)
  (method)
  (id)
  (body))

(defun lsp--make-log-entry (method id body type &optional process-time)
  "Create an outgoing log object from BODY with method METHOD and id ID.
If ID is non-nil, then the body is assumed to be a notification.
TYPE can either be 'incoming or 'outgoing"
  (cl-assert (memq type '(incoming-req outgoing-req incoming-notif
                                       outgoing-notif incoming-resp
                                       outgoing-resp)))
  (cl-check-type method string)
  (when id (cl-check-type id number))
  (make-lsp--log-entry
   :timestamp (format-time-string "%I:%M:%S %p")
   :process-time process-time
   :method method
   :id id
   :type type
   :body body))

(defun lsp--log-entry-pp (entry)
  (cl-assert (lsp--log-entry-p entry))
  (pcase-let (((cl-struct lsp--log-entry timestamp method id type process-time
                          body)
               entry)
              (json-false :json-false)
              (json-encoding-pretty-print t)
              (str nil))
    (setq str
          (concat (format "[Trace - %s]\n" timestamp)
                  (pcase type
                    ('incoming-req (format "Received request '%s - (%d).\n" method id))
                    ('outgoing-req (format "Sending request '%s - (%d)'.\n" method id))

                    ('incoming-notif (format "Received notification '%s'.\n" method))
                    ('outgoing-notif (format "Sending notification '%s'.\n" method))

                    ('incoming-resp (format "Received response '%s - (%d)' in %dms.\n"
                                            method id process-time))
                    ('outgoing-resp
                      (format
                       "Sending response '%s - (%d)'. Processing request took %dms\n"
                       method id process-time)))
                  "\n"
                  (if (memq type '(incoming-resp ougoing-resp))
                      "Result: \n"
                    "Params: \n")
                  (json-encode body) "\n"
                  "\n\n\n"))
    (setq str (propertize str 'mouse-face 'highlight 'read-only t))
    (insert str)))

(defvar-local lsp--log-io-ewoc nil)

(defun lsp--get-create-io-ewoc (workspace)
  (if (and (lsp--workspace-ewoc workspace)
           (buffer-live-p (ewoc-buffer (lsp--workspace-ewoc workspace))))
      (lsp--workspace-ewoc workspace)
    (let ((buffer (get-buffer-create (format "*lsp-log: %s*"
                                             (lsp--workspace-root workspace)))))
      (with-current-buffer buffer
        (unless (eq 'lsp-log-io-mode major-mode) (lsp-log-io-mode))
        (setq-local lsp--log-io-ewoc (ewoc-create #'lsp--log-entry-pp nil nil))
        (setf (lsp--workspace-ewoc workspace) lsp--log-io-ewoc))
      (lsp--workspace-ewoc workspace))))

(defun lsp--ewoc-count (ewoc)
  (let* ((count 0)
         (count-fn (lambda (_) (setq count (1+ count)))))
    (ewoc-map count-fn ewoc)
    count))

(defun lsp--log-entry-new (entry workspace)
  (let* ((ewoc (lsp--get-create-io-ewoc workspace))
         (count (and (not (eq lsp-io-messages-max t)) (lsp--ewoc-count ewoc)))
         (node (if (or (eq lsp-io-messages-max t)
                       (>= lsp-io-messages-max count))
                   nil
                 (ewoc-nth ewoc (1- lsp-io-messages-max))))
         (prev nil)
         (inhibit-read-only t))
    (while node
      (setq prev (ewoc-prev ewoc node))
      (ewoc-delete ewoc node)
      (setq node prev))
    (ewoc-enter-last ewoc entry)))

(defun lsp--send-notification (body)
  "Send BODY as a notification to the language server."
  (lsp-foreach-workspace
   (when lsp-print-io
     (lsp--log-entry-new (lsp--make-log-entry
                          (plist-get body :method)
                          nil (plist-get body :params) 'outgoing-notif)
                         lsp--cur-workspace))
   (lsp--send-no-wait (lsp--make-message body)
                      (lsp--workspace-proc lsp--cur-workspace))))

(defalias 'lsp-send-notification 'lsp--send-notification)

(defun lsp-notify (method params)
  "Send notification METHOD with PARAMS."
  (lsp--send-notification (lsp--make-notification method params)))

(defun lsp--cur-workspace-check ()
  "Check whether buffer lsp workspace(s) are set."
  (cl-assert (lsp-workspaces) nil
             "No language server(s) is associated with this buffer."))

(defun lsp--send-request (body &optional no-wait no-merge)
  "Send BODY as a request to the language server, get the response.
If NO-WAIT is non-nil, don't synchronously wait for a response.
If NO-MERGE is non-nil, don't merge the results but return alist workspace->result."
  (lsp-request (plist-get body :method)
               (plist-get body :params)
               :no-wait no-wait
               :no-merge no-merge))

(defalias 'lsp-send-request 'lsp--send-request
  "Send BODY as a request to the language server and return the response synchronously.
\n(fn BODY)")

(cl-defun lsp-request (method params &key no-wait no-merge)
  "Send request METHOD with PARAMS.
If NO-MERGE is non-nil, don't merge the results but return alist workspace->result.
If NO-WAIT is non-nil send the request as notification."
  (if no-wait
      (lsp-notify method params)
    (let* ((send-time (time-to-seconds (current-time)))
           ;; max time by which we must get a response
           (expected-time (+ send-time lsp-response-timeout))
           resp-result resp-error)
      (lsp-request-async method params (lambda (res) (setf resp-result (or res :finished)))
                         :error-handler (lambda (err) (setf resp-error err))
                         :no-merge no-merge
                         :mode 'detached)

      (while (not (or resp-error resp-result))
        (accept-process-output nil 0.01)
        (when (< expected-time (time-to-seconds (current-time)))
          (error "Timeout while waiting for response. Method: %s." method)))

      (cond
       ((eq resp-result :finished) nil)
       (resp-result resp-result)
       ((ht? resp-error) (error (gethash "message" resp-error)))
       (t (error (gethash "message" (cl-first resp-error))))))))

(cl-defun lsp-request-async (method params callback &key mode error-handler no-merge )
  "Send request METHOD with PARAMS."
  (lsp--send-request-async `(:jsonrpc "2.0" :method ,method :params ,params) callback mode error-handler no-merge))

(defun lsp--create-async-callback (count callback mode method no-merge)
  "Create async handler expecting COUNT results, merge them and call CALLBACK.
MODE determines when the callback will be called depending on the
condition of the original buffer. METHOD is the invoked method.
If NO-MERGE is non-nil, don't merge the results but return alist workspace->result."
  (let ((buf (current-buffer))
        results)
    (cl-labels ((handle-result () (funcall
                                   callback
                                   (if no-merge
                                       results
                                     (lsp--merge-results (-map #'cl-rest results) method)))))
      (pcase mode
        ('detached (lambda (result)
                     (push (cons lsp--cur-workspace result) results)

                     (when (and (eq (length results) count))
                       (handle-result))))
        ('alive (lambda (result)
                  (push (cons lsp--cur-workspace result) results)
                  (if (and (eq (length results) count)
                           (buffer-live-p buf))
                      (with-current-buffer buf
                        (handle-result))
                    (lsp-log "Buffer is not alive ignoring reponse. Method %s." method))))
        ('tick (let ((tick (buffer-chars-modified-tick)))
                 (lambda (result)
                   (when (buffer-live-p buf)
                     (with-current-buffer buf
                       (if (and (= tick (buffer-chars-modified-tick)))
                           (progn
                             (push (cons lsp--cur-workspace result)  results)
                             (when (eq (length results) count)
                               (handle-result)))
                         (lsp-log "Buffer modified ignoring response. Method %s." method)))))))
        (_ (lambda (result)
             (push (cons lsp--cur-workspace result) results)
             (if (and (eq (length results) count)
                      (eq buf (current-buffer)))
                 (handle-result)
               (lsp-log "Buffer switched - ignoring reponse. Method %s" method))))))))

(defun lsp--create-default-error-handler (method)
  "Default error handler.
METHOD is the executed method."
  (lambda (error)
    (lsp--warn "%s" (or (gethash "message" error)
                        (format "%s Request has failed" method)))))

(defun lsp--send-request-async (body callback &optional mode error-callback no-merge)
  "Send BODY as a request to the language server.
Call CALLBACK with the response recevied from the server
asynchronously. MODE determines when the callback will be called
depending on the condition of the original buffer. It could be:
`detached' which means that the callback will be executed no
matter what has happened to the buffer. `alive' - the callback
will be executed only if the buffer from which the call was
executed is still alive. `current' the callback will be executed
only if the original buffer is still selected. `tick' - the
callback will be executed only if the buffer was not modified.

ERROR-CALLBACK will be called in case the request has failed.
If NO-MERGE is non-nil, don't merge the results but return alist workspace->result."
  (lsp--flush-delayed-changes)

  (if-let ((target-workspaces (lsp--find-workspaces-for body)))
      (let* ((start-time (current-time))
             (method (plist-get body :method))
             (workspaces-count (length target-workspaces))
             (async-callback (lsp--create-async-callback workspaces-count
                                                         callback
                                                         mode
                                                         method
                                                         no-merge))
             (error-async-callback (lsp--create-async-callback workspaces-count
                                                               (or error-callback
                                                                   (lsp--create-default-error-handler method))
                                                               mode
                                                               method
                                                               no-merge))
             (id (cl-incf lsp-last-id))
             (body (plist-put body :id id)))
        (--each target-workspaces
          (with-lsp-workspace it
            (when lsp-print-io
              (lsp--log-entry-new (lsp--make-log-entry method id
                                                       (plist-get body :params)
                                                       'outgoing-req)
                                  it))
            (let ((message (lsp--make-message body)))
              (puthash id
                       (list async-callback error-async-callback method start-time (current-time))
                       (-> lsp--cur-workspace
                           lsp--workspace-client
                           lsp--client-response-handlers))
              (lsp--send-no-wait message (lsp--workspace-proc lsp--cur-workspace)))))
        body)
    (error "No workspace could handle %s" (plist-get body :method))))

(defalias 'lsp-send-request-async 'lsp--send-request-async)

;; Clean up the entire state of lsp mode when Emacs is killed, to get rid of any
;; pending language servers.
(add-hook 'kill-emacs-hook #'lsp--global-teardown)

(defun lsp--global-teardown ()
  "Unload working workspaces."
  (lsp-foreach-workspace (lsp--shutdown-workspace)))

(defun lsp--shutdown-workspace ()
  "Shut down the language server process for ‘lsp--cur-workspace’."
  (with-demoted-errors "LSP error: %S"
    (lsp-request "shutdown" nil)
    (lsp-notify "exit" nil))
  (lsp--uninitialize-workspace))

(defun lsp--uninitialize-workspace ()
  "Cleanup buffer state.
When a workspace is shut down, by request or from just
disappearing, unset all the variables related to it."

  (let ((proc (lsp--workspace-cmd-proc lsp--cur-workspace)))
    (when (process-live-p proc)
      (kill-process proc))
    (unless (lsp-workspaces)
      (lsp--managed-mode -1))))

(defun lsp--client-capabilities ()
  "Return the client capabilites."
  `((workspace . ((workspaceEdit . ((documentChanges . t)
                                    (resourceOperations . ["create" "rename" "delete"])))
                  (applyEdit . t)
                  (symbol . ((symbolKind . ((valueSet . ,(apply 'vector (number-sequence 1 26)))))))
                  (executeCommand . ((dynamicRegistration . :json-false)))
                  (didChangeWatchedFiles . ((dynamicRegistration . t)))
                  (workspaceFolders . t)
                  (configuration . t)))
    (textDocument . ((declaration . ((linkSupport . t)))
                     (definition . ((linkSupport . t)))
                     (implementation . ((linkSupport . t)))
                     (typeDefinition . ((linkSupport . t)))
                     (synchronization . ((willSave . t) (didSave . t) (willSaveWaitUntil . t)))
                     (documentSymbol . ((symbolKind . ((valueSet . ,(apply 'vector (number-sequence 1 26)))))
                                        (hierarchicalDocumentSymbolSupport . t)))
                     (formatting . ((dynamicRegistration . t)))
                     (codeAction . ((dynamicRegistration . t)
                                    (codeActionLiteralSupport . ((codeActionKind .
                                                                                 ((valueSet . [""
                                                                                               "quickfix"
                                                                                               "refactor"
                                                                                               "refactor.extract"
                                                                                               "refactor.inline"
                                                                                               "refactor.rewrite"
                                                                                               "source"
                                                                                               "source.organizeImports"])))))))
                     (completion . ((completionItem . ((snippetSupport . ,(if lsp-enable-snippet t :json-false))))))
                     (signatureHelp . ((signatureInformation . ((parameterInformation . ((labelOffsetSupport . t)))))))
                     (documentLink . ((dynamicRegistration . t)))
                     (hover . ((contentFormat . ["plaintext" "markdown"])))
                     (foldingRange . ,(when lsp-enable-folding
                                        `((dynamicRegistration . t)
                                          (rangeLimit . ,lsp-folding-range-limit)
                                          (lineFoldingOnly . ,lsp-folding-line-folding-only))))))))

(defun lsp-find-roots-for-workspace (workspace session)
  "Get all roots for the WORKSPACE."
  (-filter #'identity (ht-map (lambda (folder workspaces)
                                (when (-contains? workspaces workspace)
                                  folder))
                              (lsp-session-folder->servers session))))

(defun lsp-session-watches (&optional session)
  "Get watches created for SESSION."
  (or (gethash "__watches" (lsp-session-metadata (or session (lsp-session))))
      (-let [res (make-hash-table :test 'equal)]
        (puthash "__watches" res (lsp-session-metadata (or session (lsp-session))))
        res)))

(defun lsp--file-process-event (session root-folder event)
  "Process file event."
  (let ((changed-file (cl-caddr event)))
    (->> session
         lsp-session-folder->servers
         (gethash root-folder)
         (seq-do (lambda (workspace)
                   (when (->> workspace
                              lsp--workspace-registered-server-capabilities
                              (-any? (lambda (capability)
                                       (and (string= (lsp--registered-capability-method capability)
                                                     "workspace/didChangeWatchedFiles")
                                            (->> capability
                                                 lsp--registered-capability-options
                                                 (gethash "watchers")
                                                 (seq-find (-lambda ((&hash "globPattern" glob-pattern))
                                                             (-let [glob-regex (eshell-glob-regexp glob-pattern)]
                                                               (or (string-match glob-regex changed-file)
                                                                   (string-match glob-regex (f-relative changed-file root-folder)))))))))))
                     (with-lsp-workspace workspace
                       (lsp-notify
                        "workspace/didChangeWatchedFiles"
                        `((changes . [((type . ,(alist-get (cadr event) lsp--file-change-type))
                                       (uri . ,(lsp--path-to-uri changed-file)))]))))))))))

(defun lsp--server-register-capability (reg)
  "Register capability REG."
  (-let (((&hash "method" "id" "registerOptions") reg)
         (session (lsp-session)))
    (when (and lsp-enable-file-watchers
               (string= method "workspace/didChangeWatchedFiles"))
      (-let* ((created-watches (lsp-session-watches session))
              (root-folders (cl-set-difference (lsp-find-roots-for-workspace lsp--cur-workspace session)
                                               (ht-keys created-watches))))
        ;; create watch for each root folder withtout such
        (dolist (folder root-folders)
          (puthash folder (lsp-watch-root-folder
                           folder
                           (-partial #'lsp--file-process-event session folder))
                   created-watches))))

    (push
     (make-lsp--registered-capability :id id :method method :options registerOptions)
     (lsp--workspace-registered-server-capabilities lsp--cur-workspace))))

(defun lsp--cleanup-hanging-watches ()
  "Cleanup watches in case there are no more workspaces that are interested
in that particular folder."
  (let* ((session (lsp-session))
         (watches (lsp-session-watches session)))
    (dolist (watched-folder (ht-keys watches))
      (when (-none? (lambda (workspace)
                      (with-lsp-workspace workspace
                        (lsp--registered-capability "workspace/didChangeWatchedFiles")))
                    (gethash watched-folder (lsp-session-folder->servers (lsp-session))))
        (lsp-log "Cleaning up watches for folder %s. There is no workspace watching this folder..." watched-folder)
        (lsp-kill-watch (gethash watched-folder watches))
        (remhash watched-folder watches)))))

(defun lsp--server-unregister-capability (unreg)
  "Unregister capability UNREG."
  (-let [(&hash "id" "method") unreg]
    (setf (lsp--workspace-registered-server-capabilities lsp--cur-workspace)
          (seq-remove (lambda (e) (equal (lsp--registered-capability-id e) id))
                      (lsp--workspace-registered-server-capabilities lsp--cur-workspace)))
    (when (string= method "workspace/didChangeWatchedFiles")
      (lsp--cleanup-hanging-watches))))

(defun lsp--server-capabilities ()
  "Return the capabilities of the language server associated with the buffer."
  (->> (lsp-workspaces)
       (-map #'lsp--workspace-server-capabilities)
       (-filter #'identity)
       (apply #'ht-merge)))

(defun lsp--send-open-close-p ()
  "Return whether open and close notifications should be sent to the server."
  (let ((sync (gethash "textDocumentSync" (lsp--server-capabilities))))
    (and (hash-table-p sync)
         (gethash "openClose" sync))))

(defun lsp--send-will-save-p ()
  "Return whether will save notifications should be sent to the server."
  (let ((sync (gethash "textDocumentSync" (lsp--server-capabilities))))
    (and (hash-table-p sync)
         (gethash "willSave" sync))))

(defun lsp--send-will-save-wait-until-p ()
  "Return whether will save wait until notifications should be sent to the server."
  (let ((sync (gethash "textDocumentSync" (lsp--server-capabilities))))
    (and (hash-table-p sync)
         (gethash "willSaveWaitUntil" sync))))

(defun lsp--save-include-text-p ()
  "Return whether save notifications should include the text document's contents."
  (let ((sync (gethash "textDocumentSync" (lsp--server-capabilities))))
    (and (hash-table-p sync)
         (hash-table-p (gethash "save" sync nil))
         (gethash "includeText" (gethash "save" sync)))))

(defun lsp--suggest-project-root ()
  "Get project root."
  (or
   (when (featurep 'projectile) (condition-case nil
                                    (projectile-project-root)
                                  (error nil)))
   (when (featurep 'project)
     (when-let ((project (project-current)))
       (car (project-roots project))))))

(defun lsp--read-from-file (file)
  "Read FILE content."
  (when (file-exists-p file)
    (cl-first (read-from-string (f-read-text file 'utf-8)))))

(defun lsp--persist (file-name to-persist)
  "Persist TO-PERSIST in FILE-NAME."
  (f-write-text (prin1-to-string to-persist) 'utf-8 file-name))

(defun lsp-workspace-folders-add (project-root)
  "Add PROJECT-ROOT to the list of workspace folders."
  (interactive
   (list (read-directory-name "Select folder to add: "
                              (or (lsp--suggest-project-root) default-directory) nil t)))
  (push project-root (lsp-session-folders (lsp-session))))

(defun lsp-workspace-folders-remove (project-root)
  "Remove PROJECT-ROOT to the list of workspace folders."
  (interactive (list (completing-read "Select folder to remove: "
                                      (lsp-session-folders (lsp-session)) nil t
                                      (lsp-find-session-folder (lsp-session) default-directory))))
  ;; send remove folder to each multiroot workspace associated with the folder
  (dolist (wks (->> (lsp-session)
                    (lsp-session-folder->servers)
                    (gethash project-root)
                    (--filter (lsp--client-multi-root (lsp--workspace-client it)))))
    (with-lsp-workspace wks
      (lsp-notify "workspace/didChangeWorkspaceFolders"
                  `(:event (:removed ,(vector (list :uri (lsp--path-to-uri project-root))))))))

  ;; turn off servers in the removed directory
  (let* ((session (lsp-session))
         (folder->servers (lsp-session-folder->servers session))
         (server-id->folders (lsp-session-server-id->folders session))
         (workspaces (gethash project-root folder->servers)))

    (remhash project-root folder->servers)

    ;; turn off the servers without root folders
    (dolist (workspace workspaces)
      (when (--none? (-contains? it workspace) (ht-values folder->servers))
        (lsp--info "Shutdown %s since folder %s is removed..."
                   (lsp--workspace-print workspace) project-root)
        (setf (lsp--workspace-shutdown-action workspace) 'shutdown)
        (with-lsp-workspace workspace (lsp--shutdown-workspace))))

    (setf (lsp-session-folders session)
          (-remove-item project-root (lsp-session-folders session)))

    (ht-aeach (puthash key
                       (-remove-item project-root value)
                       server-id->folders)
              server-id->folders)
    (lsp--persist-session (lsp-session)))

  (run-hook-with-args 'lsp-workspace-folders-changed-hook nil (list project-root)))

(defun lsp-workspace-folders-switch()
  "Switch to another workspace folder from the current session."
  (interactive)
  (find-file (completing-read "Switch to folder: " (lsp-session-folders (lsp-session)) nil t)))

(define-minor-mode lsp--managed-mode
  "Mode for source buffers managed by lsp-mode."
  nil nil nil
  (cond
   (lsp--managed-mode
    (when (and lsp-enable-indentation
               (lsp--capability "documentRangeFormattingProvider"))
      (setq-local indent-region-function #'lsp-format-region))

    (add-function :before-until (local 'eldoc-documentation-function) #'lsp-eldoc-function)
    (eldoc-mode 1)
    (add-hook 'after-change-functions #'lsp-on-change nil t)
    (add-hook 'after-revert-hook #'lsp-on-revert nil t)
    (add-hook 'after-save-hook #'lsp-on-save nil t)
    (add-hook 'auto-save-hook #'lsp--on-auto-save nil t)
    (add-hook 'before-change-functions #'lsp-before-change nil t)
    (add-hook 'before-save-hook #'lsp--before-save nil t)
    (when (and lsp-enable-completion-at-point (lsp--capability "completionProvider"))
      (setq-local completion-at-point-functions nil)
      (add-hook 'completion-at-point-functions #'lsp-completion-at-point nil t))
    (add-hook 'kill-buffer-hook #'lsp--text-document-did-close nil t)
    (add-hook 'post-self-insert-hook #'lsp--on-self-insert nil t)
    (add-hook 'post-command-hook #'lsp--highlight nil t)
    (when lsp-enable-xref
      (add-hook 'xref-backend-functions #'lsp--xref-backend nil t)))
   (t
    (setq-local indent-region-function nil)
    (remove-function (local 'eldoc-documentation-function) #'lsp-eldoc-function)

    (remove-hook 'post-command-hook #'lsp--highlight t)
    (remove-hook 'after-change-functions #'lsp-on-change t)
    (remove-hook 'after-revert-hook #'lsp-on-revert t)
    (remove-hook 'after-save-hook #'lsp-on-save t)
    (remove-hook 'auto-save-hook #'lsp--on-auto-save t)
    (remove-hook 'before-change-functions #'lsp-before-change t)
    (remove-hook 'before-save-hook #'lsp--before-save t)
    (remove-hook 'completion-at-point-functions #'lsp-completion-at-point t)
    (remove-hook 'kill-buffer-hook #'lsp--text-document-did-close t)
    (remove-hook 'post-self-insert-hook #'lsp--on-self-insert t)
    (lsp--cancel-document-link-timer))
   (remove-hook 'xref-backend-functions #'lsp--xref-backend t)))

(defun lsp--text-document-did-open ()
  "'document/didOpen event."
  (run-hooks 'lsp-before-open-hook)
  (setq-local lsp--cur-version (or lsp--cur-version 0))
  (cl-pushnew (current-buffer) (lsp--workspace-buffers lsp--cur-workspace))
  (lsp-notify
   "textDocument/didOpen"
   (list :textDocument
         (list :uri (lsp--buffer-uri)
               :languageId (lsp-buffer-language)
               :version lsp--cur-version
               :text (buffer-substring-no-properties (point-min) (point-max)))))

  (lsp--managed-mode 1)

  (let* ((sync (gethash "textDocumentSync" (lsp--server-capabilities)))
         (kind (if (hash-table-p sync) (gethash "change" sync) sync)))
    (setq lsp--server-sync-method (or lsp-document-sync-method
                                      (alist-get kind lsp--sync-methods))))
  (when (and lsp-auto-configure (lsp--capability "documentSymbolProvider"))
    (lsp-enable-imenu))

  (run-hooks 'lsp-after-open-hook)
  (lsp--set-document-link-timer))

(define-inline lsp--text-document-identifier ()
  "Make TextDocumentIdentifier.

interface TextDocumentIdentifier {
    uri: string;
}"
  (inline-quote (list :uri (lsp--buffer-uri))))

(defun lsp--versioned-text-document-identifier ()
  "Make VersionedTextDocumentIdentifier.

interface VersionedTextDocumentIdentifier extends TextDocumentIdentifier {
    version: number;
}"
  (plist-put (lsp--text-document-identifier) :version lsp--cur-version))

(define-inline lsp--position (line char)
  "Make a Position object for the given LINE and CHAR.

interface Position {
    line: number;
    character: number;
}"
  (inline-letevals (line char)
    (inline-quote (list :line ,line :character ,char))))

(define-inline lsp--cur-line ()
  (inline-quote (1- (line-number-at-pos))))

(define-inline lsp--cur-column ()
  (inline-quote (- (point) (line-beginning-position))))

(define-inline lsp--cur-position ()
  "Make a Position object for the current point."
  (inline-quote
   (save-restriction
     (widen)
     (lsp--position (lsp--cur-line) (lsp--cur-column)))))

(defun lsp--point-to-position (point)
  "Convert POINT to Position."
  (save-excursion
    (goto-char point)
    (lsp--cur-position)))

(define-inline lsp--range (start end)
  "Make Range body from START and END.

interface Range {
     start: Position;
     end: Position;
 }"
  ;; make sure start and end are Position objects
  (inline-letevals (start end)
    (inline-quote
     (list :start ,start :end ,end))))

(define-inline lsp--region-to-range (start end)
  "Make Range object for the current region."
  (inline-letevals (start end)
    (inline-quote (lsp--range (lsp--point-to-position ,start)
                              (lsp--point-to-position ,end)))))

(defun lsp--region-or-line ()
  "The active region or the current line."
  (if (use-region-p)
      (lsp--region-to-range (region-beginning) (region-end))
    (lsp--region-to-range (point-at-bol) (point-at-eol))))

(defun lsp--check-document-changes-version (document-changes)
  "Verify that DOCUMENT-CHANGES have the proper version."
  (unless (seq-every-p
           (lambda (it)
             (or
              (not (gethash "textDocument" it))
              (let* ((ident (gethash "textDocument" it))
                     (filename (lsp--uri-to-path (gethash "uri" ident)))
                     (version (gethash "version" ident)))
                (with-current-buffer (find-file-noselect filename)
                  (or (null version) (zerop version)
                      (equal version lsp--cur-version))))))
           document-changes)
    (error "Document changes cannot be applied")))

(defun lsp--apply-workspace-edit (edit)
  "Apply the WorkspaceEdit object EDIT."
  (if-let (document-changes (gethash "documentChanges" edit))
      (progn
        (lsp--check-document-changes-version document-changes)
        (seq-do #'lsp--apply-text-document-edit document-changes))
    (when-let (changes (gethash "changes" edit))
      (maphash
       (lambda (uri text-edits)
         (let ((filename (lsp--uri-to-path uri)))
           (with-current-buffer (find-file-noselect filename)
             (lsp--apply-text-edits text-edits))))
       changes))))

(defun lsp--apply-text-document-edit (edit)
  "Apply the TextDocumentEdit object EDIT.
If the file is not being visited by any buffer, it is opened with
`find-file-noselect'.
Because lsp-mode does not store previous document versions, the edit is only
applied if the version of the textDocument matches the version of the
corresponding file.

interface TextDocumentEdit {
  textDocument: VersionedTextDocumentIdentifier;
  edits: TextEdit[];
}"
  (pcase (gethash "kind" edit)
    ("create" (-let* (((&hash "uri" "options") edit)
                      (file-name (lsp--uri-to-path uri)))
                (f-touch file-name)
                (when (-some->> options (gethash "override"))
                  (f-write-text "" nil file-name))))
    ("delete" (-let* (((&hash "uri" "options") edit)
                      (file-name (lsp--uri-to-path uri))
                      (recursive (and options (gethash "recursive" options))))
                (f-delete file-name recursive)))
    ("rename" (-let* (((&hash "oldUri" "newUri" "options") edit)
                      (old-file-name (lsp--uri-to-path oldUri))
                      (new-file-name (lsp--uri-to-path newUri))
                      (buf (find-buffer-visiting old-file-name)))
                (when buf
                  (with-current-buffer buf
                    (save-buffer)
                    (lsp--text-document-did-close)))
                (rename-file old-file-name new-file-name (and options (gethash "override" options)))
                (when buf
                  (with-current-buffer buf
                    (set-buffer-modified-p nil)
                    (set-visited-file-name new-file-name)
                    (lsp)))))
    (_ (with-current-buffer (find-file-noselect (lsp--uri-to-path (lsp--ht-get edit "textDocument" "uri")))
         (lsp--apply-text-edits (gethash "edits" edit))))))

(defun lsp--text-edit-sort-predicate (e1 e2)
  (let ((start1 (lsp--position-to-point (gethash "start" (gethash "range" e1))))
        (start2 (lsp--position-to-point (gethash "start" (gethash "range" e2)))))
    (if (= start1 start2)
        (let ((end1 (lsp--position-to-point (gethash "end" (gethash "range" e1))))
              (end2 (lsp--position-to-point (gethash "end" (gethash "range" e2)))))
          (> end1 end2))
      (> start1 start2))))

(defun lsp--apply-text-edits (edits)
  "Apply the edits described in the TextEdit[] object in EDITS."
  ;; We sort text edits so as to apply edits that modify latter parts of the
  ;; document first. Furthermore, because the LSP spec dictates that:
  ;; "If multiple inserts have the same position, the order in the array
  ;; defines which edit to apply first."
  ;; We reverse the initial list and sort stably to make sure the order among
  ;; edits with the same position is preserved.
  (atomic-change-group
    (seq-each #'lsp--apply-text-edit
                (seq-sort #'lsp--text-edit-sort-predicate
                            (nreverse edits)))))

(defun lsp--apply-text-edit (text-edit)
  "Apply the edits described in the TextEdit object in TEXT-EDIT."
  (-let* (((&hash "newText" "range") text-edit)
          ((start . end) (lsp--range-to-region range)))
    (save-excursion
      (goto-char start)
      (delete-region start end)
      (insert newText))))

(defun lsp--capability (cap &optional capabilities)
  "Get the value of capability CAP.  If CAPABILITIES is non-nil, use them instead."
  (gethash cap (or capabilities
                   (lsp--server-capabilities)
                   (make-hash-table))))

(defun lsp--registered-capability (method)
  "Check whether there is workspace providing METHOD."
  (--first
   (seq-find (lambda (reg)
               (equal (lsp--registered-capability-method reg) method))
             (lsp--workspace-registered-server-capabilities it))
   (lsp-workspaces)))

(defvar-local lsp--before-change-vals nil
  "Store the positions from the `lsp-before-change' function
  call, for validation and use in the `lsp-on-change' function.")

(defun lsp--text-document-content-change-event (start end length)
  "Make a TextDocumentContentChangeEvent body for START to END, of length LENGTH."
  ;; So (47 54 0) means add    7 chars starting at pos 47
  ;; must become
  ;;   {"range":{"start":{"line":5,"character":6}
  ;;             ,"end" :{"line":5,"character":6}}
  ;;             ,"rangeLength":0
  ;;             ,"text":"\nbb = 5"}
  ;;
  ;; And (47 47 7) means delete 7 chars starting at pos 47
  ;; must become
  ;;   {"range":{"start":{"line":6,"character":0}
  ;;            ,"end"  :{"line":7,"character":0}}
  ;;            ,"rangeLength":7
  ;;            ,"text":""}
  ;;
  ;; (208 221 3) means delete 3 chars starting at pos 208, and replace them with
  ;; 13 chars. So it must become
  ;;   {"range":{"start":{"line":5,"character":8}
  ;;             ,"end" :{"line":5,"character":11}}
  ;;             ,"rangeLength":3
  ;;             ,"text":"new-chars-xxx"}
  ;;

  ;; Adding text:
  ;;   lsp-before-change:(start,end)=(33,33)
  ;;   lsp-on-change:(start,end,length)=(33,34,0)
  ;;
  ;; Changing text:
  ;;   lsp-before-change:(start,end)=(208,211)
  ;;   lsp-on-change:(start,end,length)=(208,221,3)
  ;;
  ;; Deleting text:
  ;;   lsp-before-change:(start,end)=(19,27)
  ;;   lsp-on-change:(start,end,length)=(19,19,8)

  (if (eq length 0)
      ;; Adding something only, work from start only
      `(:range ,(lsp--range (lsp--point-to-position start)
                            (lsp--point-to-position start))
               :rangeLength 0
               :text ,(buffer-substring-no-properties start end))

    (if (eq start end)
        ;; Deleting something only
        (if (lsp--bracketed-change-p start end length)
            ;; The before-change value is bracketed, use it
            `(:range ,(lsp--range (lsp--point-to-position start)
                                  (plist-get lsp--before-change-vals :end-pos))
                     :rangeLength ,length
                     :text "")
          ;; If the change is not bracketed, send a full change event instead.
          (lsp--full-change-event))

      ;; Deleting some things, adding others
      (if (lsp--bracketed-change-p start end length)
          ;; The before-change value is valid, use it
          `(:range ,(lsp--range (lsp--point-to-position start)
                                (plist-get lsp--before-change-vals :end-pos))
                   :rangeLength ,length
                   :text ,(buffer-substring-no-properties start end))
        (lsp--full-change-event)))))

(defun lsp--bracketed-change-p (start _end length)
  "If the before and after positions are the same, and the length
is the size of the start range, we are probably good."
  (and (eq start (plist-get lsp--before-change-vals :start))
       (eq length (- (plist-get lsp--before-change-vals :end)
                     (plist-get lsp--before-change-vals :start)))))

(defun lsp--full-change-event ()
  (save-restriction
    (widen)
    `(:text ,(buffer-substring-no-properties (point-min) (point-max)))))

(defun lsp-before-change (start end)
  "Executed before a file is changed.
Added to `before-change-functions'."
  ;; Note:
  ;;
  ;; This variable holds a list of functions to call when Emacs is about to
  ;; modify a buffer. Each function gets two arguments, the beginning and end of
  ;; the region that is about to change, represented as integers. The buffer
  ;; that is about to change is always the current buffer when the function is
  ;; called.
  ;;
  ;; WARNING:
  ;;
  ;; Do not expect the before-change hooks and the after-change hooks be called
  ;; in balanced pairs around each buffer change. Also don't expect the
  ;; before-change hooks to be called for every chunk of text Emacs is about to
  ;; delete. These hooks are provided on the assumption that Lisp programs will
  ;; use either before- or the after-change hooks, but not both, and the
  ;; boundaries of the region where the changes happen might include more than
  ;; just the actual changed text, or even lump together several changes done
  ;; piecemeal.
  ;; (message "lsp-before-change:(start,end)=(%s,%s)" start end)
  (with-demoted-errors "Error in ‘lsp-before-change’: %S"
    (setq lsp--before-change-vals
          (list :start start
                :end end
                :start-pos (lsp--point-to-position start)
                :end-pos (lsp--point-to-position end)))))

(defun lsp--flush-delayed-changes ()
  (-each (prog1 lsp--delayed-requests
           (setq lsp--delayed-requests nil))
    (-lambda ((workspace . buffer))
      (with-current-buffer buffer
        (with-lsp-workspace workspace
          (lsp-notify
           "textDocument/didChange"
           `(:textDocument
             ,(lsp--versioned-text-document-identifier)
             :contentChanges ,(vector (lsp--full-change-event)))))))))

(defun lsp-on-change (start end length)
  "Executed when a file is changed.
Added to `after-change-functions'."
  ;; Note:
  ;;
  ;; Each function receives three arguments: the beginning and end of the region
  ;; just changed, and the length of the text that existed before the change.
  ;; All three arguments are integers. The buffer that has been changed is
  ;; always the current buffer when the function is called.
  ;;
  ;; The length of the old text is the difference between the buffer positions
  ;; before and after that text as it was before the change. As for the
  ;; changed text, its length is simply the difference between the first two
  ;; arguments.
  ;;
  ;; So (47 54 0) means add    7 chars starting at pos 47
  ;; So (47 47 7) means delete 7 chars starting at pos 47
  ;; (message "lsp-on-change:(start,end,length)=(%s,%s,%s)" start end length)
  ;; (message "lsp-on-change:(lsp--before-change-vals)=%s" lsp--before-change-vals)
  (when (not revert-buffer-in-progress-p)
    (if lsp--cur-version
        (cl-incf lsp--cur-version)
      ;; buffer has been reset - start from scratch.
      (setq lsp--cur-version 0))
    (--each (lsp-workspaces)
      (with-lsp-workspace it
        (with-demoted-errors "Error in ‘lsp-on-change’: %S"
          (save-match-data
            ;; A (revert-buffer) call with the 'preserve-modes parameter (eg, as done
            ;; by auto-revert-mode) will cause this hander to get called with a nil
            ;; buffer-file-name. We need the buffer-file-name to send notifications;
            ;; so we skip handling revert-buffer-caused changes and instead handle
            ;; reverts separately in lsp-on-revert
            (pcase lsp--server-sync-method
              ('incremental (lsp-notify
                             "textDocument/didChange"
                             `(:textDocument
                               ,(lsp--versioned-text-document-identifier)
                               :contentChanges ,(vector (lsp--text-document-content-change-event
                                                         start end length)))))
              ('full
                (if lsp-debounce-full-sync-notifications
                    (progn
                      (-some-> lsp--delay-timer cancel-timer)
                      (cl-pushnew (cons lsp--cur-workspace (current-buffer))
                                  lsp--delayed-requests
                                  :test 'equal)
                      (setq lsp--delay-timer (run-with-idle-timer
                                              lsp-debounce-full-sync-notifications-interval
                                              nil
                                              (lambda ()
                                                (setq lsp--delay-timer nil)
                                                (lsp--flush-delayed-changes)))))
                  (lsp-notify
                   "textDocument/didChange"
                   `(:textDocument
                     ,(lsp--versioned-text-document-identifier)
                     :contentChanges (vector (lsp--full-change-event))))))))))))
  (lsp--set-document-link-timer)
  (when lsp-lens-mode
    (lsp--lens-schedule-refresh t)))

(defun lsp--on-self-insert ()
  "Self insert handling.
Applies on type formatting."
  (-when-let* ((provider (and lsp-enable-on-type-formatting
                              (lsp--capability "documentOnTypeFormattingProvider")))
               (ch last-command-event))
    (when (or (eq (string-to-char (gethash "firstTriggerCharacter" provider)) ch)
              (cl-find ch (gethash "moreTriggerCharacter" provider) :key #'string-to-char))
      (let ((tick (buffer-chars-modified-tick)))
        (lsp-request-async "textDocument/onTypeFormatting"
                           (append (lsp--make-document-formatting-params)
                                   `(:ch ,(char-to-string ch) :position ,(lsp--cur-position)))
                           (lambda (edits)
                             (when (= tick (buffer-chars-modified-tick)) (lsp--apply-text-edits edits))))))))

(defun lsp--set-document-link-timer ()
  (lsp--cancel-document-link-timer)
  (when (and lsp-enable-links (lsp--capability "documentLinkProvider"))
    (setq-local lsp--links-idle-timer (run-with-idle-timer
                                       lsp-links-check-internal nil
                                       #'lsp--update-document-links
                                       (current-buffer)))))

(defun lsp--cancel-document-link-timer ()
  (when lsp--links-idle-timer
    (cancel-timer lsp--links-idle-timer)
    (setq-local lsp--links-idle-timer nil)))

(defun lsp--update-document-links (&optional buffer)
  (when (or (not buffer) (eq (current-buffer) buffer))
    (cl-assert (lsp--capability "documentLinkProvider"))
    (let ((buffer (current-buffer)))
      (lsp-request-async "textDocument/documentLink"
                         `(:textDocument ,(lsp--text-document-identifier))
                         (lambda (links)
                           (seq-do (lambda (overlay)
                                     (delete-overlay overlay))
                                   lsp--link-overlays)
                           (seq-do
                            (lambda (link)
                              (with-current-buffer buffer
                                (-let* (((&hash "range") link)
                                        (start (lsp--position-to-point
                                                (gethash "start" range)))
                                        (end (lsp--position-to-point
                                              (gethash "end" range)))
                                        (button (make-button start end 'action
                                                             (lsp--document-link-keymap link))))
                                  (push button lsp--link-overlays))))
                            links))
                         :mode 'alive))
    (cancel-timer lsp--links-idle-timer)
    (setq-local lsp--links-idle-timer nil)))

(defun lsp--document-link-handle-target (url)
  (let* ((parsed-url (url-generic-parse-url (url-unhex-string url)))
         (type (url-type parsed-url))
         (file (decode-coding-string (url-filename parsed-url)
                                     locale-coding-system)))
    (pcase type
      ("file" (if (and (eq system-type 'windows-nt) (eq (elt file 0) ?\/)
                       (substring file 1))
                  (find-file (lsp--fix-path-casing
                              (concat (-some 'lsp--workspace-host-root
                                              (lsp-workspaces))
                                      file)))
                (find-file file)))
      ((or "http" "https") (browse-url url))
      (type (if-let ((handler (lsp--get-uri-handler type)))
                (funcall handler url)
              (signal 'lsp-file-scheme-not-supported (list url)))))))

(defun lsp--document-link-keymap (link)
  (-let (((&hash "target") link))
    (if target
        (lambda (_)
          (interactive)
          (lsp--document-link-handle-target target))
      (lambda (_)
        (interactive)
        (when (lsp--ht-get (lsp--capability "documentLinkProvider")
                           "resolveProvider")
          (lsp-request-async
           "documentLink/resolve"
           link
           (-lambda ((&hash "target"))
             (lsp--document-link-handle-target target))))))))

(defun lsp-buffer-language ()
  "Get language corresponding current buffer."
  (->> lsp-language-id-configuration
       (-first (-lambda ((mode-or-pattern . language))
                 (cond
                  ((and (stringp mode-or-pattern) (s-matches? mode-or-pattern buffer-file-name)) language)
                  ((eq mode-or-pattern major-mode) language))))
       cl-rest))

(defun lsp-workspace-root (&optional path)
  "Find the workspace root for the current file or PATH."
  (when-let (file-name (or path (buffer-file-name)))
    (->> (lsp-session)
         (lsp-session-folders)
         (--first (f-ancestor-of? it (f-canonical file-name))))))

(defun lsp-on-revert ()
  "Executed when a file is reverted.
Added to `after-revert-hook'."
  (let ((n (buffer-size))
        (revert-buffer-in-progress-p nil))
    (lsp-on-change 0 n n)))

(defun lsp--text-document-did-close (&optional keep-workspace-alive)
  "Executed when the file is closed, added to `kill-buffer-hook'.

If KEEP-WORKSPACE-ALIVE is non-nil, do not shutdown the workspace
if it's closing the last buffer in the workspace."
  (lsp-foreach-workspace
   (with-demoted-errors "Error on ‘lsp--text-document-did-close’: %S"
     (let ((old-buffers (lsp--workspace-buffers lsp--cur-workspace)))
       ;; remove buffer from the current workspace's list of buffers
       ;; do a sanity check first
       (when (memq (current-buffer) old-buffers)
         (setf (lsp--workspace-buffers lsp--cur-workspace)
               (delq (current-buffer) old-buffers))
         (with-demoted-errors "Error sending didClose notification in ‘lsp--text-document-did-close’: %S"
           (lsp-notify
            "textDocument/didClose"
            `(:textDocument ,(lsp--versioned-text-document-identifier))))
         (when (and (not lsp-keep-workspace-alive)
                    (not keep-workspace-alive)
                    (not (lsp--workspace-buffers lsp--cur-workspace)))
           (setf (lsp--workspace-shutdown-action lsp--cur-workspace) 'shutdown)
           (lsp--shutdown-workspace)))))))

(define-inline lsp--will-save-text-document-params (reason)
  (cl-check-type reason number)
  (inline-quote
   (list :textDocument (lsp--text-document-identifier)
         :reason ,reason)))

(defun lsp--before-save ()
  "Before save handler."
  (with-demoted-errors "Error in ‘lsp--before-save’: %S"
    (let ((params (lsp--will-save-text-document-params 1)))
      (when (lsp--send-will-save-p)
        (lsp-notify "textDocument/willSave" params))
      (when (and (lsp--send-will-save-wait-until-p) lsp-before-save-edits)
        (lsp-request-async "textDocument/willSaveWaitUntil"
                           params
                           #'lsp--apply-text-edits
                           :mode 'tick)))))

(defun lsp--on-auto-save ()
  "Handler for auto-save."
  (when (lsp--send-will-save-p)
    (with-demoted-errors "Error in ‘lsp--on-auto-save’: %S"
      (lsp-notify "textDocument/willSave" (lsp--will-save-text-document-params 2)))))

(defun lsp--text-document-did-save ()
  "Executed when the file is closed, added to `after-save-hook''."
  (with-demoted-errors "Error on ‘lsp--text-document-did-save: %S’"
    (lsp-notify "textDocument/didSave"
                `(:textDocument ,(lsp--versioned-text-document-identifier)
                                :text ,(if (lsp--save-include-text-p)
                                           (save-excursion
                                             (widen)
                                             (buffer-substring-no-properties (point-min) (point-max)))
                                         nil)))))

(define-inline lsp--text-document-position-params (&optional identifier position)
  "Make TextDocumentPositionParams for the current point in the current document.
If IDENTIFIER and POSITION are non-nil, they will be used as the document identifier
and the position respectively."
  (inline-quote (list :textDocument (or ,identifier (lsp--text-document-identifier))
                      :position (or ,position (lsp--cur-position)))))

(defun lsp-cur-line-diagnostics ()
  "Return any diagnostics that apply to the current line."
  (-let* (((&plist :start (&plist :line start) :end (&plist :line end)) (lsp--region-or-line))
          (diags-in-range (cl-remove-if-not
                           (lambda (diag)
                             (let ((line (lsp-diagnostic-line diag)))
                               (and (>= line start) (<= line end))))
                           (gethash buffer-file-name (lsp-diagnostics) nil))))
    (cl-coerce (seq-map #'lsp-diagnostic-original diags-in-range) 'vector)))

(defalias 'lsp--cur-line-diagnotics 'lsp-cur-line-diagnostics)

(defun lsp--gethash (key table &optional dflt)
  "Look up KEY in TABLE and return its associated value,
unless KEY not found or its value is falsy, when it returns DFLT.
DFLT defaults to nil.

Needed for completion request fallback behavior for the fields
'sortText', 'filterText', and 'insertText' as described here:

https://microsoft.github.io/language-server-protocol/specification#textDocument_completion"
  (let ((result (gethash key table dflt)))
    (when (member result '(nil "" 0 :json-false))
      (setq result dflt))
    result))

(defun lsp--make-completion-item (item)
  (propertize (or (gethash "insertText" item)
                  (gethash "label" item ""))
              'lsp-completion-item
              item))

(defun lsp--annotate (item)
  "Annotate ITEM detail."
  (-let (((&hash "detail" "kind" kind-index) (plist-get (text-properties-at 0 item) 'lsp-completion-item))
         kind)
    ;; We need check index before call `aref'.
    (when kind-index
      (setq kind (aref lsp--completion-item-kind kind-index))
      (concat " " detail (when kind (format " (%s)" kind))))))

(defun lsp--default-prefix-function ()
  "Default prefix function."
  (bounds-of-thing-at-point 'symbol))

(defun lsp--get-completions ()
  "Get lsp completions."
  (with-demoted-errors "Error in ‘lsp--get-completions’: %S"
    (let* ((prefix-function (or (lsp--client-prefix-function
                                 (lsp--workspace-client (cl-first (lsp-workspaces))))
                                #'lsp--default-prefix-function))
           (bounds (funcall prefix-function)))
      (list
       (if bounds (car bounds) (point))
       (if bounds (cdr bounds) (point))
       (completion-table-dynamic
        #'(lambda (_)
            ;; *we* don't need to know the string being completed
            ;; the language server does all the work by itself
            (let* ((resp (lsp-request "textDocument/completion"
                                      (lsp--text-document-position-params)))
                   (items (cond
                           ((seqp resp) resp)
                           ((hash-table-p resp) (gethash "items" resp nil)))))
              (seq-into (seq-map #'lsp--make-completion-item items) 'list))))
       :annotation-function #'lsp--annotate))))

(defun lsp--sort-completions (completions)
  "Sort COMPLETIONS."
  (--sort (let ((left (gethash "sortText" it))
                (right (gethash "sortText" other)))
            (if (string= left right)
                (string-lessp (gethash "label" it) (gethash "label" other))
              (string-lessp left right)))
          completions))

(defun lsp--resolve-completion (item)
  "Resolve completion ITEM."
  (cl-assert item nil "Completion item must not be nil")
  (or (-first 'identity
               (lsp-foreach-workspace
                (when (gethash "resolveProvider" (lsp--capability "completionProvider"))
                  (lsp-request "completionItem/resolve" item))))
      item))

(defun lsp--extract-line-from-buffer (pos)
  "Return the line pointed to by POS (a Position object) in the current buffer."
  (let* ((point (lsp--position-to-point pos))
         (inhibit-field-text-motion t))
    (save-excursion
      (goto-char point)
      (buffer-substring-no-properties (line-beginning-position)
                                      (line-end-position)))))

(defun lsp--xref-make-item (filename range)
  "Return a xref-item from a RANGE in FILENAME."
  (let* ((pos-start (gethash "start" range))
         (pos-end (gethash "end" range))
         (line (lsp--extract-line-from-buffer pos-start))
         (start (gethash "character" pos-start))
         (end (gethash "character" pos-end))
         (len (length line)))
    (add-face-text-property (max (min start len) 0)
                            (max (min end len) 0)
                            'highlight t line)
    ;; LINE is nil when FILENAME is not being current visited by any buffer.
    (xref-make (or line filename)
               (xref-make-file-location filename
                                        (1+ (gethash "line" pos-start))
                                        (gethash "character" pos-start)))))

(defun lsp--locations-to-xref-items (locations)
  "Return a list of `xref-item' from Location[] or LocationLink[]."
  (unless (seq-empty-p locations)
    (cl-labels ((get-xrefs-in-file
                 (file-locs location-link)
                 (let* ((filename (seq-first file-locs))
                        (visiting (find-buffer-visiting filename))
                        (fn (lambda (loc)
                              (lsp--xref-make-item filename
                                                   (if location-link (or (gethash "targetSelectionRange" loc)
                                                                         (gethash "targetRange" loc))
                                                     (gethash "range" loc))))))
                   (if visiting
                       (with-current-buffer visiting
                         (seq-map fn (cdr file-locs)))
                     (when (file-readable-p filename)
                       (with-temp-buffer
                         (insert-file-contents-literally filename)
                         (seq-map fn (cdr file-locs))))))))
      (apply #'append
               (if (gethash "uri" (seq-first locations))
                   (seq-map
                    (-rpartial #'get-xrefs-in-file nil)
                    (seq-group-by
                     (-compose #'lsp--uri-to-path (-partial 'gethash "uri"))
                     locations))
                 (seq-map
                  (-rpartial #'get-xrefs-in-file t)
                  (seq-group-by
                   (-compose #'lsp--uri-to-path (-partial 'gethash "targetUri"))
                   locations)))))))

(defun lsp--make-reference-params (&optional td-position include-declaration)
  "Make a ReferenceParam object.
If TD-POSITION is non-nil, use it as TextDocumentPositionParams object instead.
If INCLUDE-DECLARATION is non-nil, request the server to include declarations."
  (let ((json-false :json-false))
    (plist-put (or td-position (lsp--text-document-position-params))
               :context `(:includeDeclaration ,(or include-declaration json-false)))))

(defun lsp--cancel-request (id)
  "Cancel request with ID in all workspaces."
  (--each (lsp-workspaces)
    (with-lsp-workspace it
      (->> lsp--cur-workspace lsp--workspace-client lsp--client-response-handlers (remhash id))
      (lsp-notify "$/cancelRequest" `(:id ,id)))))

(defun lsp-eldoc-function ()
  "`lsp-mode' eldoc function."
  (run-hook-wrapped
   'lsp-eldoc-hook
   (lambda (fn)
     (condition-case nil
         (funcall fn)
       (lsp-capability-not-supported nil))
     nil))
  eldoc-last-message)

(defvar-local lsp--highlight-bounds nil)
(defvar-local lsp--highlight-timer nil)

(defun lsp--highlight ()
  (let ((buff (current-buffer)))
    (with-demoted-errors "Error in ‘lsp--highlight’: %S"
      (when (and lsp-enable-symbol-highlighting
                 (lsp--capability "documentHighlightProvider"))
        (let ((bounds (bounds-of-thing-at-point 'symbol))
              (last lsp--highlight-bounds)
              (point (point)))
          (when (and last (or (< point (car last)) (> point (cdr last))))
            (setq lsp--highlight-bounds nil)
            (--each (lsp-workspaces)
              (with-lsp-workspace it
                (lsp--remove-cur-overlays))))
          (when (and bounds (not (equal last bounds)))
            (and lsp--highlight-timer (cancel-timer lsp--highlight-timer))
            (setq lsp--highlight-timer
                  (run-with-idle-timer
                   lsp-document-highlight-delay nil
                   (lambda nil
                     (when (and (eq buff (current-buffer))
                                lsp-enable-symbol-highlighting
                                (lsp--capability "documentHighlightProvider"))
                       (setq lsp--highlight-bounds
                             (bounds-of-thing-at-point 'symbol))
                       (lsp--document-highlight)))))))))))

(defun lsp-describe-thing-at-point ()
  "Display the full documentation of the thing at point."
  (interactive)
  (let ((contents (-some->> (lsp--text-document-position-params)
                            (lsp--make-request "textDocument/hover")
                            (lsp--send-request)
                            (gethash "contents"))))
    (if (and contents (not (equal contents "")) )
        (pop-to-buffer
         (with-current-buffer (get-buffer-create "*lsp-help*")
           (let ((inhibit-read-only t))
             (erase-buffer)
             (insert (lsp--render-on-hover-content contents t))
             (goto-char (point-min))
             (view-mode t)
             (current-buffer))))
      (lsp--info "No content at point."))))

(defun lsp--point-in-bounds-p (bounds)
  "Return whether the current point is within BOUNDS."
  (and (<= (car bounds) (point)) (< (point) (cdr bounds))))

(defun lsp-get-renderer (language)
  "Get renderer for LANGUAGE."
  (lambda (str)
    (lsp--render-string str language)))

(defun lsp--setup-markdown (mode)
  "Setup the ‘markdown-mode’ in the frame.
MODE is the mode used in the parent frame."
  (make-local-variable 'markdown-code-lang-modes)
  (dolist (mark (alist-get mode lsp-custom-markup-modes))
    (add-to-list 'markdown-code-lang-modes (cons mark mode)))
  (setq-local markdown-fontify-code-blocks-natively t)
  (setq-local markdown-fontify-code-block-default-mode mode)
  (setq-local markdown-hide-markup t))

(defun lsp--buffer-string-visible ()
  "Return visible buffer string.
Stolen from `org-copy-visible'."
  (let ((result "")
        (beg (point-min))
        (end (point-max)))
    (while (/= beg end)
      (when (get-char-property beg 'invisible)
        (setq beg (next-single-char-property-change beg 'invisible nil end)))
      (let ((next (next-single-char-property-change beg 'invisible nil end)))
        (setq result (concat result (buffer-substring beg next)))
        (setq beg next)))
    (setq deactivate-mark t)
    (s-chop-suffix "\n" result)))

(defun lsp--render-markdown ()
  "Render markdown."

  (let((markdown-enable-math nil))
    (goto-char (point-min))
    (while (re-search-forward "&gt;" nil t)
      (replace-match ">"))

    (goto-char (point-min))

    (while (re-search-forward "&lt;" nil t)
      (replace-match "<"))

    (gfm-view-mode)
    (lsp--setup-markdown major-mode)))

(defun lsp--fontlock-with-mode (str mode)
  "Fontlock STR with MODE."
  (condition-case nil
      (with-temp-buffer
        (insert str)
        (delay-mode-hooks (funcall mode))
        (font-lock-ensure)
        (lsp--buffer-string-visible))
    (error str)))

(defun lsp--render-string (str language)
  "Render STR using `major-mode' corresponding to LANGUAGE.
When language is nil render as markup if `markdown-mode' is loaded."
  (if-let (mode (-some (-lambda ((mode . lang))
                         (when (and (equal lang language) (functionp mode))
                           mode))
                       lsp-language-id-configuration))
      (lsp--fontlock-with-mode str mode)
    str))

(defun lsp--render-element (content)
  "Render CONTENT element."
  ;; MarkedString
  (cond
   ((and (hash-table-p content)
         (gethash "language" content))
    (-let [(&hash "language" "value") content]
      (lsp--render-string value language)))

   ;; MarkupContent
   ((and (hash-table-p content)
         (gethash "kind" content))
    (-let [(&hash "value" "kind") content]
      (lsp--render-string value kind)))
   ;; plain string
   ((stringp content) (lsp--render-string content "markdown"))
   (t (error "Failed to handle %s" content))))

(defun lsp--render-on-hover-content (contents render-all)
  "Render the content received from 'document/onHover' request.
CONTENTS  - MarkedString | MarkedString[] | MarkupContent
RENDER-ALL - nil if only the signature should be rendered."
  (if (and (hash-table-p contents) (gethash "kind" contents))
      ;; MarkupContent, deprecated by LSP but actually very flexible.
      ;; It tends to be long and is not suitable in echo area.
      (if render-all (lsp--render-element contents) "")
    ;; MarkedString -> MarkedString[]
    (when (or (hash-table-p contents) (stringp contents))
      (setq contents (list contents)))
    ;; Consider the signature consisting of the elements who have a renderable
    ;; "language" property. When render-all is nil, ignore other elements.
    (string-join
     (seq-map
      #'lsp--render-element
      (if render-all
          contents
        ;; Only render contents that have an available renderer.
        (seq-filter
         (-andfn 'hash-table-p
                  (-compose #'lsp-get-renderer (-partial 'gethash "language")))
         contents)))
     "\n")))

(defvar-local lsp--hover-saved-bounds nil)
(defvar-local lsp--eldoc-saved-message nil)

(defun lsp--signature->eldoc-message (signature-help)
  "Generate eldoc message from SIGNATURE-HELP response."
  ;; (setq my/signature-help signature-help)
  (-when-let* (((&hash "activeSignature" active-signature-index
                       "activeParameter" active-parameter
                       "signatures") signature-help)
               (signatures (append signatures nil)))
    (->> signatures
         (-map-indexed
          (-lambda (index (signature &as &hash "label"))
            (cond
             ((= index active-signature-index)
              (-when-let* ((parameters (append (gethash "parameters" signature) nil))
                           (param (seq-elt parameters active-parameter))
                           (selected-param-label (let ((label (-some->> param (gethash "label"))))
                                                   (if (stringp label) label (append label nil))))
                           (start (if (stringp selected-param-label)
                                      (s-index-of selected-param-label label)
                                    (cl-first selected-param-label)))
                           (end (if (stringp selected-param-label)
                                    (+ start (length selected-param-label))
                                  (cl-second selected-param-label))))
                (add-face-text-property start end 'eldoc-highlight-function-argument nil label))
              label)
             (lsp-signature-render-all (propertize label 'face 'shadow)))))
         (-filter #'identity)
         (s-join "\n"))))

(defvar-local lsp-hover-request-id 0)

(defun lsp-hover ()
  "Display signature or hover info based on the current position."
  (if (and lsp--hover-saved-bounds
           (lsp--point-in-bounds-p lsp--hover-saved-bounds))
      (lsp--eldoc-message lsp--eldoc-saved-message)

    (setq lsp--hover-saved-bounds nil
          lsp--eldoc-saved-message nil)
    (let ((request-id (cl-incf lsp-hover-request-id)) (pending 0))
      (when (and lsp-eldoc-enable-hover (lsp--capability "hoverProvider"))
        (cl-incf pending)
        (lsp-request-async
         "textDocument/hover"
         (lsp--text-document-position-params)
         (lambda (hover)
           (when (and (eq request-id lsp-hover-request-id))
             (when hover
               (when-let (range (gethash "range" hover))
                 (setq lsp--hover-saved-bounds (lsp--range-to-region range)))
               (-let (((&hash "contents") hover))
                 (when-let (message
                            (and contents (lsp--render-on-hover-content contents lsp-eldoc-render-all)))
                   (when (or (and (not lsp-eldoc-prefer-signature-help) (setq pending 1))
                             (not lsp--eldoc-saved-message))
                     (setq lsp--eldoc-saved-message message)))))
             (when (zerop (cl-decf pending))
               (lsp--eldoc-message lsp--eldoc-saved-message))
             (run-hook-with-args 'lsp-on-hover-hook hover)))
         :error-handler #'ignore))
      (when (and lsp-eldoc-enable-signature-help (lsp--capability "signatureHelpProvider"))
        (cl-incf pending)
        (lsp-request-async
         "textDocument/signatureHelp"
         (lsp--text-document-position-params)
         (lambda (signature)
           (when (eq request-id lsp-hover-request-id)
             (when-let (message (and signature (lsp--signature->eldoc-message signature)))
               (when (or (and lsp-eldoc-prefer-signature-help (setq pending 1))
                         (not lsp--eldoc-saved-message))
                 (setq lsp--eldoc-saved-message message)))
             (when (zerop (cl-decf pending))
               (lsp--eldoc-message lsp--eldoc-saved-message))))
         :error-handler #'ignore)))))

(defun lsp--select-action (actions)
  "Select an action to execute from ACTIONS."
  (cond
   ((seq-empty-p actions) (user-error "No actions to select from"))
   ((and (eq (seq-length actions) 1) lsp-auto-execute-action)
    (seq-first actions))
   (t (lsp--completing-read "Select code action: "
                            (seq-into actions 'list)
                            (-lambda ((&hash "title" "command"))
                              (or title command))
                            nil t))))

(defun lsp--find-action-handler (command)
  "Find action handler for particular COMMAND."
  (--some (-some->> it
                    (lsp--workspace-client)
                    (lsp--client-action-handlers)
                    (gethash command))
          (lsp-workspaces)))

(defun lsp--text-document-code-action-params ()
  "Code action params."
  (list :textDocument (lsp--text-document-identifier)
        :range (if (use-region-p)
                   (lsp--region-to-range (region-beginning) (region-end))
                 (lsp--region-to-range (point) (point)))
        :context (list :diagnostics (lsp-cur-line-diagnostics))))

(defun lsp-code-actions-at-point ()
  "Retrieve the code actions for the active region or the current line."
  (lsp-request "textDocument/codeAction" (lsp--text-document-code-action-params)))

(defun lsp-execute-code-action-by-kind (command-kind)
  "Execute code action by name."
  (if-let (action (-first
                   (-lambda ((&hash "kind"))
                     (equal command-kind kind))
                   (lsp-get-or-calculate-code-actions)))
      (lsp-execute-code-action action)
    (user-error "No to action")))

(defalias 'lsp-get-or-calculate-code-actions 'lsp-code-actions-at-point)

(defun lsp--execute-command (action)
  "Parse and execute a code ACTION represented as a Command LSP type."
  (-if-let* ((command (gethash "command" action))
             (action-handler (lsp--find-action-handler command)))
      (funcall action-handler action)
    (lsp--send-execute-command command (gethash "arguments" action))))

(defun lsp-execute-code-action (action)
  "Execute code action ACTION.
If ACTION is not set it will be selected from `lsp-code-actions'."
  (interactive (list (lsp--select-action
                      (lsp-code-actions-at-point))))
  (when-let ((edit (gethash "edit" action)))
    (lsp--apply-workspace-edit edit))

  (let ((command (gethash "command" action)))
    (cond
     ((stringp command) (lsp--execute-command action))
     ((hash-table-p command) (lsp--execute-command command)))))

(defun lsp--make-document-formatting-params ()
  "Create document formatting params."
  `(:textDocument ,(lsp--text-document-identifier)
                  :options (:tabSize ,tab-width :insertSpaces
                                     ,(if indent-tabs-mode :json-false t))))

(defun lsp-format-buffer ()
  "Ask the server to format this document."
  (interactive "*")
  (unless (or (lsp--capability "documentFormattingProvider")
              (lsp--registered-capability "textDocument/formatting"))
    (signal 'lsp-capability-not-supported (list "documentFormattingProvider")))
  (let ((edits (lsp-request "textDocument/formatting"
                            (lsp--make-document-formatting-params))))
    (lsp--apply-formatting edits)))

(defun lsp-format-region (s e)
  "Ask the server to format the region, or if none is selected, the current line."
  (interactive "r")
  (unless (or (lsp--capability "documentFormattingProvider")
              (lsp--capability "documentRangeFormattingProvider")
              (lsp--registered-capability "textDocument/rangeFormatting"))
    (signal 'lsp-capability-not-supported (list "documentFormattingProvider")))
  (let ((edits (lsp-request "textDocument/rangeFormatting"
                            (lsp--make-document-range-formatting-params s e))))
    (lsp--apply-formatting edits)))

(defun lsp-organize-imports ()
  "Perform the source.organizeImports code action."
  (interactive)
  (lsp-execute-code-action-by-kind "source.organizeImports"))

(defun lsp--apply-formatting (edits)
  (if (fboundp 'replace-buffer-contents)
      (let ((current-buffer (current-buffer)))
        (with-temp-buffer
          (insert-buffer-substring-no-properties current-buffer)
          (lsp--apply-text-edits edits)
          (let ((temp-buffer (current-buffer)))
            (with-current-buffer current-buffer
              (replace-buffer-contents temp-buffer)))))
    (let ((point (point))
          (w-start (window-start)))
      (lsp--apply-text-edits edits)
      (goto-char point)
      (goto-char (line-beginning-position))
      (set-window-start (selected-window) w-start))))

(defun lsp--make-document-range-formatting-params (start end)
  "Make DocumentRangeFormattingParams for selected region.
interface DocumentRangeFormattingParams {
    textDocument: TextDocumentIdentifier;
    range: Range;
    options: FormattingOptions;
}"
  (plist-put (lsp--make-document-formatting-params)
             :range (lsp--region-to-range start end)))

(defconst lsp--highlight-kind-face
  '((1 . lsp-face-highlight-textual)
    (2 . lsp-face-highlight-read)
    (3 . lsp-face-highlight-write)))

(defun lsp--remove-cur-overlays ()
  (let ((overlays (lsp--workspace-highlight-overlays lsp--cur-workspace))
        (buf (current-buffer)))
    (dolist (overlay (gethash buf overlays))
      (delete-overlay overlay))
    (remhash buf overlays)))

(defun lsp--document-highlight ()
  (lsp-request-async "textDocument/documentHighlight"
                     (lsp--text-document-position-params)
                     (lsp--make-document-highlight-callback (current-buffer))))

(defun lsp-document-highlight ()
  "Highlight all relevant references to the symbol under point."
  (interactive)
  (unless (lsp--capability "documentHighlightProvider")
    (signal 'lsp-capability-not-supported (list "documentHighlightProvider")))
  (lsp--document-highlight))

(defun lsp--make-document-highlight-callback (buf)
  "Create a callback to process the reply of a
'textDocument/documentHightlight' message for the buffer BUF.
A reference is highlighted only if it is visible in a window."
  (cl-check-type buf buffer)
  (lambda (highlights)
    (with-current-buffer buf
      (lsp--remove-cur-overlays)
      (when (/= (seq-length highlights) 0)
        (let* ((windows-on-buffer (get-buffer-window-list nil nil 'visible))
               (overlays (lsp--workspace-highlight-overlays lsp--cur-workspace))
               (buf-overlays (gethash (current-buffer) overlays))
               wins-visible-pos)
          (save-restriction
            (widen)
            ;; Save visible portions of the buffer
            (dolist (win windows-on-buffer)
              (let* ((win-start (window-start win))
                     (win-end (window-end win)))
                (push (cons (1- (line-number-at-pos win-start))
                            (1+ (line-number-at-pos win-end)))
                      wins-visible-pos)))
            (seq-doseq (highlight highlights)
              (let* ((range (gethash "range" highlight nil))
                     (kind (gethash "kind" highlight 1))
                     (start (gethash "start" range))
                     (end (gethash "end" range))
                     overlay)
                (dolist (win wins-visible-pos)
                  (let* ((start-window (car win))
                         (end-window (cdr win)))
                    ;; Make the overlay only if the reference is visible
                    (when (and (> (1+ (gethash "line" start)) start-window)
                               (< (1+ (gethash "line" end)) end-window)
                               (not (and lsp-symbol-highlighting-skip-current
                                         (< (lsp--position-to-point start)
                                            (point)
                                            (lsp--position-to-point end)))))
                      (setq overlay (make-overlay (lsp--position-to-point start)
                                                  (lsp--position-to-point end)))
                      (overlay-put overlay 'face
                                   (cdr (assq kind lsp--highlight-kind-face)))
                      (push overlay buf-overlays)
                      (puthash (current-buffer) buf-overlays overlays))))))))))))

(defconst lsp--symbol-kind
  '((1 . "File")
    (2 . "Module")
    (3 . "Namespace")
    (4 . "Package")
    (5 . "Class")
    (6 . "Method")
    (7 . "Property")
    (8 . "Field")
    (9 . "Constructor")
    (10 . "Enum")
    (11 . "Interface")
    (12 . "Function")
    (13 . "Variable")
    (14 . "Constant")
    (15 . "String")
    (16 . "Number")
    (17 . "Boolean")
    (18 . "Array")
    (19 . "Object")
    (20 . "Key")
    (21 . "Null")
    (22 . "Enum Member")
    (23 . "Struct")
    (24 . "Event")
    (25 . "Operator")
    (26 . "Type Parameter")))

(defun lsp--symbol-information-to-xref (symbol)
  "Return a `xref-item' from SYMBOL information."
  (let* ((location (gethash "location" symbol))
         (uri (gethash "uri" location))
         (range (gethash "range" location))
         (start (gethash "start" range)))
    (xref-make (format "[%s] %s"
                       (alist-get (gethash "kind" symbol) lsp--symbol-kind)
                       (gethash "name" symbol))
               (xref-make-file-location (lsp--uri-to-path uri)
                                        (1+ (gethash "line" start))
                                        (gethash "character" start)))))

(defun lsp--location-to-td-position (location)
  "Convert LOCATION to a TextDocumentPositionParams object."
  `(:textDocument (:uri ,(gethash "uri" location))
                  :position ,(gethash "start" (gethash "range" location))))

(defun lsp--symbol-info-to-identifier (symbol)
  (let ((td-params (lsp--location-to-td-position (gethash "location" symbol))))
    (propertize (gethash "name" symbol)
                'ref-params (lsp--make-reference-params td-params)
                'def-params td-params)))

(defun lsp--get-document-symbols ()
  "Get document symbols.

If the buffer has not been modified since symbols were last
retrieved, simply return the latest result.

Else, if the request was initiated by Imenu updating its menu-bar
entry, perform it asynchronously; i.e., give Imenu the latest
result and then force a refresh when a new one is available.

Else (e.g., due to intereactive use of `imenu' or `xref'),
perform the request synchronously."
  (if (= (buffer-chars-modified-tick) lsp--document-symbols-tick)
      lsp--document-symbols
    (let ((method "textDocument/documentSymbol")
          (params `(:textDocument ,(lsp--text-document-identifier)))
          (tick (buffer-chars-modified-tick)))
      (if (not lsp--document-symbols-request-async)
          (prog1
              (setq lsp--document-symbols (lsp-request method params))
            (setq lsp--document-symbols-tick tick))
        (lsp-request-async method params
                           (lambda (document-symbols)
                             (setq lsp--document-symbols document-symbols
                                   lsp--document-symbols-tick tick)
                             (lsp--imenu-refresh))
                           :mode 'alive)
        lsp--document-symbols))))

(advice-add 'imenu-update-menubar :around
             (lambda (oldfun &rest r)
               (let ((lsp--document-symbols-request-async t))
                 (apply oldfun r))))

(defun lsp--xref-backend () 'xref-lsp)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql xref-lsp)))
  (propertize (or (thing-at-point 'symbol) "")
              'def-params (lsp--text-document-position-params)
              'ref-params (lsp--make-reference-params)))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql xref-lsp)))
  (let ((json-false :json-false)
        (symbols (lsp--get-document-symbols)))
    (seq-map #'lsp--symbol-info-to-identifier (seq-remove 'null symbols))))

(cl-defmethod xref-backend-definitions ((_backend (eql xref-lsp)) identifier)
  (let* ((maybeparams (get-text-property 0 'def-params identifier))
         ;; In some modes (such as haskell-mode), xref-find-definitions gets
         ;; called directly without applying the properties expected here. So we
         ;; must test if the properties are present, and if not use the current
         ;; point location.
         (params (if (null maybeparams)
                     (lsp--text-document-position-params)
                   maybeparams))
         (defs (lsp--send-request (lsp--make-request
                                   "textDocument/definition"
                                   params))))
    (lsp--locations-to-xref-items (if (sequencep defs) defs (list defs)))))

(cl-defmethod xref-backend-references ((_backend (eql xref-lsp)) identifier)
  (let* ((properties (text-properties-at 0 identifier))
         (params (plist-get properties 'ref-params))
         (refs (lsp-request "textDocument/references"
                            (or params (lsp--make-reference-params)))))
    (lsp--locations-to-xref-items refs)))

(cl-defmethod xref-backend-apropos ((_backend (eql xref-lsp)) pattern)
  (seq-map #'lsp--symbol-information-to-xref
             (lsp-request "workspace/symbol" `(:query ,pattern))))

(defun lsp--get-symbol-to-rename ()
  "Get synbol at point."
  (if (let ((table (lsp--capability "renameProvider")))
        (and (hash-table-p table)
             (gethash "prepareProvider" table)))
      (-let (((start . end) (lsp--range-to-region
                             (lsp-request "textDocument/prepareRename"
                                          (lsp--text-document-position-params)))))
        (buffer-substring-no-properties start end))
    (thing-at-point 'symbol t)))

(defun lsp-rename (newname)
  "Rename the symbol (and all references to it) under point to NEWNAME."
  (interactive (list (let ((symbol (lsp--get-symbol-to-rename)))
                       (read-string (format "Rename %s to: " symbol) symbol))))
  (lsp--cur-workspace-check)
  (unless (lsp--capability "renameProvider")
    (signal 'lsp-capability-not-supported (list "renameProvider")))
  (let ((edits (lsp-request "textDocument/rename"
                            `(:textDocument ,(lsp--text-document-identifier)
                                            :position ,(lsp--cur-position)
                                            :newName ,newname))))
    (when edits
      (lsp--apply-workspace-edit edits))))

(cl-defun lsp-find-locations (method &optional extra &key display-action)
  "Send request named METHOD and get cross references of the symbol under point.
EXTRA is a plist of extra parameters."
  (let ((loc (lsp-request method
                          (append (lsp--text-document-position-params) extra))))
    (if loc
        (xref--show-xrefs
         (lsp--locations-to-xref-items (if (sequencep loc) loc (list loc)))
         display-action)
      (message "Not found for: %s" (thing-at-point 'symbol t)))))

(cl-defun lsp-find-declaration (&key display-action)
  "Find declarations of the symbol under point."
  (interactive)
  (lsp-find-locations "textDocument/declaration" nil :display-action display-action))

(cl-defun lsp-find-definition (&key display-action)
  "Find definitions of the symbol under point."
  (interactive)
  (unless (lsp--capability "definitionProvider")
    (signal 'lsp-capability-not-supported (list "definitionProvider")))
  (lsp-find-locations "textDocument/definition" nil :display-action display-action))

(cl-defun lsp-find-implementation (&key display-action)
  "Find implementations of the symbol under point."
  (interactive)
  (unless (lsp--capability "implementationProvider")
    (signal 'lsp-capability-not-supported (list "implementationProvider")))
  (lsp-find-locations "textDocument/implementation" nil :display-action display-action))

(cl-defun lsp-find-references (&optional include-declaration &key display-action)
  "Find references of the symbol under point."
  (interactive)
  (unless (lsp--capability "referencesProvider")
    (signal 'lsp-capability-not-supported (list "referencesProvider")))
  (lsp-find-locations "textDocument/references"
                      (list :context `(:includeDeclaration ,(or include-declaration json-false)))
                      :display-action display-action))

(cl-defun lsp-find-type-definition (&key display-action)
  "Find type definitions of the symbol under point."
  (interactive)
  (unless (lsp--capability "typeDefinitionProvider")
    (signal 'lsp-capability-not-supported (list "typeDefinitionProvider")))
  (lsp-find-locations "textDocument/typeDefinition" nil :display-action display-action))

(defalias 'lsp-find-custom #'lsp-find-locations)
(defalias 'lsp-goto-implementation #'lsp-find-implementation)
(defalias 'lsp-goto-type-definition #'lsp-find-type-definition)

(with-eval-after-load 'evil
  (evil-set-command-property 'lsp-find-definition :jump t)
  (evil-set-command-property 'lsp-find-implementation :jump t)
  (evil-set-command-property 'lsp-find-references :jump t)
  (evil-set-command-property 'lsp-find-type-definition :jump t))

(defun lsp--find-workspaces-for (msg)
  "Find all workspaces in the current that can handle MSG."
  (-if-let (reqs (cdr (assoc (plist-get msg :method) lsp-method-requirements)))
      (-let (((&plist :capability :registered-capability :check-command) reqs))
        (--filter
         (with-lsp-workspace it
           (or
            (when check-command (funcall check-command it))
            (when capability (lsp--capability capability))
            (when registered-capability
              (lsp--registered-capability registered-capability))
            (and (not capability)
                 (not registered-capability)
                 (not check-command))))
         (lsp-workspaces)))
    (lsp-workspaces)))

(cl-defmethod lsp-execute-command (_server command arguments)
  "Execute COMMAND on SERVER with `workspace/executeCommand'."
  (lsp-request "workspace/executeCommand"
               `(:command ,(format "%s" command) :arguments ,arguments)))

(defun lsp--send-execute-command (command &optional args)
  "Create and send a 'workspace/executeCommand' message having command COMMAND and optional ARGS."
  (let ((params (if args
                    (list :command command :arguments args)
                  (list :command command))))
    (lsp-request "workspace/executeCommand" params)))

(defalias 'lsp-point-to-position #'lsp--point-to-position)
(defalias 'lsp-text-document-identifier #'lsp--text-document-identifier)
(defalias 'lsp-send-execute-command #'lsp--send-execute-command)
(defalias 'lsp-on-open #'lsp--text-document-did-open)
(defalias 'lsp-on-save #'lsp--text-document-did-save)
(defalias 'lsp-completion-at-point #'lsp--get-completions)

(defun lsp--set-configuration (settings)
  "Set the SETTINGS for the lsp server."
  (lsp-notify "workspace/didChangeConfiguration" `(:settings , settings)))

(defun lsp--on-set-visitied-file-name (old-func &rest args)
  "Advice around function `set-visited-file-name'.

This advice sends textDocument/didClose for the old file and
textDocument/didOpen for the new file."
  (when lsp--cur-workspace
    (lsp--text-document-did-close t))
  (prog1 (apply old-func args)
    (when lsp--cur-workspace
      (lsp--text-document-did-open))))

(advice-add 'set-visited-file-name :around #'lsp--on-set-visitied-file-name)

(defun lsp--send-no-wait (message proc)
  "Send MESSAGE to PROC without waiting for further output."
  (when (memq (process-status proc) '(stop exit closed failed nil))
    (error "%s: Cannot communicate with the process (%s)" (process-name proc)
           (process-status proc)))
  (process-send-string proc message))

(define-error 'lsp-parse-error
  "Error parsing message from language server" 'lsp-error)
(define-error 'lsp-unknown-message-type
  "Unknown message type" '(lsp-error lsp-parse-error))
(define-error 'lsp-unknown-json-rpc-version
  "Unknown JSON-RPC protocol version" '(lsp-error lsp-parse-error))
(define-error 'lsp-no-content-length
  "Content-Length header missing in message" '(lsp-error lsp-parse-error))
(define-error 'lsp-invalid-header-name
  "Invalid header name" '(lsp-error lsp-parse-error))

;;  id  method
;;   x    x     request
;;   x    .     response
;;   .    x     notification
(defun lsp--get-message-type (json-data)
  "Get the message type from JSON-DATA."
  (when (not (string= (gethash "jsonrpc" json-data "") "2.0"))
    (signal 'lsp-unknown-json-rpc-version (list (gethash "jsonrpc" json-data))))
  (if (gethash "id" json-data nil)
      (if (gethash "error" json-data nil)
          'response-error
        (if (gethash "method" json-data nil)
            'request
          'response))
    (if (gethash "method" json-data nil)
        'notification
      (signal 'lsp-unknown-message-type (list json-data)))))

(defconst lsp--default-notification-handlers
  (ht ("window/showMessage" #'lsp--window-show-message)
      ("window/logMessage" #'lsp--window-log-message)
      ("textDocument/publishDiagnostics" #'lsp--on-diagnostics)
      ("textDocument/diagnosticsEnd" #'ignore)
      ("textDocument/diagnosticsBegin" #'ignore)
      ("telemetry/event" #'ignore)))

(defun lsp--on-notification (workspace notification)
  "Call the appropriate handler for NOTIFICATION."
  (-let (((&hash "params" "method") notification)
         (client (lsp--workspace-client workspace)))
    (when lsp-print-io
      (lsp--log-entry-new (lsp--make-log-entry method nil params 'incoming-notif)
                          lsp--cur-workspace))
    (if-let (handler (or (gethash method (lsp--client-notification-handlers client))
                         (gethash method lsp--default-notification-handlers)))
        (funcall handler workspace params)
      (unless (string-prefix-p "$" method)
        (lsp-warn "Unknown method: %s" method)))))

(defun lsp--build-workspace-configuration-response (params)
  "Get section configuration.
PARAMS are the `workspace/configuration' request params"
  (-some->> params
            (gethash "items")
            (-map (-lambda ((&hash "section"))
                    (gethash section (lsp-configuration-section section))))
            (apply #'vector)))

(defun lsp--on-request (workspace request)
  "Call the appropriate handler for REQUEST, and send the return value to the server.
WORKSPACE is the active workspace."
  (-let* ((recv-time (current-time))
          ((&hash "params" "method" "id") request)
          (req-entry (and lsp-print-io
                          (lsp--make-log-entry method id params 'incoming-req)))
          (client (lsp--workspace-client workspace))
          (process (lsp--workspace-proc workspace))
          (empty-response (lsp--make-response request nil))
          (response (pcase method
                      ("client/registerCapability"
                       (seq-doseq (reg (gethash "registrations" params))
                         (lsp--server-register-capability reg))
                       empty-response)
                      ("window/showMessageRequest"
                       (let ((choice (lsp--window-log-message-request params)))
                         (lsp--make-response request `(:title ,choice))))
                      ("client/unregisterCapability"
                       (seq-doseq (unreg (gethash "unregisterations" params))
                         (lsp--server-unregister-capability unreg))
                       empty-response)
                      ("workspace/applyEdit"
                       (lsp--apply-workspace-edit (gethash "edit" params))
                       empty-response)
                      ("workspace/configuration"
                       (lsp--make-response request (lsp--build-workspace-configuration-response params)))
                      (other
                       (-if-let (handler (gethash other (lsp--client-request-handlers client) nil))
                           (lsp--make-response request (funcall handler workspace params))
                         (lsp-warn "Unknown request method: %s" other)
                         empty-response))))
          (resp-entry (and lsp-print-io
                           (lsp--make-log-entry method id response 'outgoing-resp
                                                (/ (nth 2 (time-since recv-time)) 1000)))))
    ;; Send response to the server.
    (when lsp-print-io
      (lsp--log-entry-new req-entry workspace)
      (lsp--log-entry-new resp-entry workspace))
    (lsp--send-no-wait (lsp--make-message response) process)))

(defun lsp--error-string (err)
  "Format ERR as a user friendly string."
  (let ((code (gethash "code" err))
        (message (gethash "message" err)))
    (format "Error from the Language Server: %s (%s)"
            message
            (or (car (alist-get code lsp--errors)) "Unknown error"))))

(defun lsp--get-body-length (headers)
  (let ((content-length (cdr (assoc "Content-Length" headers))))
    (if content-length
        (string-to-number content-length)

      ;; This usually means either the server our our parser is
      ;; screwed up with a previous Content-Length
      (error "No Content-Length header"))))

(defun lsp--parse-header (s)
  "Parse string S as a LSP (KEY . VAL) header."
  (let ((pos (string-match "\:" s))
        key val)
    (unless pos
      (signal 'lsp-invalid-header-name (list s)))
    (setq key (substring s 0 pos)
          val (s-trim-left (substring s (+ 1 pos))))
    (when (string-equal key "Content-Length")
      (cl-assert (cl-loop for c across val
                          when (or (> c ?9) (< c ?0)) return nil
                          finally return t)
                 nil (format "Invalid Content-Length value: %s" val)))
    (cons key val)))

(defun lsp--parser-reset (p)
  "Reset parser P."
  (setf
   (lsp--parser-leftovers p) ""
   (lsp--parser-body-length p) nil
   (lsp--parser-body-received p) nil
   (lsp--parser-headers p) '()
   (lsp--parser-body p) nil
   (lsp--parser-reading-body p) nil))

(defun lsp--read-json (str)
  "Read json string STR."
  (let* ((use-native-json (and lsp-use-native-json (fboundp 'json-parse-string)))
         (json-array-type (if lsp-json-use-lists 'list 'vector))
         (json-object-type 'hash-table)
         (json-false nil))
    (if use-native-json
        (with-no-warnings
          (with-temp-buffer
            (json-parse-string str
                               :object-type 'hash-table
                               :null-object nil
                               :false-object nil)))
      (json-read-from-string str))))

(defun lsp--log-request-time (server-id method id start-time before-send received-time after-parsed-time after-processed-time)
  (when lsp-print-performance
    (lsp-log "Perf> Request/Response
  ServerId: %s
  Request: %s (%s)
  Serialization took: %.06f
  ServerTime: %.06f
  Deserialization: %.06f
  CallbackTime: %s"
             server-id
             method
             id
             (float-time (time-subtract before-send start-time))
             (float-time (time-subtract received-time before-send))
             (float-time (time-subtract after-parsed-time received-time))
             (if after-processed-time
                 (format "%.06f" (float-time (time-subtract after-processed-time after-parsed-time)))
               "N/A"))))

(defun log--notification-performance (server-id json-data received-time after-parsed-time before-notification after-processed-time)
  (when lsp-print-performance
    (lsp-log "Perf> notification
  ServerId: %s
  Notification: %s
  Deserialization: %.06f
  Processing: %.06f "
             server-id
             (when json-data (gethash "method" json-data))
             (float-time (time-subtract after-parsed-time received-time))
             (float-time (time-subtract after-processed-time before-notification)))))

(defun lsp--parser-on-message (p msg)
  "Called when the parser P read a complete MSG from the server."
  (with-lsp-workspace (lsp--parser-workspace p)
    (let* ((client (lsp--workspace-client lsp--cur-workspace))
           (received-time (current-time))
           (server-id (lsp--client-server-id client))
           (json-data (lsp--read-json msg))
           (after-parsed-time (current-time))
           (id (--when-let (gethash "id" json-data)
                 (if (stringp it) (string-to-number it) it)))
           (data (gethash "result" json-data))
           after-processed-time)
      (pcase (lsp--get-message-type json-data)
        ('response
          (cl-assert id)
          (-let [(callback _ method start-time before-send) (gethash id (lsp--client-response-handlers client))]
            (when lsp-print-io
              (lsp--log-entry-new
               (lsp--make-log-entry method id data 'incoming-resp
                                    (/ (nth 2 (time-since before-send)) 1000))
               lsp--cur-workspace))
            (when callback
              (funcall callback (gethash "result" json-data))
              (remhash id (lsp--client-response-handlers client))
              (setq after-processed-time (current-time))
              (lsp--log-request-time server-id method id start-time before-send
                                     received-time after-parsed-time after-processed-time))))
        ('response-error
          (cl-assert id)
          (-let [(_ callback method start-time before-send) (gethash id (lsp--client-response-handlers client))]
            (when callback
              (funcall callback (gethash "error" json-data))
              (remhash id (lsp--client-response-handlers client))
              (setq after-processed-time (current-time))
              (lsp--log-request-time server-id method id start-time before-send
                                     received-time after-parsed-time after-processed-time))))
        ('notification
          (let ((before-notification (current-time)))
            (lsp--on-notification lsp--cur-workspace json-data)
            (log--notification-performance
             server-id json-data received-time after-parsed-time before-notification (current-time))))
        ('request (lsp--on-request lsp--cur-workspace json-data))))))

(defun lsp--parser-read (p output)
  "Handle OUTPUT using parser P."
  (let* ((messages '())
         (output (with-no-warnings (string-as-unibyte output)))
         (chunk (concat (lsp--parser-leftovers p) output)))
    (while (not (string-empty-p chunk))
      (if (not (lsp--parser-reading-body p))
          ;; Read headers
          (let* ((body-sep-pos (string-match-p "\r\n\r\n" chunk)))
            (if body-sep-pos
                ;; We've got all the headers, handle them all at once:
                (let* ((header-raw (substring chunk 0 body-sep-pos))
                       (content (substring chunk (+ body-sep-pos 4)))
                       (headers
                        (mapcar 'lsp--parse-header
                                 (split-string header-raw "\r\n")))
                       (body-length (lsp--get-body-length headers)))
                  (setf
                   (lsp--parser-headers p) headers
                   (lsp--parser-reading-body p) t
                   (lsp--parser-body-length p) body-length
                   (lsp--parser-body-received p) 0
                   (lsp--parser-body p) (make-string body-length ?\0)
                   (lsp--parser-leftovers p) nil)
                  (setq chunk content))

              ;; Haven't found the end of the headers yet. Save everything
              ;; for when the next chunk arrives and await further input.
              (setf (lsp--parser-leftovers p) chunk)
              (setq chunk "")))

        ;; Read body
        (let* ((total-body-length (lsp--parser-body-length p))
               (received-body-length (lsp--parser-body-received p))
               (chunk-length (string-bytes chunk))
               (left-to-receive (- total-body-length received-body-length))
               (this-body
                (substring chunk 0 (min left-to-receive chunk-length)))
               (leftovers (substring chunk (string-bytes this-body))))
          (store-substring (lsp--parser-body p) received-body-length this-body)
          (setf (lsp--parser-body-received p) (+ (lsp--parser-body-received p)
                                                 (string-bytes this-body)))
          (when (>= chunk-length left-to-receive)
            ;; TODO: keep track of the Content-Type header, if
            ;; present, and use its value instead of just defaulting
            ;; to utf-8
            (push (decode-coding-string (lsp--parser-body p) 'utf-8) messages)
            (lsp--parser-reset p))

          (setq chunk leftovers))))
    (nreverse messages)))

(defun lsp--json-pretty-print (msg)
  "Convert json MSG string to pretty printed json string."
  (let ((json-encoding-pretty-print t))
    (json-encode (json-read-from-string msg))))

(defun lsp--parser-make-filter (p ignore-regexps)
  "Make filter for the lsp parser P ignoring IGNORE-REGEXPS."
  #'(lambda (_proc output)
      (when (cl-loop for r in ignore-regexps
                     ;; check if the output is to be ignored or not
                     ;; TODO: Would this ever result in false positives?
                     when (string-match r output) return nil
                     finally return t)
        (-when-let (messages (condition-case err
                                 (lsp--parser-read p output)
                               (error
                                (let ((chunk (concat (lsp--parser-leftovers p) output)))
                                  (lsp--parser-reset p)
                                  (ignore (lsp-warn "Failed to parse the following chunk:\n'''\n%s\n'''\nwith message %s" chunk err))))))
          (dolist (m messages)
            (lsp--parser-on-message p m))))))

(defun lsp--symbol-to-imenu-elem (sym)
  "Convert SYM to imenu element.

SYM is a SymbolInformation message.

Return a cons cell (full-name . start-point)."
  (let* ((start-point (lsp--symbol-get-start-point sym))
         (name (gethash "name" sym))
         (container (gethash "containerName" sym)))
    (cons (if (and lsp-imenu-show-container-name container)
              (concat container lsp-imenu-container-name-separator name)
            name)
          start-point)))

(defun lsp--symbol-to-hierarchical-imenu-elem (sym)
  "Convert SYM to hierarchical imenu elements.

SYM is a DocumentSymbol message.

Return cons cell (\"symbol-name (symbol-kind)\" . start-point) if
SYM doesn't have any children. Otherwise return a cons cell with
an alist

  (\"symbol-name\" . ((\"(symbol-kind)\" . start-point)
                    cons-cells-from-children))"
  (let* ((start-point (lsp--symbol-get-start-point sym))
         (name (gethash "name" sym)))
    (if (seq-empty-p (gethash "children" sym))
        (cons (format "%s (%s)" name (lsp--get-symbol-type sym)) start-point)
      (cons name
            (cons (cons (format "(%s)" (lsp--get-symbol-type sym)) start-point)
                  (lsp--imenu-create-hierarchical-index (gethash "children" sym)))))))

(defun lsp--symbol-get-start-point (sym)
  "Get the start point of the name of SYM.

SYM can be either DocumentSymbol or SymbolInformation."
  (let* ((location (gethash "location" sym))
         (name-range (or (and location (gethash "range" location))
                         (gethash "selectionRange" sym)))
         (start-point (lsp--position-to-point
                       (gethash "start" name-range))))
    (if imenu-use-markers
        (save-excursion (goto-char start-point) (point-marker))
      start-point)))

(defun lsp--symbol-filter (sym)
  "Determine if SYM is for the current document."
  ;; It's a SymbolInformation or DocumentSymbol, which is always in the current
  ;; buffer file.
  (-if-let ((&hash "location") sym)
      (not (eq (->> location
                    (gethash "uri")
                    (lsp--uri-to-path)
                    (find-buffer-visiting))
               (current-buffer)))))

(defun lsp--get-symbol-type (sym)
  "The string name of the kind of SYM."
  (-> (gethash "kind" sym)
      (assoc lsp--symbol-kind)
      (cdr)

      (or "Other")))

(defun lsp--imenu-create-index ()
  "Create imenu index from document symbols."
  (let ((symbols (lsp--get-document-symbols)))
    (if (lsp--imenu-hierarchical-p symbols)
        (lsp--imenu-create-hierarchical-index symbols)
      (seq-map (lambda (nested-alist)
                 (cons (car nested-alist)
                       (seq-map #'lsp--symbol-to-imenu-elem (cdr nested-alist))))
               (seq-group-by #'lsp--get-symbol-type (lsp--imenu-filter-symbols symbols))))))

(defun lsp--imenu-filter-symbols (symbols)
  "Filter out unsupported symbols from SYMBOLS."
  (seq-remove #'lsp--symbol-filter symbols))

(defun lsp--imenu-hierarchical-p (symbols)
  "Determine whether any element in SYMBOLS has children."
  (seq-some (-partial 'gethash "children") symbols))

(defun lsp--imenu-create-hierarchical-index (symbols)
  "Create imenu index for hierarchical SYMBOLS.

SYMBOLS are a list of DocumentSymbol messages.

Return a nested alist keyed by symbol names. e.g.

   ((\"SomeClass\" (\"(Class)\" . 10)
                 (\"someField (Field)\" . 20)
                 (\"someFunction (Function)\" . 25)
                 (\"SomeSubClass\" (\"(Class)\" . 30)
                                  (\"someSubField (Field)\" . 35))
    (\"someFunction (Function)\" . 40))"
  (let ((symbols (lsp--imenu-filter-symbols symbols)))
    (seq-map #'lsp--symbol-to-hierarchical-imenu-elem
               (seq-sort #'lsp--imenu-symbol-lessp
                           (lsp--imenu-filter-symbols symbols)))))

(defun lsp--imenu-symbol-lessp (sym1 sym2)
  (let* ((compare-results (mapcar (lambda (method)
                                    (funcall (alist-get method lsp--imenu-compare-function-alist)
                                             sym1 sym2))
                                  lsp-imenu-sort-methods))
         (result (seq-find (lambda (result)
                             (not (= result 0)))
                           compare-results
                           0)))
    (and (numberp result) (< result 0))))

(defun lsp--imenu-compare-kind (sym1 sym2)
  "Compare SYM1 and SYM2 by kind."
  (let ((kind1 (gethash "kind" sym1))
        (kind2 (gethash "kind" sym2)))
    (- kind1 kind2)))

(defun lsp--imenu-compare-position (sym1 sym2)
  "Compare SYM1 and SYM2 by position."
  (let ((position1 (lsp--symbol-get-start-point sym1))
        (position2 (lsp--symbol-get-start-point sym2)))
    (- position1 position2)))

(defun lsp--imenu-compare-name (sym1 sym2)
  "Compare SYM1 and SYM2 by name."
  (let* ((name1 (gethash "name" sym1))
         (name2 (gethash "name" sym2))
         (result (compare-strings name1 0 (length name1) name2 0 (length name2))))
    (if (numberp result) result 0)))

(defun lsp--imenu-refresh ()
  "Force Imenu to refresh itself."
  (imenu--menubar-select imenu--rescan-item))

(defun lsp-enable-imenu ()
  "Use lsp-imenu for the current buffer."
  (setq-local imenu-create-index-function #'lsp--imenu-create-index)
  (lsp--imenu-refresh))

(defun lsp-resolve-final-function (command)
  "Resolve final function COMMAND."
  (-let [command (if (functionp command) (funcall command) command)]
    (if (consp command) command (list command))))

(defun lsp-server-present? (final-command)
  "Check whether FINAL-COMMAND is present."
  ;; executable-find only gained support for remote checks after 26.1 release
  (or (and (cond
            ((not (file-remote-p default-directory))
             (executable-find (cl-first final-command)))
            ((not (version<= emacs-version "26.2"))
             (with-no-warnings (executable-find (cl-first final-command) (file-remote-p default-directory))))
            (t))
           (prog1 t
             (lsp-log "Command \"%s\" is present on the path." (s-join " " final-command))))
      (ignore (lsp-log "Command \"%s\" is not present on the path." (s-join " " final-command)))))

(defun lsp-stdio-connection (command)
  "Create LSP stdio connection named name.
  COMMAND is either list of strings, string or function which
  returns the command to execute."
  (list :connect (lambda (filter sentinel name)
                   (let ((final-command (lsp-resolve-final-function command))
                         (process-name (generate-new-buffer-name name)))
                     (let ((proc (make-process
                                  :name process-name
                                  :connection-type 'pipe
                                  :buffer (format "*%s*" process-name)
                                  :coding 'no-conversion
                                  :command final-command
                                  :filter filter
                                  :sentinel sentinel
                                  :stderr (format "*%s::stderr*" process-name)
                                  :noquery t)))
                       (set-process-query-on-exit-flag proc nil)
                       (cons proc proc))))
        :test? (lambda () (-> command lsp-resolve-final-function lsp-server-present?))))

(defun lsp--open-network-stream (host port name &optional retry-count sleep-interval)
  "Open network stream to HOST:PORT.
  NAME will be passed to `open-network-stream'.
  RETRY-COUNT is the number of the retries.
  SLEEP-INTERVAL is the sleep interval between each retry."
  (let ((retries 0)
        connection)
    (while (and (not connection) (< retries (or retry-count 100)))
      (condition-case err
          (setq connection (open-network-stream name nil host port :type 'plain))
        (file-error
         (let ((inhibit-message t))
           (lsp--warn "Failed to connect to %s:%s with error message %s"
                      host
                      port
                      (error-message-string err))
           (sit-for (or sleep-interval 0.02))
           (cl-incf retries)))))
    connection))

(defun lsp--find-available-port (host starting-port)
  "Find available port on HOST starting from STARTING-PORT."
  (let ((success nil)
        (port starting-port))
    (while (and (not success))
      (condition-case _err
          (progn
            (delete-process (open-network-stream "*connection-test*" nil host port :type 'plain))
            (cl-incf port))
        (file-error (setq success t))))
    port))

(defun lsp-tcp-connection (command-fn)
  "Create LSP TCP connection named name.
  COMMAND-FN will be called to generate Language Server command."
  (list
   :connect (lambda (filter sentinel name)
              (let* ((host "localhost")
                     (port (lsp--find-available-port host (cl-incf lsp--tcp-port)))
                     (command (funcall command-fn port))
                     (final-command (if (consp command) command (list command)))
                     (_ (unless (executable-find (cl-first final-command))
                          (user-error (format "Couldn't find executable %s" (cl-first final-command)))))
                     (proc (make-process :name name :connection-type 'pipe :coding 'no-conversion
                                         :command final-command :sentinel sentinel :stderr name :noquery t))
                     (tcp-proc (lsp--open-network-stream host port (concat name "::tcp"))))

                ;; TODO: Same :noquery issue (see above)
                (set-process-query-on-exit-flag (get-buffer-process (get-buffer (process-name proc))) nil)
                (set-process-query-on-exit-flag tcp-proc nil)
                (set-process-filter tcp-proc filter)
                (cons tcp-proc proc)))
   :test? (lambda () (executable-find (cl-first (funcall command-fn 0))))))

(defun lsp-tcp-server (command)
  "Create tcp server connection.
In this mode Emacs is TCP server and the language server connects
to it. COMMAND is function with one parameter(the port) and it
should return the command to start the LS server."
  (list
   :connect (lambda (filter sentinel name)
              (let* (tcp-client-connection
                     (tcp-server (make-network-process :name (format "*tcp-server-%s*" name)
                                                       :buffer (format "*tcp-server-%s*" name)
                                                       :family 'ipv4
                                                       :service 0
                                                       :sentinel (lambda (proc _string)
                                                                   (lsp-log "Language server %s is connected." name)
                                                                   (setf tcp-client-connection proc))
                                                       :server 't))
                     (port (process-contact tcp-server :service))
                     (final-command (funcall command port))

                     (cmd-proc (make-process :name name
                                             :connection-type 'pipe
                                             :coding 'no-conversion
                                             :command final-command
                                             :stderr (format "*tcp-server-%s*::stderr" name)
                                             :noquery t)))
                (let ((retries 0))
                  (while (and (not tcp-client-connection) (< retries 20))
                    (lsp--info "Waiting for connection for %s, retries: %s" name retries)
                    (sit-for 0.500)
                    (cl-incf retries)))

                (unless tcp-client-connection
                  (condition-case nil (delete-process tcp-server) (error))
                  (condition-case nil (delete-process cmd-proc) (error))
                  (error "Failed to create connection to %s on port %s" name port))
                (lsp--info "Successfully connected to %s" name )

                (set-process-query-on-exit-flag cmd-proc nil)
                (set-process-query-on-exit-flag tcp-client-connection  nil)
                (set-process-query-on-exit-flag tcp-server nil)

                (set-process-filter tcp-client-connection filter)
                (set-process-sentinel tcp-client-connection sentinel)
                (cons tcp-client-connection cmd-proc)))
   :test? (lambda () (executable-find (cl-first (funcall command 0))))))

(defun lsp-tramp-connection (local-command)
  "Create LSP stdio connection named name.
LOCAL-COMMAND is either list of strings, string or function which
returns the command to execute."
  (list :connect (lambda (filter sentinel name)
                   (let* ((final-command (lsp-resolve-final-function local-command))
                          ;; wrap with stty to disable converting \r to \n
                          (wrapped-command (append '("stty" "raw" ";") final-command))
                          (process-name (generate-new-buffer-name name)))
                     (let ((proc (apply 'start-file-process-shell-command process-name
                                         (format "*%s*" process-name) wrapped-command)))
                       (set-process-sentinel proc sentinel)
                       (set-process-filter proc filter)
                       (set-process-query-on-exit-flag proc nil)
                       (set-process-coding-system proc 'binary 'binary)
                       (cons proc proc))))
        :test? (lambda () (-> local-command lsp-resolve-final-function lsp-server-present?))))

(defun lsp--auto-configure ()
  "Autoconfigure `lsp-ui', `company-lsp' if they are installed."

  (with-no-warnings
    (when (functionp 'lsp-ui-mode)
      (lsp-ui-mode))

    (cond
     ((eq :none lsp-prefer-flymake))
     ((and (not (version< emacs-version "26.1")) lsp-prefer-flymake)
      (lsp--flymake-setup))
     ((and (functionp 'lsp-ui-mode) (featurep 'flycheck))
      (require 'lsp-ui-flycheck)
      (lsp-ui-flycheck-enable t)
      (flycheck-mode 1)))

    (when (functionp 'company-lsp)
      (company-mode 1)
      (add-to-list 'company-backends 'company-lsp)

      (when (functionp 'yas-minor-mode)
        (yas-minor-mode t)))))

(defun lsp--make-workspace (client root)
  "Make workspace for the CLIENT and ROOT."
  (let* ((parser (make-lsp--parser))
         (workspace (make-lsp--workspace
                     :parser parser
                     :root root
                     :client client
                     :status 'starting
                     :buffers (list (current-buffer))
                     :host-root (file-remote-p root))))
    (setf (lsp--parser-workspace parser) workspace)
    workspace))

(defun lsp--restart-if-needed (workspace)
  "Handler restart for WORKSPACE."
  (when (or (eq lsp-restart 'auto-restart)
            (eq (lsp--workspace-shutdown-action workspace) 'restart)
            (and (eq lsp-restart 'interactive)
                 (let ((query (format "Server %s exited with status %s. Do you want to restart it?"
                                      (lsp--workspace-print workspace)
                                      (process-status (lsp--workspace-proc workspace)))))
                   (y-or-n-p query))))
    (--each (lsp--workspace-buffers workspace)
      (when (buffer-live-p it)
        (with-current-buffer it
          (lsp--info "Restarting LSP in buffer %s" (buffer-name))
          (lsp))))))

(defun lsp--update-key (table key fn)
  "Apply FN on value corresponding to KEY in TABLE."
  (let ((existing-value (gethash key table)))
    (if-let (new-value (funcall fn existing-value))
        (puthash key new-value table)
      (remhash key table))))

(defun lsp--create-sentinel (workspace)
  "Create sentinel handler for WORKSPACE."
  (lambda (process exit-str)
    (let* ((status (process-status process))
           (folder->workspaces (lsp-session-folder->servers (lsp-session)))
           (stderr (-> workspace lsp--workspace-proc process-name get-buffer)))

      (when (memq status '(exit signal))
        (lsp--warn "%s has exited (%s)"
                   (process-name (lsp--workspace-proc workspace))
                   (string-trim-right exit-str)))

      (with-lsp-workspace workspace
        ;; clean workspace related data in each of the buffers
        ;; in the workspace.
        (--each (lsp--workspace-buffers workspace)
          (when (buffer-live-p it)
            (with-current-buffer it
              (setq lsp--buffer-workspaces (delete workspace lsp--buffer-workspaces))
              (lsp--uninitialize-workspace)
              (lsp--spinner-stop))))

        ;; cleanup session from references to the closed workspace.
        (--each (hash-table-keys folder->workspaces)
          (lsp--update-key folder->workspaces it (apply-partially 'delete workspace)))

        ;; Kill standard error buffer only if the process exited normally.
        ;; Leave it intact otherwise for debugging purposes.
        (when (and (eq status 'exit) (zerop (process-exit-status process)) (buffer-live-p stderr))
          (kill-buffer stderr))

        (lsp--remove-cur-overlays))

      (run-hook-with-args 'lsp-after-uninitialized-hook workspace)

      (if (eq (lsp--workspace-shutdown-action workspace) 'shutdown)
          (lsp--info "Workspace %s shutdown." (lsp--workspace-print workspace))
        (lsp--restart-if-needed workspace))
      (lsp--cleanup-hanging-watches))))

(defun lsp--start-workspace (session client-template root &optional initialization-options)
  "Create new workspace for CLIENT-TEMPLATE with project root ROOT.
INITIALIZATION-OPTIONS are passed to initialize function.
SESSION is the active session."
  (lsp--spinner-start)
  (-let* ((default-directory root)
          (client (copy-lsp--client client-template))
          (workspace (lsp--make-workspace client root))
          (server-id (lsp--client-server-id client))
          ((proc . cmd-proc) (funcall
                              (or (plist-get (lsp--client-new-connection client) :connect)
                                  (user-error "Client %s is configured incorrectly" client))
                              (lsp--parser-make-filter (lsp--workspace-parser workspace)
                                                       (lsp--client-ignore-regexps client))
                              (lsp--create-sentinel workspace)
                              (format "%s" server-id)))
          (workspace-folders (gethash server-id (lsp-session-server-id->folders session))))
    (setf (lsp--workspace-proc workspace) proc
          (lsp--workspace-cmd-proc workspace) cmd-proc)

    ;; update (lsp-session-folder->servers) depending on whether we are starting
    ;; multi/single folder workspace
    (-map (lambda (project-root)
            (->> session
                 lsp-session-folder->servers
                 (gethash project-root)
                 (cl-pushnew workspace)))
          (or workspace-folders (list root)))

    (with-lsp-workspace workspace
      (run-hooks 'lsp-before-initialize-hook)
      (lsp-request-async "initialize"
                         (append
                          (list :processId (emacs-pid)
                                :rootPath (lsp-file-local-name (expand-file-name root))
                                :rootUri (lsp--path-to-uri root)
                                :capabilities (lsp--client-capabilities)
                                :initializationOptions initialization-options)
                          (when (lsp--client-multi-root client)
                            (->> workspace-folders
                                 (-map (lambda (folder)
                                         (list :uri (lsp--path-to-uri folder)
                                               :name (f-filename folder))))
                                 (apply 'vector)
                                 (list :workspaceFolders))))
                         (lambda (response)
                           (unless response
                             (lsp--spinner-stop)
                             (signal 'lsp-empty-response-error (list "initialize")))

                           (setf (lsp--workspace-server-capabilities workspace) (gethash "capabilities" response)
                                 (lsp--workspace-status workspace) 'initialized)

                           (with-lsp-workspace workspace
                             (lsp-notify "initialized" (make-hash-table)))

                           (--each (lsp--workspace-buffers workspace)
                             (with-current-buffer it
                               (lsp--open-in-workspace workspace)))

                           (when-let (initialize-fn (lsp--client-initialized-fn client))
                             (funcall initialize-fn workspace))

                           (with-lsp-workspace workspace
                             (run-hooks 'lsp-after-initialize-hook)))
                         :mode 'detached))
    workspace))

(defun lsp--load-default-session ()
  "Load default session."
  (setq lsp--session (or (lsp--read-from-file lsp-session-file)
                         (make-lsp-session))))

(defun lsp-session ()
  "Get the session associated with the current buffer."
  (or lsp--session (setq lsp--session (lsp--load-default-session))))

(defun lsp--find-clients (buffer-major-mode file-name)
  "Find clients which can handle BUFFER-MAJOR-MODE.
SESSION is the currently active session. The function will also
pick only remote enabled clients in case the FILE-NAME is on
remote machine and vice versa."
  (let ((remote? (file-remote-p file-name)))
    (-when-let (matching-clients (->> lsp-clients
                                      hash-table-values
                                      (-filter (-lambda (client)
                                                 (and (and (or (-some-> client lsp--client-activation-fn (funcall buffer-file-name buffer-major-mode))
                                                               (and (not (lsp--client-activation-fn client))
                                                                    (-contains? (lsp--client-major-modes client) buffer-major-mode)
                                                                    (eq (---truthy? remote?) (---truthy? (lsp--client-remote? client)))))
                                                           (-some-> client lsp--client-new-connection (plist-get :test?) funcall))
                                                      (or (null lsp-enabled-clients)
                                                          (or (member (lsp--client-server-id client) lsp-enabled-clients)
                                                              (ignore (lsp--info "Client %s is not in lsp-enabled-clients"
                                                                                 (lsp--client-server-id client))))))))))
      (lsp-log "Found the following clients for %s: %s"
               file-name
               (s-join ", "
                       (-map (lambda (client)
                               (format "(server-id %s, priority %s)"
                                       (lsp--client-server-id client)
                                       (lsp--client-priority client)))
                             matching-clients)))
      (-let* (((add-on-clients main-clients) (-separate 'lsp--client-add-on? matching-clients))
              (selected-clients (if-let (main-client (and main-clients
                                                          (--max-by (> (lsp--client-priority it)
                                                                       (lsp--client-priority other))
                                                                    main-clients)))
                                    (cons main-client add-on-clients)
                                  add-on-clients)))
        (lsp-log "The following clients were selected based on priority: %s"
                 (s-join ", "
                         (-map (lambda (client)
                                 (format "(server-id %s, priority %s)"
                                         (lsp--client-server-id client)
                                         (lsp--client-priority client)))
                               selected-clients)))
        selected-clients))))

(defun lsp-register-client (client)
  "Registers LSP client CLIENT."
  (puthash (lsp--client-server-id client) client lsp-clients))

(defun lsp--create-initialization-options (_session client)
  "Create initialization-options from SESSION and CLIENT.
Add workspace folders depending on server being multiroot and
session workspce folder configuration for the server."
  (let* ((initialization-options-or-fn (lsp--client-initialization-options client)))
    (if (functionp initialization-options-or-fn)
        (funcall initialization-options-or-fn)
      initialization-options-or-fn)))

(defun lsp--plist-delete (prop plist)
  "Delete by side effect the property PROP from PLIST.
If PROP is the first property in PLIST, there is no way
to remove it by side-effect; therefore, write
\(setq foo (evil-plist-delete :prop foo)) to be sure of
changing the value of `foo'."
  (let ((tail plist) elt head)
    (while tail
      (setq elt (car tail))
      (cond
       ((eq elt prop)
        (setq tail (cdr (cdr tail)))
        (if head
            (setcdr (cdr head) tail)
          (setq plist tail)))
       (t
        (setq head tail
              tail (cdr (cdr tail))))))
    plist))

(defvar lsp-client-settings nil)

(defun lsp-register-custom-settings (props)
  "Register PROPS.
The PROPS is list of triple (path symbol boolean?) Where: path is
the path to the property, symbol is the defcustom symbol which
will be used to retrieve the value and boolean determines whether
the type of the property is boolean?"
  (setq lsp-client-settings (-uniq (append lsp-client-settings props))))

(defun lsp-region-text (region)
  "Get the text for REGION in current buffer."
  (-let (((start . end) (lsp--range-to-region region)))
    (buffer-substring-no-properties start end)))

(defun lsp-ht-set (tbl paths value)
  "Set nested hashtable value.
TBL - a hashtable, PATHS is the path to the nested VALUE."
  (pcase paths
    (`(,path) (ht-set! tbl path value))
    (`(,path . ,rst) (let ((nested-tbl (or (gethash path tbl)
                                           (let ((temp-tbl (ht)))
                                             (ht-set! tbl path temp-tbl)
                                             temp-tbl)) ))
                       (lsp-ht-set nested-tbl rst value)))))

(defun lsp-configuration-section (section)
  "Get settings for SECTION."
  (let ((ret (ht-create)))
    (mapc (-lambda ((path variable boolean?))
            (when (s-matches? (concat section "\\..*") path)
              (let* ((symbol-value (symbol-value variable))
                     (value (if (and boolean? (not symbol-value))
                                :json-false
                              symbol-value)))
                (lsp-ht-set ret (s-split "\\." path) value))))
          lsp-client-settings)
    ret))

(defun lsp--start-connection (session client project-root)
  "Initiates connection created from CLIENT for PROJECT-ROOT.
SESSION is the active session."
  (when (lsp--client-multi-root client)
    (cl-pushnew project-root (gethash (lsp--client-server-id client)
                                      (lsp-session-server-id->folders session)) ))
  (run-hook-with-args 'lsp-workspace-folders-changed-hook (list project-root) nil)

  (unwind-protect
      (lsp--start-workspace session client project-root (lsp--create-initialization-options session client))
    (lsp--spinner-stop)))

(defun lsp-switch-to-io-log-buffer (workspace)
  (interactive
   (list (if lsp-print-io
             (if (eq (length (lsp-workspaces)) 1)
                 (nth 0 (lsp-workspaces))
               (lsp--completing-read "Workspace: " (lsp-workspaces)
                                     'lsp--workspace-print nil t))
           (user-error "IO logging is disabled"))))
  (let ((buffer (get-buffer-create (format "*lsp-io: %s*"
                                           (lsp--workspace-root workspace)))))
    (switch-to-buffer buffer)))

(defun lsp-log-io-next (arg)
  (interactive "P")
  (ewoc-goto-next lsp--log-io-ewoc (or arg 1)))

(defun lsp-log-io-prev (arg)
  (interactive "P")
  (ewoc-goto-prev lsp--log-io-ewoc (or arg 1)))

(define-derived-mode lsp-log-io-mode view-mode "LspLogIo"
  "Special mode for viewing IO logs.")

(define-key lsp-log-io-mode-map (kbd "M-n") #'lsp-log-io-next)
(define-key lsp-log-io-mode-map (kbd "M-p")  #'lsp-log-io-prev)

(define-derived-mode lsp-browser-mode special-mode "LspBrowser"
  "Define mode for displaying lsp sessions."
  (setq-local display-buffer-base-action '(nil . ((inhibit-same-window . t)))))

(defun lsp--workspace-print (workspace)
  "Visual representation WORKSPACE."
  (let* ((proc (lsp--workspace-cmd-proc workspace))
         (status (lsp--workspace-status workspace))
         (server-id (-> workspace lsp--workspace-client lsp--client-server-id symbol-name (propertize 'face 'bold-italic)))
         (pid (propertize (format "%s" (process-id proc)) 'face 'italic)))

    (if (eq 'initialized status)
        (format "%s:%s" server-id pid)
      (format "%s:%s status:%s" server-id pid status))))

(defun lsp--map-tree-widget (m)
  "Build `tree-widget' from a hash-table M."
  (when (hash-table-p m)
    (let (nodes)
      (maphash (lambda (k v)
                 (push `(tree-widget
                         :tag ,(if (hash-table-p v)
                                   (format "%s:" k)
                                 (format "%s: %s" k
                                         (propertize (format "%s" v)
                                                     'face
                                                     'font-lock-string-face)))
                         :open t
                         ,@(lsp--map-tree-widget v))
                       nodes))
               m)
      nodes)))

(defun lsp--render-workspace (workspace)
  "Tree node representation of WORKSPACE."
  `(tree-widget :tag ,(lsp--workspace-print workspace)
                :open t
                (tree-widget :tag ,(propertize "Buffers" 'face 'font-lock-function-name-face)
                             :open t
                             ,@(->> workspace
                                    (lsp--workspace-buffers)
                                    (--map `(tree-widget
                                             :tag ,(when (buffer-live-p it)
                                                     (if (with-current-buffer it buffer-read-only)
                                                         (propertize (buffer-name it)
                                                                     'face 'font-lock-constant-face)
                                                       (buffer-name it)))))))
                (tree-widget :tag ,(propertize "Capabilities" 'face 'font-lock-function-name-face)
                             ,@(-> workspace lsp--workspace-server-capabilities lsp--map-tree-widget))))

(defun lsp-describe-session ()
  "Describes current `lsp-session'."
  (interactive)
  (let ((session (lsp-session))
        (buf (get-buffer-create "*lsp session*")))
    (with-current-buffer buf
      (lsp-browser-mode)
      (cursor-sensor-mode 1)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (--each (lsp-session-folders session)
          (widget-create
           `(tree-widget
             :tag ,(propertize it 'face 'font-lock-keyword-face)
             :open t
             ,@(->> session
                    (lsp-session-folder->servers)
                    (gethash it)
                    (-map 'lsp--render-workspace)))))))
    (pop-to-buffer buf)))

(defun lsp--session-workspaces (session)
  "Get all workspaces that are part of the SESSION."
  (-> session lsp-session-folder->servers hash-table-values -flatten -uniq))

(defun lsp--find-multiroot-workspace (session client project-root)
  "Look for a multiroot connection in SESSION created from CLIENT for PROJECT-ROOT and BUFFER-MAJOR-MODE."
  (when (lsp--client-multi-root client)
    (-when-let (multi-root-workspace (->> session
                                          (lsp--session-workspaces)
                                          (--first (eq (-> it lsp--workspace-client lsp--client-server-id)
                                                       (lsp--client-server-id client)))))
      (with-lsp-workspace multi-root-workspace
        (lsp-notify "workspace/didChangeWorkspaceFolders"
                    `(:event (:added ,(vector (list :uri (lsp--path-to-uri project-root)))))))

      (->> session (lsp-session-folder->servers) (gethash project-root) (cl-pushnew multi-root-workspace))
      (->> session (lsp-session-server-id->folders) (gethash (lsp--client-server-id client)) (cl-pushnew project-root))

      (lsp--persist-session session)

      (lsp--info "Opened folder %s in workspace %s" project-root (lsp--workspace-print multi-root-workspace))

      multi-root-workspace)))

(defun lsp--ensure-lsp-servers (session clients project-root ignore-multi-folder)
  "Ensure that SESSION contain server CLIENTS created for PROJECT-ROOT.
IGNORE-MULTI-FOLDER to ignore multi folder server."
  (-map (lambda (client)
          (or
           (lsp--find-workspace session client project-root)
           (unless ignore-multi-folder
             (lsp--find-multiroot-workspace session client project-root))
           (lsp--start-connection session client project-root)))
        clients))

(defun lsp--spinner-stop ()
  "Stop the spinner in case all of the workspaces are started."
  (when (--all? (eq (lsp--workspace-status it) 'initialized)
                lsp--buffer-workspaces)
    (spinner-stop)))

(defun lsp--open-in-workspace (workspace)
  "Open in existing WORKSPACE."
  (if (eq 'initialized (lsp--workspace-status workspace))
      ;; when workspace is initialized just call document did open.
      (progn
        (with-lsp-workspace workspace
          (when-let (before-document-open-fn (-> workspace
                                                 lsp--workspace-client
                                                 lsp--client-before-file-open-fn))
            (funcall before-document-open-fn workspace))
          (lsp--text-document-did-open))
        (lsp--spinner-stop))
    ;; when it is not initialized
    (lsp--spinner-start)
    (cl-pushnew (current-buffer) (lsp--workspace-buffers workspace))))

(defun lsp--find-workspace (session client project-root)
  "Find server connection created with CLIENT in SESSION for PROJECT-ROOT."
  (when-let ((workspace (->> session
                             (lsp-session-folder->servers)
                             (gethash project-root)
                             (--first (eql (-> it lsp--workspace-client lsp--client-server-id)
                                           (lsp--client-server-id client))))))
    (lsp--open-in-workspace workspace)
    workspace))

(defun lsp--find-root-interactively (session)
  "Find project interactively.
Returns nil if the project should not be added to the current SESSION."
  (condition-case nil
      (let* ((project-root-suggestion (or (lsp--suggest-project-root) default-directory))
             (choices (list
                       (format "Import project root %s" project-root-suggestion)
                       "Import project by selecting root directory interactively."
                       (format "Do not ask more for the current project(add \"%s\" to lsp-session-folder-blacklist)"
                               project-root-suggestion)
                       "Do not ask more for the current project(select ignore path interactively)."
                       "Do nothing and ask me again when opening other files from the folder."))
             (action-index (cl-position
                            (completing-read (format "%s is not part of any project. Select action: "
                                                     (buffer-name))
                                             choices
                                             nil
                                             t)
                            choices
                            :test 'equal)))
        (cl-case action-index
          (0 project-root-suggestion)
          (1 (read-directory-name "Select workspace folder to add: "
                                  (or project-root-suggestion default-directory)
                                  nil
                                  t))
          (2 (push project-root-suggestion (lsp-session-folders-blacklist session))
             (lsp--persist-session session)
             nil)
          (3 (push (read-directory-name "Select folder to blacklist: "
                                        (or project-root-suggestion default-directory)
                                        nil
                                        t)
                   (lsp-session-folders-blacklist session))
             (lsp--persist-session session)
             nil)
          (t nil)))
    ('quit)))

(defun lsp-find-session-folder (session file-name)
  "Look in the current SESSION for folder containing FILE-NAME."
  (let ((file-name-canonical (f-canonical file-name)))
    (->> session
         (lsp-session-folders)
         (--filter (or (f-same? it file-name-canonical)
                       (f-ancestor-of? it file-name-canonical)))
         (--max-by (> (length it)
                      (length other))))))

(defun lsp-find-workspace (server-id file-name)
  "Find workspace for SERVER-ID for FILE-NAME."
  (-when-let* ((session (lsp-session))
               (folder->servers (lsp-session-folder->servers session))
               (workspaces (if file-name
                               (gethash (lsp-find-session-folder session file-name) folder->servers)
                             (lsp--session-workspaces session))))

    (--first (eq (lsp--client-server-id (lsp--workspace-client it)) server-id) workspaces)))

(defun lsp--calculate-root (session file-name)
  "Calculate project root for FILE-NAME in SESSION."
  (and
   (->> session
        (lsp-session-folders-blacklist)
        (--first (and (f-ancestor-of? it file-name)
                      (prog1 t
                        (lsp--info "File %s is in blacklisted directory %s" file-name it))))
        not)
   (or
    (when lsp-auto-guess-root
      (lsp--suggest-project-root))
    (lsp-find-session-folder session file-name)
    (unless lsp-auto-guess-root
      (lsp--find-root-interactively session)))))

(defun lsp--try-open-in-library-workspace ()
  "Try opening current file as library file in any of the active workspace.
The library folders are defined by each client for each of the active workspace."
  (when-let (workspace (->> (lsp-session)
                            (lsp--session-workspaces)
                            (--first
                             (and (-contains? (-> it lsp--workspace-client lsp--client-major-modes)
                                              major-mode)
                                  (when-let (library-folders-fn
                                             (-> it lsp--workspace-client lsp--client-library-folders-fn))
                                    (-first (lambda (library-folder)
                                              (f-ancestor-of? library-folder (buffer-file-name)))
                                            (funcall library-folders-fn it)))))))
    (lsp--open-in-workspace workspace)
    (view-mode t)
    (list workspace)))

(defun lsp--persist-session (session)
  "Persist SESSION to `lsp-session-file'."
  (lsp--persist lsp-session-file (make-lsp-session
                                  :folders (lsp-session-folders session)
                                  :folders-blacklist (lsp-session-folders-blacklist session)
                                  :server-id->folders (lsp-session-server-id->folders session))))

(defun lsp--try-project-root-workspaces (ask-for-client ignore-multi-folder)
  "Try create opening file as a project file.
When IGNORE-MULTI-FOLDER is t the lsp mode will start new
language server even if there is language server which can handle
current language. When IGNORE-MULTI-FOLDER is nil current file
will be openned in multi folder language server if there is
such."
  (-let ((session (lsp-session)))
    (-if-let (clients (if ask-for-client
                          (list (lsp--completing-read "Select server to start: "
                                                      (ht-values lsp-clients)
                                                      (-compose 'symbol-name 'lsp--client-server-id) nil t))
                        (lsp--find-clients major-mode (buffer-file-name))))
        (-if-let (project-root (lsp--calculate-root session (buffer-file-name)))
            (progn
              ;; update project roots if needed and persit the lsp session
              (unless (-contains? (lsp-session-folders session) project-root)
                (push project-root (lsp-session-folders session))
                (lsp--persist-session session))
              (lsp--ensure-lsp-servers session clients project-root ignore-multi-folder))
          (lsp--warn "%s not in project or it is blacklisted." (buffer-name))
          nil)
      (lsp--warn "No LSP server for %s." major-mode)
      nil)))

(defun lsp-shutdown-workspace ()
  "Shutdown language server."
  (interactive)
  (--when-let (pcase (lsp-workspaces)
                (`nil (user-error "There are no active servers in the current buffer"))
                (`(,workspace) (when (y-or-n-p (format "Are you sure you want to stop the server %s?"
                                                       (lsp--workspace-print workspace)))
                                 workspace))
                (workspaces (lsp--completing-read "Select server: "
                                                  workspaces
                                                  'lsp--workspace-print nil t)))
    (lsp--warn "Stopping %s" (lsp--workspace-print it))
    (setf (lsp--workspace-shutdown-action it) 'shutdown)
    (with-lsp-workspace it (lsp--shutdown-workspace))))

(defun lsp-restart-workspace ()
  "Restart language server."
  (interactive)
  (--when-let (pcase (lsp-workspaces)
                (`nil (user-error "There are no active servers in the current buffer"))
                (`(,workspace) workspace)
                (workspaces (lsp--completing-read "Select server: "
                                                  workspaces
                                                  'lsp--workspace-print nil t)))
    (lsp--warn "Restarting %s" (lsp--workspace-print it))
    (setf (lsp--workspace-shutdown-action it) 'restart)
    (with-lsp-workspace it (lsp--shutdown-workspace))))

;;;###autoload
(defun lsp (&optional arg)
  "Entry point for the server startup.
When ARG is t the lsp mode will start new language server even if
there is language server which can handle current language. When
ARG is nil current file will be openned in multi folder language
server if there is such. When `lsp' is called with prefix
argument ask the user to select which language server to start. "
  (interactive "P")

  (when (and lsp-auto-configure lsp-auto-require-clients)
    (require 'lsp-clients))

  (when (and (buffer-file-name)
             (setq-local lsp--buffer-workspaces
                         (or (lsp--try-open-in-library-workspace)
                             (lsp--try-project-root-workspaces (equal arg '(4))
                                                               (and arg (not (equal arg 1)))))))
    (lsp-mode 1)
    (when lsp-auto-configure (lsp--auto-configure))

    (lsp--info "Connected to %s."
               (apply 'concat (--map (format "[%s]" (lsp--workspace-print it))
                                     lsp--buffer-workspaces)))))

(provide 'lsp-mode)
;;; lsp-mode.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
