;;; eglot-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "eglot" "eglot.el" (0 0 0 0))
;;; Generated autoloads from eglot.el

(autoload 'eglot "eglot" "\
Start LSP server for PROJECT's buffers under MANAGED-MAJOR-MODES.

This starts a Language Server Protocol (LSP) server suitable for
the buffers of PROJECT whose `major-mode' is among
MANAGED-MAJOR-MODES.  CLASS is the class of the LSP server to
start and CONTACT specifies how to connect to the server.

Interactively, the command attempts to guess MANAGED-MAJOR-MODES,
CLASS, CONTACT, and LANGUAGE-IDS from `eglot-server-programs',
according to the current buffer's `major-mode'.  PROJECT is
guessed from `project-find-functions'.  The search for active
projects in this context binds `eglot-lsp-context' (which see).

If it can't guess, it prompts the user for the mode and the
server.  With a single \\[universal-argument] prefix arg, it
always prompts for COMMAND.  With two \\[universal-argument], it
also always prompts for MANAGED-MAJOR-MODE.

The LSP server of CLASS is started (or contacted) via CONTACT.
If this operation is successful, current *and future* file
buffers of MANAGED-MAJOR-MODE inside PROJECT become \"managed\"
by the LSP server, meaning the information about their contents is
exchanged periodically with the server to provide enhanced
code-analysis via `xref-find-definitions', `flymake-mode',
`eldoc-mode', and `completion-at-point', among others.

PROJECT is a project object as returned by `project-current'.

CLASS is a subclass of `eglot-lsp-server'.

CONTACT specifies how to contact the server.  It is a
keyword-value plist used to initialize CLASS or a plain list as
described in `eglot-server-programs', which see.

LANGUAGE-IDS is a list of language ID string to send to the
server for each element in MANAGED-MAJOR-MODES.

INTERACTIVE is ignored and provided for backward compatibility.

\(fn MANAGED-MAJOR-MODES PROJECT CLASS CONTACT LANGUAGE-IDS &optional INTERACTIVE)" t nil)

(autoload 'eglot-ensure "eglot" "\
Start Eglot session for current buffer if there isn't one." nil nil)

(autoload 'eglot-update "eglot" "\
Update Eglot.

\(fn &rest _)" t nil)

(put 'eglot-workspace-configuration 'safe-local-variable 'listp)

(put 'eglot--debbugs-or-github-bug-uri 'bug-reference-url-format t)

(defun eglot--debbugs-or-github-bug-uri nil (format (if (string= (match-string 2) "github") "https://github.com/joaotavora/eglot/issues/%s" "https://debbugs.gnu.org/%s") (match-string 3)))

(register-definition-prefixes "eglot" '("eglot-"))

;;;***

;;;### (autoloads nil nil ("eglot-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; eglot-autoloads.el ends here
