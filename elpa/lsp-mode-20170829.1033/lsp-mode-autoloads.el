;;; lsp-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "lsp-methods" "lsp-methods.el" (22950 34548
;;;;;;  133097 599000))
;;; Generated autoloads from lsp-methods.el

(let ((loads (get 'lsp-mode 'custom-loads))) (if (member '"lsp-methods" loads) nil (put 'lsp-mode 'custom-loads (cons '"lsp-methods" loads))))

(let ((loads (get 'lsp-faces 'custom-loads))) (if (member '"lsp-methods" loads) nil (put 'lsp-faces 'custom-loads (cons '"lsp-methods" loads))))

(defvar lsp-document-sync-method nil "\
How to sync the document with the language server.")

(custom-autoload 'lsp-document-sync-method "lsp-methods" t)

(defvar lsp-project-blacklist nil "\
A list of project directories for which LSP shouldn't be initialized.")

(custom-autoload 'lsp-project-blacklist "lsp-methods" t)

(defvar lsp-enable-eldoc t "\
Enable `eldoc-mode' integration.")

(custom-autoload 'lsp-enable-eldoc "lsp-methods" t)

(defvar lsp-highlight-symbol-at-point t "\
Highlight the symbol under the point.")

(custom-autoload 'lsp-highlight-symbol-at-point "lsp-methods" t)

(defvar lsp-enable-codeaction t "\
Enable code action processing.")

(custom-autoload 'lsp-enable-codeaction "lsp-methods" t)

(defvar lsp-enable-completion-at-point t "\
Enable `completion-at-point' integration.")

(custom-autoload 'lsp-enable-completion-at-point "lsp-methods" t)

(defvar lsp-enable-xref t "\
Enable xref integration.")

(custom-autoload 'lsp-enable-xref "lsp-methods" t)

(defvar lsp-enable-flycheck t "\
Enable flycheck integration.")

(custom-autoload 'lsp-enable-flycheck "lsp-methods" t)

(defvar lsp-enable-indentation t "\
Indent regions using the file formatting functionality provided by the language server.")

(custom-autoload 'lsp-enable-indentation "lsp-methods" t)

(defface lsp-face-highlight-textual '((t :background "yellow")) "\
Face used for textual occurances of symbols." :group (quote lsp-faces))

(defface lsp-face-highlight-read '((t :background "red")) "\
Face used for highlighting symbols being read." :group (quote lsp-faces))

(defface lsp-face-highlight-write '((t :background "green")) "\
Face used for highlighting symbols being written to." :group (quote lsp-faces))

(defvar lsp-change-idle-delay 0.5 "\
Number of seconds of idle timer to wait before sending file changes to the server.")

(custom-autoload 'lsp-change-idle-delay "lsp-methods" t)

;;;***

;;;### (autoloads nil "lsp-mode" "lsp-mode.el" (22950 34548 269765
;;;;;;  250000))
;;; Generated autoloads from lsp-mode.el

(autoload 'lsp-mode "lsp-mode" "\


\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("lsp-common.el" "lsp-flycheck.el" "lsp-mode-pkg.el"
;;;;;;  "lsp-notifications.el" "lsp-receive.el" "lsp-send.el") (22950
;;;;;;  34548 236431 676000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; lsp-mode-autoloads.el ends here
