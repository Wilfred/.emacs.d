;;; helm-dash.el --- Helm extension to search dash docsets
;; Copyright (C) 2013-2014  Raimon Grau
;; Copyright (C) 2013-2014  Toni Reina

;; Author: Raimon Grau <raimonster@gmail.com>
;;         Toni Reina  <areina0@gmail.com>
;; Version: 20140122.1457
;; X-Original-Version: 0.1
;; Package-Requires: ((helm "0.0.0"))
;; Keywords: docs

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
;;
;;; Commentary:
;;
;; Clone the functionality of dash using helm foundation.  Browse
;; documentation via dash docsets.
;;
;; M-x helm-dash
;; M-x helm-dash-at-point
;;
;; More info in the project site https://github.com/areina/helm-dash
;;
;;; Code:
(eval-when-compile (require 'cl))

(require 'helm)
(require 'helm-match-plugin)
(require 'json)
(require 'ido)

(defgroup helm-dash nil
  "Search Dash docsets using helm."
  :prefix "helm-dash-"
  :group 'applications)

(defcustom helm-dash-docsets-path
  (format "%s/.docsets"  (getenv "HOME"))
  "Default path for docsets."
  :group 'helm-dash)

(defcustom helm-dash-docsets-url "https://raw.github.com/Kapeli/feeds/master"
  "Foo." :group 'helm-dash)

(defcustom helm-dash-completing-read-func 'ido-completing-read
  "Completion function to be used when installing docsets.

Suggested possible values are:
 * `completing-read':       built-in completion method.
 * `ido-completing-read':   dynamic completion within the minibuffer."
  :type 'function
  :options '(completing-read ido-completing-read)
  :group 'helm-dash)

(defcustom helm-dash-min-lengh 3
  "Minimum length to start searching in docsets.
0 facilitates discoverability, but may be a bit heavy when lots
of docsets are active.  Between 0 and 3 is sane.")

(defvar helm-dash-common-docsets
  '() "List of Docsets to search active by default.")

(defun helm-dash-connect-to-docset (docset)
  "Compose the path to sqlite DOCSET."
  (format "%s/%s.docset/Contents/Resources/docSet.dsidx"
	  helm-dash-docsets-path docset))

(defvar helm-dash-connections nil
  "Create conses like (\"Go\" . connection).")

(defun helm-dash-sql (db-path sql)
  ""
  (mapcar (lambda (x) (split-string x "|" t))
	  (split-string (shell-command-to-string (format "sqlite3 \"%s\" \"%s\"" db-path sql)) "\n" t)))

(defun helm-dash-filter-connections ()
  "Filter connections using `helm-dash-connections-filters'."
  (let ((docsets (helm-dash-buffer-local-docsets))
        (connections nil))
    (setq docsets (append docsets helm-dash-common-docsets))
    (delq nil (mapcar (lambda (y)
                        (assoc y helm-dash-connections))
             docsets))))

(defun helm-dash-buffer-local-docsets ()
  "Get the docsets configured for the current buffer."
  (with-helm-current-buffer
    (or (and (boundp 'helm-dash-docsets) helm-dash-docsets)
        '())))

(defun helm-dash-create-common-connections ()
  "Create connections to sqlite docsets for common docsets."
  (when (not helm-dash-connections)
    (setq helm-dash-connections
          (mapcar (lambda (x)
                    (let ((db-path (helm-dash-connect-to-docset x)))
                      (list x db-path (helm-dash-docset-type db-path))))
                  helm-dash-common-docsets))))

(defun helm-dash-create-buffer-connections ()
  "Create connections to sqlite docsets for buffer-local docsets."
  (mapc (lambda (x) (when (not (assoc x helm-dash-connections))
                      (let ((connection  (helm-dash-connect-to-docset x)))
                        (setq helm-dash-connections
                              (cons (list x connection (helm-dash-docset-type connection))
                                    helm-dash-connections)))))
        (helm-dash-buffer-local-docsets)))

(defun helm-dash-reset-connections ()
  "Wipe all connections to docsets."
  (interactive)
  (setq helm-dash-connections nil))

(defun helm-dash-docset-type (db-path)
  "Return the type of the docset based in db schema.
Possible values are \"DASH\" and \"ZDASH\.
The Argument DB-PATH should be a string with the sqlite db path."
  (let ((type_sql "SELECT name FROM sqlite_master WHERE type = 'table' LIMIT 1"))
    (if (member "searchIndex" (car (helm-dash-sql db-path type_sql)))
	"DASH"
      "ZDASH")))

(defun helm-dash-search-all-docsets ()
  "Fetch docsets from the original Kapeli's feed."
  (let ((url "https://api.github.com/repos/Kapeli/feeds/contents/"))
    (with-current-buffer
        (url-retrieve-synchronously url)
      (goto-char url-http-end-of-headers)
      (json-read))))

(defvar helm-dash-ignored-docsets
  '("Bootstrap" "Drupal" "Zend_Framework" "Ruby_Installed_Gems" "Man_Pages")
  "Return a list of ignored docsets.
These docsets are not available to install.
See here the reason: https://github.com/areina/helm-dash/issues/17.")

(defun helm-dash-available-docsets ()
  "Return a list of official docsets (http://kapeli.com/docset_links)."
  (delq nil (mapcar (lambda (docset)
                      (let ((name (assoc-default 'name (cdr docset))))
                        (if (and (equal (file-name-extension name) "xml")
                                 (not
                                  (member (file-name-sans-extension name) helm-dash-ignored-docsets)))
                            (file-name-sans-extension name))))
                    (helm-dash-search-all-docsets))))

(defun helm-dash-installed-docsets ()
  "Return a list of installed docsets."
  (let ((docsets (directory-files helm-dash-docsets-path nil ".docset$")))
    (mapcar #'(lambda (name)
               (cond ((string-match "[^.]+" name) (match-string 0 name))
                     (t name)))
            docsets)))

(defun helm-dash-activate-docset (docset)
  "Activate DOCSET.  If called interactively prompts for the docset name."
  (interactive (list (funcall helm-dash-completing-read-func
                              "Activate docset: " (helm-dash-installed-docsets)
                              nil t)))
  (add-to-list 'helm-dash-common-docsets docset)
  (helm-dash-reset-connections))

;;;###autoload
(defun helm-dash-install-docset ()
  "Download docset with specified NAME and move its stuff to docsets-path."
  (interactive)
  (let* ((docset-name (funcall helm-dash-completing-read-func
                               "Install docset: " (helm-dash-available-docsets)))
         (feed-url (format "%s/%s.xml" helm-dash-docsets-url docset-name))
         (docset-tmp-path (format "%s%s-docset.tgz" temporary-file-directory docset-name))
         (feed-tmp-path (format "%s%s-feed.xml" temporary-file-directory docset-name)))
    (url-copy-file feed-url feed-tmp-path t)
    (url-copy-file (helm-dash-get-docset-url feed-tmp-path) docset-tmp-path t)

    (when (and (not (file-directory-p helm-dash-docsets-path))
	       (y-or-n-p (format "Directory %s does not exist.  Want to create it? "
				 helm-dash-docsets-path)))
      (mkdir helm-dash-docsets-path))
    (let ((docset-folder
	   (helm-dash-docset-folder-name
	    (shell-command-to-string (format "tar xvf %s -C %s" docset-tmp-path helm-dash-docsets-path)))))
      (helm-dash-activate-docset docset-folder)
      (message (format
		"Docset installed. Add \"%s\" to helm-dash-common-docsets or helm-dash-docsets."
		docset-folder)))))

(fset 'helm-dash-update-docset 'helm-dash-install-docset)

(defun helm-dash-docset-folder-name (tar-output)
  "Return the name of the folder where the docset has been extracted.
The argument TAR-OUTPUT should be an string with the output of a tar command."
  (let ((last-line
	 (car (last (split-string tar-output "\n" t)))))
    (replace-regexp-in-string "^x " "" (car (split-string last-line "\\." t)))))

(defun helm-dash-get-docset-url (feed-path)
  "Parse a xml feed with docset urls and return the first url.
The Argument FEED-PATH should be a string with the path of the xml file."
  (let* ((xml (xml-parse-file feed-path))
         (urls (car xml))
         (url (xml-get-children urls 'url)))
    (caddr (first url))))

(defvar helm-dash-sql-queries
  '((DASH . ((select . (lambda ()
                         (let ((like (helm-dash-sql-compose-like "t.name" helm-pattern))
                               (query "SELECT t.type, t.name, t.path FROM searchIndex t WHERE %s ORDER BY LOWER(t.name) LIMIT 20"))
                           (format query like))))))
    (ZDASH . ((select . (lambda ()
                          (let ((like (helm-dash-sql-compose-like "t.ZTOKENNAME" helm-pattern))
                                (query "SELECT ty.ZTYPENAME, t.ZTOKENNAME, f.ZPATH, m.ZANCHOR FROM ZTOKEN t, ZTOKENTYPE ty, ZFILEPATH f, ZTOKENMETAINFORMATION m WHERE ty.Z_PK = t.ZTOKENTYPE AND f.Z_PK = m.ZFILE AND m.ZTOKEN = t.Z_PK AND %s ORDER BY LOWER(t.ZTOKENNAME) LIMIT 20"))
                            (format query like))))))))

(defun helm-dash-sql-compose-like (column pattern)
  ""
  (let ((conditions (mapcar (lambda (word) (format "%s like '%%%s%%'" column word))
                            (split-string pattern " "))))
    (format "%s" (mapconcat 'identity conditions " AND "))))

(defun helm-dash-sql-execute (query-type docset-type)
  ""
  (funcall (cdr (assoc query-type (assoc (intern docset-type) helm-dash-sql-queries)))))

(defun helm-dash-search ()
  "Iterates every `helm-dash-connections' looking for the `helm-pattern'."
  (let ((full-res (list))
        (connections (helm-dash-filter-connections)))
    (dolist (docset connections)
      (let* ((docset-type (caddr docset))
             (res
	      (helm-dash-sql (cadr docset)
			     (helm-dash-sql-execute 'select docset-type))))
        ;; how to do the appending properly?
        (setq full-res
              (append full-res
                      (mapcar (lambda (x)
                                (cons (format "%s - %s"  (car docset) (cadr x)) (helm-dash-result-url docset x)))
                              res)))))
    full-res))

(defun helm-dash-result-url (docset result)
  ""
  (let* ((anchor (car (last result)))
	 (filename
	 (format "%s%s"
		 (caddr result)
		 (if (or (eq :null anchor) (not anchor)) "" (format "#%s" anchor)))))
    (format "%s%s%s%s"
	    "file://"
	    helm-dash-docsets-path
	    (format "/%s.docset/Contents/Resources/Documents/" (car docset))
	    filename)))

(defun helm-dash-actions (actions doc-item) `(("Go to doc" . browse-url)))

(defun helm-source-dash-search ()
  `((name . "Dash")
    (volatile)
    (delayed)
    (requires-pattern . ,helm-dash-min-lengh)
    (candidates-process . helm-dash-search)
    (action-transformer . helm-dash-actions)))

;;;###autoload
(defun helm-dash ()
  "Bring up a Dash search interface in helm."
  (interactive)
  (helm-dash-create-common-connections)
  (helm-dash-create-buffer-connections)
  (helm :sources (list (helm-source-dash-search))
	:buffer "*helm-dash*"))

;;;###autoload
(defun helm-dash-at-point ()
  "Bring up a Dash search interface in helm using the symbol at
point as prefilled search."
  (interactive)
  (helm-dash-create-common-connections)
  (helm-dash-create-buffer-connections)
  (helm :sources (list (helm-source-dash-search))
	:buffer "*helm-dash*"
	:input (thing-at-point 'symbol)))

(provide 'helm-dash)

;;; helm-dash.el ends here
