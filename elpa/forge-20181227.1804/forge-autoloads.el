;;; forge-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "forge-bitbucket" "forge-bitbucket.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from forge-bitbucket.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "forge-bitbucket" '("forge-bitbucket-repository")))

;;;***

;;;### (autoloads nil "forge-commands" "forge-commands.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from forge-commands.el
 (autoload 'forge-dispatch "forge-commands" nil t)

(autoload 'forge-pull "forge-commands" "\
Pull topics from the forge repository.

\(fn &optional REPO)" t nil)

(autoload 'forge-pull-notifications "forge-commands" "\
Fetch notifications for all repositories from the current forge.

\(fn)" t nil)

(autoload 'forge-pull-pullreq "forge-commands" "\
Pull a single pull-request from the forge repository.
Normally you wouldn't want to pull a single pull-request by
itself, but due to a bug in the Github API you might sometimes
have to do so.  See https://platform.github.community/t/7284.

\(fn PULLREQ)" t nil)

(autoload 'forge-browse-dwim "forge-commands" "\
Visit a topic, branch or commit using a browser.
Prefer a topic over a branch and that over a commit.

\(fn)" t nil)

(autoload 'forge-browse-commit "forge-commands" "\
Visit the url corresponding to REV using a browser.

\(fn REV)" t nil)

(autoload 'forge-browse-branch "forge-commands" "\
Visit the url corresponding BRANCH using a browser.

\(fn BRANCH)" t nil)

(autoload 'forge-browse-remote "forge-commands" "\
Visit the url corresponding to REMOTE using a browser.

\(fn REMOTE)" t nil)

(autoload 'forge-browse-topic "forge-commands" "\
Visit the current topic using a browser.

\(fn)" t nil)

(autoload 'forge-browse-pullreqs "forge-commands" "\
Visit the url corresponding to REPO's pull-requests using a browser.

\(fn REPO)" t nil)

(autoload 'forge-browse-pullreq "forge-commands" "\
Visit the url corresponding to PULLREQ using a browser.

\(fn PULLREQ)" t nil)

(autoload 'forge-browse-issues "forge-commands" "\
Visit the url corresponding to REPO's issues using a browser.

\(fn REPO)" t nil)

(autoload 'forge-browse-issue "forge-commands" "\
Visit the url corresponding to ISSUE using a browser.

\(fn ISSUE)" t nil)

(autoload 'forge-browse-post "forge-commands" "\
Visit the url corresponding to the post at point using a browser.

\(fn)" t nil)

(autoload 'forge-visit-topic "forge-commands" "\
View the topic at point in a separate buffer.

\(fn)" t nil)

(autoload 'forge-visit-pullreq "forge-commands" "\
View the pull-request at point in a separate buffer.

\(fn PULLREQ)" t nil)

(autoload 'forge-visit-issue "forge-commands" "\
View the issue at point in a separate buffer.

\(fn ISSUE)" t nil)

(autoload 'forge-branch-pullreq "forge-commands" "\
Create and configure a new branch from a pull-request.
Please see the manual for more information.

\(fn PULLREQ)" t nil)

(autoload 'forge-checkout-pullreq "forge-commands" "\
Create, configure and checkout a new branch from a pull-request.
Please see the manual for more information.

\(fn PULLREQ)" t nil)

(autoload 'forge-checkout-worktree "forge-commands" "\
Create, configure and checkout a new worktree from a pull-request.
This is like `magit-checkout-pull-request', except that it
also creates a new worktree. Please see the manual for more
information.

\(fn PATH PULLREQ)" t nil)

(autoload 'forge-list-notifications "forge-commands" "\
List notifications.

\(fn)" t nil)

(autoload 'forge-add-pullreq-refspec "forge-commands" "\
Configure Git to fetch all pull-requests.
This is done by adding \"+refs/pull/*/head:refs/pullreqs/*\"
to the value of `remote.REMOTE.fetch', where REMOTE is the
upstream remote.  Also fetch from REMOTE.

\(fn)" t nil)

(autoload 'forge-reset-database "forge-commands" "\
Move the current database file to the trash.
This is useful after the database scheme has changed, which will
happen a few times while the forge functionality is still under
heavy development.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "forge-commands" '("forge-")))

;;;***

;;;### (autoloads nil "forge-core" "forge-core.el" (0 0 0 0))
;;; Generated autoloads from forge-core.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "forge-core" '("forge-")))

;;;***

;;;### (autoloads nil "forge-db" "forge-db.el" (0 0 0 0))
;;; Generated autoloads from forge-db.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "forge-db" '("forge-")))

;;;***

;;;### (autoloads nil "forge-gitea" "forge-gitea.el" (0 0 0 0))
;;; Generated autoloads from forge-gitea.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "forge-gitea" '("forge-gitea-repository")))

;;;***

;;;### (autoloads nil "forge-github" "forge-github.el" (0 0 0 0))
;;; Generated autoloads from forge-github.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "forge-github" '("forge-")))

;;;***

;;;### (autoloads nil "forge-gitlab" "forge-gitlab.el" (0 0 0 0))
;;; Generated autoloads from forge-gitlab.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "forge-gitlab" '("forge-")))

;;;***

;;;### (autoloads nil "forge-gogs" "forge-gogs.el" (0 0 0 0))
;;; Generated autoloads from forge-gogs.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "forge-gogs" '("forge-gogs-repository")))

;;;***

;;;### (autoloads nil "forge-issue" "forge-issue.el" (0 0 0 0))
;;; Generated autoloads from forge-issue.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "forge-issue" '("forge-")))

;;;***

;;;### (autoloads nil "forge-list" "forge-list.el" (0 0 0 0))
;;; Generated autoloads from forge-list.el

(autoload 'forge-list-issues "forge-list" "\
List issues in a separate buffer.

\(fn REPO)" t nil)

(autoload 'forge-list-pullreqs "forge-list" "\
List pull-requests in a separate buffer.

\(fn REPO)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "forge-list" '("forge-")))

;;;***

;;;### (autoloads nil "forge-notify" "forge-notify.el" (0 0 0 0))
;;; Generated autoloads from forge-notify.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "forge-notify" '("forge-")))

;;;***

;;;### (autoloads nil "forge-post" "forge-post.el" (0 0 0 0))
;;; Generated autoloads from forge-post.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "forge-post" '("forge-")))

;;;***

;;;### (autoloads nil "forge-pullreq" "forge-pullreq.el" (0 0 0 0))
;;; Generated autoloads from forge-pullreq.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "forge-pullreq" '("forge-")))

;;;***

;;;### (autoloads nil "forge-repo" "forge-repo.el" (0 0 0 0))
;;; Generated autoloads from forge-repo.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "forge-repo" '("forge-")))

;;;***

;;;### (autoloads nil "forge-revnote" "forge-revnote.el" (0 0 0 0))
;;; Generated autoloads from forge-revnote.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "forge-revnote" '("forge-revnote")))

;;;***

;;;### (autoloads nil "forge-semi" "forge-semi.el" (0 0 0 0))
;;; Generated autoloads from forge-semi.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "forge-semi" '("forge-")))

;;;***

;;;### (autoloads nil "forge-topic" "forge-topic.el" (0 0 0 0))
;;; Generated autoloads from forge-topic.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "forge-topic" '("forge-" "bug-reference-fontify")))

;;;***

;;;### (autoloads nil nil ("forge-pkg.el" "forge.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; forge-autoloads.el ends here
