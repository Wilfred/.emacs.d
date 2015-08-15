;;; python-django-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "python-django" "python-django.el" (21967 8669
;;;;;;  360629 132000))
;;; Generated autoloads from python-django.el

(autoload 'python-django-open-project "python-django" "\
Open a Django project at given DIRECTORY using SETTINGS.
Optional argument EXISTING is internal and should not be used.

The recommended way to chose your project root, is to use the
directory containing your settings module; for instance if your
settings module is in /path/django/settings.py, use /path/django/
as your project path and django.settings as your settings module.

When called with no `prefix-arg', this function will try to find
an opened project-buffer, if current buffer is already a project
buffer it will cycle to next opened project.  If no project
buffers are found, then the user prompted for the project path
and settings module unless `python-django-project-root' and
`python-django-project-settings' are somehow set, normally via
directory local variables.  If none of the above matched or the
function is called with one `prefix-arg' and there are projects
defined in the `python-django-known-projects' variable the user
is prompted for any of those known projects, if the variable
turns to be nil the user will be prompted for project-path and
settings module (the same happens when called with two or more
`prefix-arg').

\(fn DIRECTORY SETTINGS &optional EXISTING)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; python-django-autoloads.el ends here
