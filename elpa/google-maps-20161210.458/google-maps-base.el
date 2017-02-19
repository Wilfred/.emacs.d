;;; google-maps-static.el --- Access Google Maps Static from Emacs

;; Copyright (C) 2010 Julien Danjou

;; Author: Julien Danjou <julien@danjou.info>
;; Keywords: comm

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Functions and data used by Google Maps submodules.

;;; Code:

(require 'cl-lib)

(defgroup google-maps nil
  "Google Maps."
  :group 'comm)

(defcustom google-maps-default-sensor nil
  "Default sensor value for map request."
  :group 'google-maps
  :type 'boolean)

(defcustom google-maps-cache-ttl 86400
  "Defaut TTL for cache, in seconds."
  :group 'google-maps
  :type 'integer)

(defun mapconcat-if-not (predicate function sequence separator)
  "Apply FUNCTION to each element of SEQUENCE, and concat the results as strings if they validate PREDICATE.
In between each pair of results, stick in SEPARATOR.  Thus, \" \"
as SEPARATOR results in spaces between the values returned by
FUNCTION.  SEQUENCE may be a list, a vector, a bool-vector, or a
string."
  (mapconcat
   'identity
   (cl-remove-if predicate
                 (mapcar
                  function
                  sequence))
   separator))

(defun google-maps-plist-delete (plist property)
  "Delete PROPERTY from PLIST."
  (let (p)
    (while plist
      (unless (eq property (car plist))
        (setq p (plist-put p (car plist) (nth 1 plist))))
      (setq plist (cddr plist)))
    p))

(defun google-maps-symbol-to-property (symbol)
  "Transform SYMBOL to :SYMBOL."
  (intern-soft (concat ":" (symbol-name symbol))))

(defun google-maps-build-plist (plist)
  "Build a property list based on PLIST."
  (unless (plist-member plist :sensor)
    (plist-put plist :sensor google-maps-default-sensor))
  plist)

(defun google-maps-cache-expired (url expire-time)
  "Check if URL is cached for more than EXPIRE-TIME."
  (cond (url-standalone-mode
         (not (file-exists-p (url-cache-create-filename url))))
        (t (let ((cache-time (url-is-cached url)))
             (if cache-time
                 (time-less-p
                  (time-add
                   cache-time
                   (seconds-to-time expire-time))
                  (current-time))
               t)))))

(defun google-maps-static-cache-fetch (url)
  "Fetch URL from the cache."
  (with-current-buffer (generate-new-buffer " *temp*")
    (url-cache-extract (url-cache-create-filename url))
    (current-buffer)))

(defun google-maps-retrieve-data (url &optional expire-time)
  "Retrieve URL and return its data as string.
If EXPIRE-TIME is set, the data will be fetched from the cache if
their are not older than EXPIRE-TIME seconds. Otherwise, they
will be fetched and then cached. Therefore, setting EXPIRE-TIME
to 0 force a cache renewal."
  (let* ((expired (if expire-time
                      (google-maps-cache-expired url expire-time)
                    t))
         (buffer (if expired
                     (url-retrieve-synchronously url)
                   (google-maps-static-cache-fetch url)))
         data)
    (with-current-buffer buffer
      (goto-char (point-min))
      (search-forward "\n\n")
      (let ((headers (buffer-substring (point-min) (point))))
        (unless (string-match-p "^HTTP/[0-9]\\(?:\.[0-9]\\)+ 200 OK" headers)
          (kill-buffer)
          (error "Unable to fetch data"))
        (if (string-match-p "^Content-Type: .+; charset=UTF-8$" headers)
            (set-buffer-multibyte t)
          (set-buffer-multibyte nil)))
      (setq data (buffer-substring (point) (point-max)))
      (when (and expired expire-time)
        (url-store-in-cache (current-buffer)))
      (kill-buffer (current-buffer))
      data)))

(defun google-maps-urlencode-plist (plist properties &optional eqs separator)
  "Encode PLIST for a URL using PROPERTIES.
PROPERTIES should have form '((property-name . format))."
  (let ((eqs (or eqs "="))
        (separator (or separator "&")))
    (mapconcat-if-not
     'null
     (lambda (entry)
       (let* ((property (car entry))
              (propsym (google-maps-symbol-to-property property))
              (value (plist-get plist propsym))
              (value-format (or (cdr entry) 'identity))
              ;; If value-format is list or function, eval
              (value (cond ((functionp value-format) (funcall
                                                      value-format
                                                      value))
                           (t (eval value-format)))))
         (when value
           (format "%s%s%s" property eqs value))))
     properties
     separator)))

(provide 'google-maps-base)

;;; google-maps-base.el ends here
