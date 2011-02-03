;;; mumamo.el --- Multiple major modes in a buffer
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Maintainer:
;; Created: Fri Mar 09 2007
(defconst mumamo:version "0.91") ;;Version:
;; Last-Updated: 2009-10-19 Mon
;; URL: http://OurComments.org/Emacs/Emacs.html
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;; `appmenu', `apropos', `backquote', `button', `bytecomp', `cl',
;; `comint', `compile', `easymenu', `flyspell', `grep', `ido',
;; `ispell', `mail-prsvr', `mlinks', `mm-util', `nxml-enc',
;; `nxml-glyph', `nxml-mode', `nxml-ns', `nxml-outln',
;; `nxml-parse', `nxml-rap', `nxml-util', `ourcomments-util',
;; `recentf', `ring', `rng-dt', `rng-loc', `rng-match',
;; `rng-parse', `rng-pttrn', `rng-uri', `rng-util', `rng-valid',
;; `rx', `sgml-mode', `timer', `tool-bar', `tree-widget',
;; `url-expand', `url-methods', `url-parse', `url-util',
;; `url-vars', `wid-edit', `xmltok'.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;; Commentary:
;;
;; In some cases you may find that it is quite hard to write one major
;; mode that does everything for the type of file you want to handle.
;; That is the case for example for a PHP file where there comes
;; useful major modes with Emacs for the html parts, and where you can
;; get a major mode for PHP from other sources (see EmacsWiki for
;; Aaron Hawleys php-mode.el, or the very similar version that comes
;; with nXhtml).
;;
;; Using one major mode for the HTML part and another for the PHP part
;; sounds like a good solution.  But this means you want to use (at
;; least) two major modes in the same buffer.
;;
;; This file implements just that, support for MUltiple MAjor MOdes
;; (mumamo) in a buffer.
;;
;;
;;;; Usage:
;;
;; The multiple major mode support is turned on by calling special
;; functions which are used nearly the same way as major modes.  See
;; `mumamo-defined-multi-major-modes' for more information about those
;; functions.
;;
;; Each such function defines how to take care of a certain mix of
;; major functions in the buffer. We call them "multi major modes".
;;
;; You may call those functions directly (like you can with major mode
;; functions) or you may use them in for example `auto-mode-alist'.
;;
;; You can load mumamo in your .emacs with
;;
;;   (require 'mumamo-fun)
;;
;; or you can generate an autoload file from mumamo-fun.el
;;
;; Note that no multi major mode functions are defined in this file.
;; Together with this file comes the file mumamo-fun.el that defines
;; some such functions.  All those functions defined in that file are
;; marked for autoload.
;;
;;
;;
;; Thanks to Stefan Monnier for beeing a good and knowledgeable
;; speaking partner for some difficult parts while I was trying to
;; develop this.
;;
;; Thanks to RMS for giving me support and ideas about the programming
;; interface.  That simplified the code and usage quite a lot.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;; How to add support for a new mix of major modes
;;
;; This is done by creating a new function using
;; `define-mumamo-multi-major-mode'.  See that function for more
;; information.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;; Information for major mode authors
;;
;; There are a few special requirements on major modes to make them
;; work with mumamo:
;;
;; - fontification-functions should be '(jit-lock-function). However
;;   nxml-mode derivates can work too, see the code for more info.
;;
;; - narrowing should be respected during fontification and
;;   indentation when font-lock-dont-widen is non-nil.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;; Information for minor mode authors
;;
;; Some minor modes are written to be specific for the file edited in
;; the buffer and some are written to be specific for a major
;; modes.  Others are emulating another editor.  Those are probably
;; global, but might still have buffer local values.
;;
;; Those minor modes that are not meant to be specific for a major
;; mode should probably survive changing major mode in the
;; buffer.  That is mostly not the case in Emacs today.
;;
;; There are (at least) two type of values for those minor modes that
;; sometimes should survive changing major mode: buffer local
;; variables and functions added locally to hooks.
;;
;; * Some buffer local variables are really that - buffer local. Other
;;   are really meant not for the buffer but for the major mode or
;;   some minor mode that is local to the buffer.
;;
;;   If the buffer local variable is meant for the buffer then it is
;;   easy to make them survive changing major mode: just add
;;
;;    (put 'VARIABLE 'permanent-local t)
;;
;;   to those variables.  That will work regardless of the way major
;;   mode is changed.
;;
;;   If one only wants the variables to survive the major mode change
;;   that is done when moving between chunks with different major
;;   modes then something different must be used.  To make a variable
;;   survive this, but not a major mode change for the whole buffer,
;;   call any the function `mumamo-make-variable-buffer-permanent':
;;
;;     (mumamo-make-variable-buffer-permanent 'VARIABLE)
;;
;; * For functions entered to local hooks use this
;;
;;     (put 'FUNSYM 'permanent-local-hook t)
;;     (add-hook 'HOOKSYM 'FUNSYM nil t)
;;
;;   where HOOKSYM is the hook and FUNSYM is the function.
;;
;; * Some functions that are run in `change-major-mode' and dito
;;   after- must be avoided when mumamo changes major mode.  The
;;   functions to avoid should be listed in
;;
;;     `mumamo-change-major-mode-no-nos'
;;     `mumamo-after-change-major-mode-no-nos'
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;; Comments on code etc:
;;
;; This is yet another way to try to get different major modes for
;; different chunks of a buffer to work.  (I borrowed the term "chunk"
;; here from multi-mode.el.)  I am aware of two main previous elisp
;; packages that tries to do this, multi-mode.el and mmm-mode.el.
;; (See http://www.emacswiki.org/cgi-bin/wiki/MultipleModes where
;; there are also some other packages mentioned.)  The solutions in
;; those are a bit different from the approach here.
;;
;; The idea of doing it the way mumamo does it is of course based on a
;; hope that switching major mode when moving between chunks should be
;; quick.  I found that it took from 0 - 62 000 ms, typically 0 - 16
;; 000 ms on a 3ghz cpu.  However unfortunately this is not the whole
;; truth.  It could take longer time, depending on what is run in the
;; hooks: The major mode specific hook, `after-change-major-mode-hook'
;; and `change-major-mode-hook'.
;;
;; Because it currently may take long enough time switching major mode
;; when moving between chunks to disturb smooth moving around in the
;; buffer I have added a way to let the major mode switching be done
;; after moving when Emacs is idle.  This is currently the default, but
;; see the custom variable `mumamo-set-major-mode-delay'.
;;
;; Since the intention is to set up the new major mode the same way as
;; it should have been done if this was a major mode for the whole
;; buffer these hooks must be run.  However if this idea is developed
;; further some of the things done in these hooks (like switching on
;; minor modes) could perhaps be streamlined so that switching minor
;; modes off and then on again could be avoided.  In fact there is
;; already tools for this in mumamo.el, see the section below named
;; "Information for minor mode authors".
;;
;; Another problem is that the major modes must use
;; `font-lock-fontify-region-function'.  Currently the only major
;; modes I know that does not do this are `nxml-mode' and its
;; derivatives.
;;
;; The indentation is currently working rather ok, but with the price
;; that buffer modified is sometimes set even though there are no
;; actual changes.  That seems a bit unnecessary and it could be
;; avoided if the indentation functions for the the various major
;; modes were rewritten so that you could get the indentation that
;; would be done instead of actually doing the indentation.  (Or
;; mumamo could do this better, but I do not know how right now.)
;;
;; See also "Known bugs and problems etc" below.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;; Known bugs:
;;
;; - See the various FIX-ME for possible bugs.  See also below.
;;
;;
;;;; Known problems and ideas:
;;
;; - There is no way in Emacs to tell a mode not to change
;;   fontification when changing to or from that mode.
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;; Code:

(eval-when-compile (require 'cl))
(eval-when-compile (require 'cc-engine))
(eval-when-compile (require 'desktop))
(eval-when-compile (require 'flyspell))
(eval-when-compile (require 'rngalt nil t))
(eval-when-compile (require 'nxml-mode nil t))
(eval-when-compile
  (when (featurep 'nxml-mode)
    (require 'rng-valid nil t)
    ;;(require 'rngalt nil t)
    ))
(eval-when-compile (require 'sgml-mode)) ;; For sgml-xml-mode
;; For `define-globalized-minor-mode-with-on-off':
;;(require 'ourcomments-util)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rng-valid.el support

(defvar rng-get-major-mode-chunk-function nil
  "Function to use to get major mode chunk.
It should take one argument, the position where to get the major
mode chunk.

This is to be set by multiple major mode frame works, like
mumamo.

See also `rng-valid-nxml-major-mode-chunk-function' and
`rng-end-major-mode-chunk-function'.  Note that all three
variables must be set.")
(make-variable-buffer-local 'rng-get-major-mode-chunk-function)
(put 'rng-get-major-mode-chunk-function 'permanent-local t)

(defvar rng-valid-nxml-major-mode-chunk-function nil
  "Function to use to check if nxml can parse major mode chunk.
It should take one argument, the chunk.

For more info see also `rng-get-major-mode-chunk-function'.")
(make-variable-buffer-local 'rng-valid-nxml-major-mode-chunk-function)
(put 'rng-valid-nxml-major-mode-chunk-function 'permanent-local t)

(defvar rng-end-major-mode-chunk-function nil
  "Function to use to get the end of a major mode chunk.
It should take one argument, the chunk.

For more info see also `rng-get-major-mode-chunk-function'.")
(make-variable-buffer-local 'rng-end-major-mode-chunk-function)
(put 'rng-end-major-mode-chunk-function 'permanent-local t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Some variables

(defvar mumamo-major-mode-indent-line-function nil)
(make-variable-buffer-local 'mumamo-major-mode-indent-line-function)

(defvar mumamo-buffer-locals-per-major nil)
(make-variable-buffer-local 'mumamo-buffer-locals-per-major)
(put 'mumamo-buffer-locals-per-major 'permanent-local t)

(defvar mumamo-just-changed-major nil
  "Avoid refontification when switching major mode.
Set to t by `mumamo-set-major'.  Checked and reset to nil by
`mumamo-jit-lock-function'.")
(make-variable-buffer-local 'mumamo-just-changed-major)

(defvar mumamo-multi-major-mode nil
  "The function that handles multiple major modes.
If this is nil then multiple major modes in the buffer is not
handled by mumamo.

Set by functions defined by `define-mumamo-multi-major-mode'.")
(make-variable-buffer-local 'mumamo-multi-major-mode)
(put 'mumamo-multi-major-mode 'permanent-local t)

(defvar mumamo-set-major-running nil
  "Internal use.  Handling of mumamo turn off.")

(defun mumamo-chunk-car (chunk prop)
  (car (overlay-get chunk prop)))

(defun mumamo-chunk-cadr (chunk prop)
  (cadr (overlay-get chunk prop)))

;; (let ((l '(1 2))) (setcar (nthcdr 1 l) 10) l)
;; setters
(defsubst mumamo-chunk-value-set-min (chunk-values min)
  "In CHUNK-VALUES set min value to MIN.
CHUNK-VALUES should have the format return by
`mumamo-create-chunk-values-at'."
  (setcar (nthcdr 0 chunk-values) min))
(defsubst mumamo-chunk-value-set-max (chunk-values max)
  "In CHUNK-VALUES set max value to MAX.
See also `mumamo-chunk-value-set-min'."
  (setcar (nthcdr 1 chunk-values) max))
(defsubst mumamo-chunk-value-set-syntax-min (chunk-values min)
  "In CHUNK-VALUES set min syntax diff value to MIN.
See also `mumamo-chunk-value-set-min'."
  (setcar (nthcdr 3 chunk-values) min))
(defsubst mumamo-chunk-value-set-syntax-max (chunk-values max)
  "In CHUNK-VALUES set max syntax diff value to MAX.
See also `mumamo-chunk-value-set-min'."
  (setcar (nthcdr 3 chunk-values) max))
;; getters
(defsubst mumamo-chunk-value-min    (chunk-values)
  "Get min value from CHUNK-VALUES.
See also `mumamo-chunk-value-set-min'."
  (nth 0 chunk-values))
(defsubst mumamo-chunk-value-max    (chunk-values)
  "Get max value from CHUNK-VALUES.
See also `mumamo-chunk-value-set-min'."
  (nth 1 chunk-values))
(defsubst mumamo-chunk-value-major  (chunk-values)
  "Get major value from CHUNK-VALUES.
See also `mumamo-chunk-value-set-min'."
  (nth 2 chunk-values))
(defsubst mumamo-chunk-value-syntax-min    (chunk-values)
  "Get min syntax diff value from CHUNK-VALUES.
See also `mumamo-chunk-value-set-min'."
  (nth 3 chunk-values))
(defsubst mumamo-chunk-value-syntax-max    (chunk-values)
  "Get max syntax diff value from CHUNK-VALUES.
See also `mumamo-chunk-value-set-min'."
  (nth 4 chunk-values))
(defsubst mumamo-chunk-value-parseable-by    (chunk-values)
  "Get parseable-by from CHUNK-VALUES.
See also `mumamo-chunk-value-set-min'.
For parseable-by see `mumamo-find-possible-chunk'."
  (nth 5 chunk-values))
;; (defsubst mumamo-chunk-prev-chunk (chunk-values)
;;   "Get previous chunk from CHUNK-VALUES.
;; See also `mumamo-chunk-value-set-min'."
;;   (nth 6 chunk-values))
(defsubst mumamo-chunk-value-fw-exc-fun (chunk-values)
  "Get function that find chunk end from CHUNK-VALUES.
See also `mumamo-chunk-value-set-min'."
  (nth 6 chunk-values))

(defsubst mumamo-chunk-major-mode (chunk)
  "Get major mode specified in CHUNK."
  ;;(assert chunk)
  ;;(assert (overlay-buffer chunk))
  (let ((mode-spec (if chunk
                       (mumamo-chunk-car chunk 'mumamo-major-mode)
                     (mumamo-main-major-mode))))
    (mumamo-major-mode-from-modespec mode-spec)))

(defsubst mumamo-chunk-syntax-min-max (chunk no-obscure)
  (when chunk
    (let* ((ovl-end   (overlay-end chunk))
           (ovl-start (overlay-start chunk))
           (syntax-min (min ovl-end
                            (+ ovl-start
                               (or (overlay-get chunk 'mumamo-syntax-min-d)
                                   0))))
           ;;(dummy (msgtrc "chunk-syntax-min-max:syntax-min=%s, chunk=%S" syntax-min chunk))
           (syntax-max
            (max ovl-start
                 (- (overlay-end chunk)
                    (or (overlay-get chunk 'mumamo-syntax-max-d)
                        0)
                    (if (= (1+ (buffer-size))
                           (overlay-end chunk))
                        0
                      ;; Note: We must subtract one here because
                      ;; overlay-end is +1 from the last point in the
                      ;; overlay.
                      ;;
                      ;; This cured the problem with
                      ;; kubica-freezing-i.html that made Emacs loop
                      ;; in `font-lock-extend-region-multiline'. But
                      ;; was it really this one, I can't find any
                      ;; 'font-lock-multiline property.  So it should
                      ;; be `font-lock-extend-region-whole-lines'.
                      ;;
                      ;; Should not the problem then be the value of font-lock-end?
                      ;;
                      ;; Fix-me: however this is not correct since it
                      ;; leads to not fontifying the last character in
                      ;; the chunk, see bug 531324.
                      ;;
                      ;; I think this is cured by now. I have let
                      ;; bound `font-lock-extend-region-functions'
                      ;; once more before the call to
                      ;; `font-lock-fontify-region'.
                      0
                      ;;0
                      ))))
           (obscure (unless no-obscure (overlay-get chunk 'obscured)))
           (region-info (cadr obscure))
           (obscure-min (car region-info))
           (obscure-max (cdr region-info))
           ;;(dummy (message "syn-mn-mx:obs=%s r-info=%s ob=%s/%s" obscure region-info obscure-min obscure-max ))
           (actual-min (max (or obscure-min ovl-start)
                            (or syntax-min ovl-start)))
           (actual-max (min (or obscure-max ovl-end)
                            (or syntax-max ovl-end)))
           (maj (mumamo-chunk-car chunk 'mumamo-major-mode))
           ;;(dummy (message "syn-mn-mx:obs=%s r-info=%s ob=%s/%s ac=%s/%s" obscure region-info obscure-min obscure-max actual-min actual-max))
           )
      (cons actual-min actual-max))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Macros

;; Borrowed from font-lock.el
(defmacro mumamo-save-buffer-state (varlist &rest body)
  "Bind variables according to VARLIST and eval BODY restoring buffer state.
Do not record undo information during evaluation of BODY."
  (declare (indent 1) (debug let))
  (let ((modified (make-symbol "modified")))
    `(let* ,(append varlist
                    `((,modified (buffer-modified-p))
                      (buffer-undo-list t)
                      (inhibit-read-only t)
                      (inhibit-point-motion-hooks t)
                      (inhibit-modification-hooks t)
                      deactivate-mark
                      buffer-file-name
                      buffer-file-truename))
       (progn
         ,@body)
       (unless ,modified
         (restore-buffer-modified-p nil)))))

;; From jit-lock.el:
(defmacro mumamo-jit-with-buffer-unmodified (&rest body)
  "Eval BODY, preserving the current buffer's modified state."
  (declare (debug t))
  (let ((modified (make-symbol "modified")))
    `(let ((,modified (buffer-modified-p)))
       (unwind-protect
           (progn ,@body)
         (unless ,modified
           (restore-buffer-modified-p nil))))))

(defmacro mumamo-with-buffer-prepared-for-jit-lock (&rest body)
  "Execute BODY in current buffer, overriding several variables.
Preserves the `buffer-modified-p' state of the current buffer."
  (declare (debug t))
  `(mumamo-jit-with-buffer-unmodified
    (let ((buffer-undo-list t)
          (inhibit-read-only t)
          (inhibit-point-motion-hooks t)
          (inhibit-modification-hooks t)
          deactivate-mark
          buffer-file-name
          buffer-file-truename)
      ,@body)))

(defmacro mumamo-condition-case (var body-form &rest handlers)
  "Like `condition-case', but optional.
If `mumamo-use-condition-case' is non-nil then do

  (condition-case VAR
      BODY-FORM
    HANDLERS).

Otherwise just evaluate BODY-FORM."
  (declare (indent 2) (debug t))
  `(if (not mumamo-use-condition-case)
       (let* ((debugger (or mumamo-debugger 'debug))
              (debug-on-error (if debugger t debug-on-error)))
         ,body-form)
     (condition-case ,var
         ,body-form
       ,@handlers)))

(defmacro mumamo-msgfntfy (format-string &rest args)
  "Give some messages during fontification.
This macro should just do nothing during normal use.  However if
there are any problems you can uncomment one of the lines in this
macro and recompile/reeval mumamo.el to get those messages.

You have to search the code to see where you will get them.  All
uses are in this file.

FORMAT-STRING and ARGS have the same meaning as for the function
`message'."
  ;;(list 'apply (list 'quote 'msgtrc) format-string (append '(list) args))
  ;;(list 'apply (list 'quote 'message) format-string (append '(list) args))
  ;;(list 'progn 'apply (list 'quote 'message) format-string (append '(list) args) nil)
  ;; (condition-case err
  ;; (list 'apply (list 'quote 'message) format-string (append '(list) args)) ;; <--
  ;; (error (message "err in msgfntfy %S" err)))
  ;;(message "%s %S" format-string args)
  ;;(list 'apply (list 'quote 'message) (list 'concat "%s: " format-string)
  ;;   (list 'get-internal-run-time) (append '(list) args))
  )
;;(mumamo-msgfntfy "my-format=%s" (get-internal-run-time))

(defmacro mumamo-msgindent (format-string &rest args)
  "Give some messages during indentation.
This macro should just do nothing during normal use.  However if
there are any problems you can uncomment one of the lines in this
macro and recompile/reeval mumamo.el to get those messages.

You have to search the code to see where you will get them.  All
uses are in this file.

FORMAT-STRING and ARGS have the same meaning as for the function
`message'."
  ;;(list 'apply (list 'quote 'msgtrc) format-string (append '(list) args))
  ;;(list 'apply (list 'quote 'message) format-string (append '(list) args)) ;; <---
  ;;(list 'apply (list 'quote 'message) (list 'concat "%s: " format-string)
  ;;   (list 'get-internal-run-time) (append '(list) args))
  )

(defmacro mumamo-with-major-mode-setup (major for-what &rest body)
  "Run code with some local variables set as in specified major mode.
Set variables as needed for major mode MAJOR when doing FOR-WHAT
and then run BODY using `with-syntax-table'.

FOR-WHAT is used to choose another major mode than MAJOR in
certain cases.  It should be 'fontification or 'indentation.

Note: We must let-bind the variables here instead of make them buffer
local since they otherwise could be wrong at \(point) in top
level \(ie user interaction level)."
  (declare (indent 2) (debug t))
  `(let ((need-major-mode (mumamo-get-major-mode-substitute ,major ,for-what)))
     ;;(msgtrc "mumamo-with-major-mode-setup %s => %s, modified=%s" ,major need-major-mode (buffer-modified-p))
     ;;(msgtrc "with-major-mode-setup <<<<<<<<<< body=%S\n>>>>>>>>>>" '(progn ,@body))
     ;;(msgtrc "with-major-mode-setup:in buffer %s after-chunk=%s" (current-buffer) (when (boundp 'after-chunk) after-chunk))
     (let ((major-mode need-major-mode)
           (evaled-set-mode (mumamo-get-major-mode-setup need-major-mode)))
       ;;(message ">>>>>> before %s" evaled-set-mode)
       ;;(message ">>>>>> before %s, body=%s" evaled-set-mode (list ,@body))
       (funcall (symbol-value evaled-set-mode)
                (list 'progn
                      ,@body))
       ;;(mumamo-msgfntfy "<<<<<< after evaled-set-mode modified=%s" (buffer-modified-p))
       )))

(defmacro mumamo-with-major-mode-fontification (major &rest body)
  "With fontification variables set as major mode MAJOR eval BODY.
This is used during font locking and indentation.  The variables
affecting those are set as they are in major mode MAJOR.

See the code in `mumamo-fetch-major-mode-setup' for exactly which
local variables that are set."
  (declare (indent 1) (debug t))
  `(mumamo-with-major-mode-setup ,major 'fontification
     ,@body))
;; Fontification disappears in for example *grep* if
;; font-lock-mode-major-mode is 'permanent-local t.
;;(put 'font-lock-mode-major-mode 'permanent-local t)

(defmacro mumamo-with-major-mode-indentation (major &rest body)
  "With indentation variables set as in another major mode do things.
Same as `mumamo-with-major-mode-fontification' but for
indentation.  See that function for some notes about MAJOR and
BODY."
  (declare (indent 1) (debug t))
  `(mumamo-with-major-mode-setup ,major 'indentation ,@body))

;; fix-me: tell no sub-chunks in sub-chunks
;;;###autoload
(defmacro define-mumamo-multi-major-mode (fun-sym spec-doc chunks)
  "Define a function that turn on support for multiple major modes.
Define a function FUN-SYM that set up to divide the current
buffer into chunks with different major modes.

The documentation string for FUN-SYM should contain the special
documentation in the string SPEC-DOC, general documentation for
functions of this type and information about chunks.

The new function will use the definitions in CHUNKS \(which is
called a \"chunk family\") to make the dividing of the buffer.

The function FUN-SYM can be used to setup a buffer instead of a
major mode function:

- The function FUN-SYM can be called instead of calling a major
  mode function when you want to use multiple major modes in a
  buffer.

- The defined function can be used instead of a major mode
  function in for example `auto-mode-alist'.

- As the very last thing FUN-SYM will run the hook FUN-SYM-hook,
  just as major modes do.

- There is also a general hook, `mumamo-turn-on-hook', which is
  run when turning on mumamo with any of these functions.  This
  is run right before the hook specific to any of the functions
  above that turns on the multiple major mode support.

- The multi major mode FUN-SYM has a keymap named FUN-SYM-map.
  This overrides the major modes' keymaps since it is handled as
  a minor mode keymap.

- There is also a special mumamo keymap, `mumamo-map' that is
  active in every buffer with a multi major mode.  This is also
  handled as a minor mode keymap and therefor overrides the major
  modes' keymaps.

- However when this support for multiple major mode is on the
  buffer is divided into chunks, each with its own major mode.

- The chunks are fontified according the major mode assigned to
  them for that.

- Indenting is also done according to the major mode assigned to
  them for that.

- The actual major mode used in the buffer is changed to the one
  in the chunk when moving point between these chunks.

- When major mode is changed the hooks for the new major mode,
  `after-change-major-mode-hook' and `change-major-mode-hook' are
  run.

- There will be an alias for FUN-SYM called mumamo-alias-FUN-SYM.
  This can be used to check whic multi major modes have been
  defined.

** A little bit more technical description:

The dividing of a buffer into chunks is done during fontification
by `mumamo-get-chunk-at'.

The name of the function is saved in in the buffer local variable
`mumamo-multi-major-mode' when the function is called.

All functions defined by this macro is added to the list
`mumamo-defined-multi-major-modes'.

Basically Mumamo handles only major modes that uses jit-lock.
However as a special effort also `nxml-mode' and derivatives
thereof are handled.  Since it seems impossible to me to restrict
those major modes fontification to only a chunk without changing
`nxml-mode' the fontification is instead done by
`html-mode'/`sgml-mode' for chunks using `nxml-mode' and its
derivates.

CHUNKS is a list where each entry have the format

  \(CHUNK-DEF-NAME MAIN-MAJOR-MODE SUBMODE-CHUNK-FUNCTIONS)

CHUNK-DEF-NAME is the key name by which the entry is recognized.
MAIN-MAJOR-MODE is the major mode used when there is no chunks.
If this is nil then `major-mode' before turning on this mode will
be used.

SUBMODE-CHUNK-FUNCTIONS is a list of the functions that does the
chunk division of the buffer.  They are tried in the order they
appear here during the chunk division process.

If you want to write new functions for chunk divisions then
please see `mumamo-find-possible-chunk'.  You can perhaps also
use `mumamo-quick-static-chunk' which is more easy-to-use
alternative.  See also the file mumamo-fun.el where there are
many routines for chunk division.

When you write those new functions you may want to use some of
the functions for testing chunks:

 `mumamo-test-create-chunk-at'  `mumamo-test-create-chunks-at-all'
 `mumamo-test-easy-make'        `mumamo-test-fontify-region'

These are in the file mumamo-test.el."
  ;;(let ((c (if (symbolp chunks) (symbol-value chunks) chunks))) (message "c=%S" c))
  (let* (;;(mumamo-describe-chunks (make-symbol "mumamo-describe-chunks"))
         (turn-on-fun (if (symbolp fun-sym)
                          fun-sym
                        (error "Parameter FUN-SYM must be a symbol")))
         (turn-on-fun-alias (intern (concat "mumamo-alias-" (symbol-name fun-sym))))
         ;; Backward compatibility nXhtml v 1.60
         (turn-on-fun-old (when (string= (substring (symbol-name fun-sym) -5)
                                         "-mode")
                            (intern (substring (symbol-name fun-sym) 0 -5))))
         (turn-on-hook (intern (concat (symbol-name turn-on-fun) "-hook")))
         (turn-on-map  (intern (concat (symbol-name turn-on-fun) "-map")))
         (turn-on-hook-doc (concat "Hook run at the very end of `"
                                   (symbol-name turn-on-fun) "'."))
         (chunks2 (if (symbolp chunks)
                      (symbol-value chunks)
                    chunks))
         (docstring
          (concat
           spec-doc
           "



This function is called a multi major mode.  It sets up for
multiple major modes in the buffer in the following way:

"
           ;; Fix-me: During byte compilation the next line is not
           ;; expanded as I thought because the functions in CHUNKS
           ;; are not defined. How do I fix this?  Move out the
           ;; define-mumamo-multi-major-mode calls?
           (funcall 'mumamo-describe-chunks chunks2)
           "
At the very end this multi major mode function runs first the hook
`mumamo-turn-on-hook' and then `" (symbol-name turn-on-hook) "'.

There is a keymap specific to this multi major mode, but it is
not returned by `current-local-map' which returns the chunk's
major mode's local keymap.

The multi mode keymap is named `" (symbol-name turn-on-map) "'.



The main use for a multi major mode is to use it instead of a
normal major mode in `auto-mode-alist'.  \(You can of course call
this function directly yourself too.)

The value of `mumamo-multi-major-mode' tells you which multi
major mode if any has been turned on in a buffer.  For more
information about multi major modes please see
`define-mumamo-multi-major-mode'.

Note: When adding new font-lock keywords for major mode chunks
you should use the function `mumamo-refresh-multi-font-lock'
afterwards.
"  )))
    `(progn
       ;;(add-to-list 'mumamo-defined-multi-major-modes (cons (car ',chunks2) ',turn-on-fun))
       (mumamo-add-to-defined-multi-major-modes (cons (car ',chunks2) ',turn-on-fun))
       (defvar ,turn-on-hook nil ,turn-on-hook-doc)
       (defvar ,turn-on-map (make-sparse-keymap)
         ,(concat "Keymap for multi major mode function `"
                  (symbol-name turn-on-fun) "'"))
       (defvar ,turn-on-fun nil)
       (make-variable-buffer-local ',turn-on-fun)
       (put ',turn-on-fun 'permanent-local t)
       (put ',turn-on-fun 'mumamo-chunk-family (copy-tree ',chunks2))
       (put ',turn-on-fun-alias 'mumamo-chunk-family (copy-tree ',chunks2))
       (defun ,turn-on-fun nil ,docstring
         (interactive)
         (let ((old-major-mode (or mumamo-major-mode
                                   major-mode)))
           (kill-all-local-variables)
           (run-hooks 'change-major-mode-hook)
           (setq mumamo-multi-major-mode ',turn-on-fun)
           (setq ,turn-on-fun t)
           (mumamo-add-multi-keymap ',turn-on-fun ,turn-on-map)
           (setq mumamo-current-chunk-family (copy-tree ',chunks2))
           (mumamo-turn-on-actions old-major-mode)
           (run-hooks ',turn-on-hook)))
       (defalias ',turn-on-fun-alias ',turn-on-fun)
       (when (intern-soft ',turn-on-fun-old)
         (defalias ',turn-on-fun-old ',turn-on-fun))
       )))

;;;###autoload
(defun mumamo-add-to-defined-multi-major-modes (entry)
  (add-to-list 'mumamo-defined-multi-major-modes entry))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Debugging etc

(defsubst mumamo-while (limit counter where)
  (let ((count (symbol-value counter)))
    (if (= count limit)
        (progn
          (msgtrc "Reached (while limit=%s, where=%s)" limit where)
          nil)
      (set counter (1+ count)))))

;; (defun dbg-smarty-err ()
;; ;;   (insert "}{")

;; ;;   (insert "}{")
;; ;;   (backward-char)
;; ;;   (backward-char)
;; ;;   (search-backward "}")

;;   ;; This gives an error rather often, but not always:
;;   (delete-char 3)
;;   (search-backward "}")
;;   )

;; (defun dbg-smarty-err2 ()
;;   (forward-char 5)
;;   (insert "}{")
;;   ;; Start in nxhtml part and make sure the insertion is in smarty
;;   ;; part.  Gives reliably an error if moved backward so point stay in
;;   ;; the new nxhtml-mode part, otherwise not.
;;   ;;
;;   ;; Eh, no.  If chunk family is changed and reset there is no more an
;;   ;; error.
;;   ;;
;;   ;; Seems to be some race condition, but I am unable to understand
;;   ;; how.  I believed that nxml always left in a reliable state.  Is
;;   ;; this a state problem in mumamo or nxml? I am unable to make it
;;   ;; happen again now.
;;   ;;
;;   ;; I saw one very strange thing: The error message got inserted in
;;   ;; the .phps buffer once.  How could this happen? Is this an Emacs
;;   ;; bug? Can't see how this could happen since it is the message
;;   ;; function that outputs the message.  A w32 race condition? Are
;;   ;; people aware that the message queue runs in parallell? (I have
;;   ;; tried to ask on the devel list, but got no answer at that time.)
;;   (backward-char 2)
;;   )


(defvar msgtrc-buffer
  "*Messages*"
  ;;"*trace-output*"
  "Buffer or name of buffer for trace messages.
See `msgtrc'."
  )

(defun msgtrc (format-string &rest args)
  "Print message to `msgtrc-buffer'.
Arguments FORMAT-STRING and ARGS are like for `message'."
  (if nil
      nil ;;(apply 'message format-string args)
    ;; bug#3350 prevents use of this:
    (let ((trc-buffer (get-buffer-create msgtrc-buffer))
          ;; Cure 3350: Stop insert from deactivating the mark
          (deactivate-mark))
      (with-current-buffer trc-buffer
        (goto-char (point-max))
        (insert "MU:" (apply 'format format-string args) "\n")
        ;;(insert "constant string\n")
        (when buffer-file-name (write-region nil nil buffer-file-name))))))

(defvar mumamo-message-file-buffer nil)
(defsubst mumamo-msgtrc-to-file ()
  "Start writing message to file. Erase `msgtrc-buffer' first."
  (unless mumamo-message-file-buffer
    (setq mumamo-message-file-buffer (find-file-noselect "c:/emacs/bugs/temp-messages.txt"))
    (setq msgtrc-buffer mumamo-message-file-buffer)
    (with-current-buffer mumamo-message-file-buffer
      (erase-buffer))))

(defvar mumamo-display-error-lwarn nil
  "Set to t to call `lwarn' on fontification errors.
If this is t then `*Warnings*' buffer will popup on fontification
errors.")
(defvar mumamo-display-error-stop nil
  "Set to t to stop fontification on errors.")

(defun mumamo-message-with-face (msg face)
  "Put MSG with face FACE in *Messages* buffer."
  (let ((start (+ (with-current-buffer msgtrc-buffer
                    (point-max))
                  1))
        ;; This is for the echo area:
        (msg-with-face (propertize (format "%s" msg)
                                   'face face)))

    (msgtrc "%s" msg-with-face)
    ;; This is for the buffer:
    (with-current-buffer msgtrc-buffer
      (goto-char (point-max))
      (backward-char)
      (put-text-property start (point)
                         'face face))))

;;(run-with-idle-timer 1 nil 'mumamo-show-report-message)
(defun mumamo-show-report-message ()
  "Tell the user there is a long error message."
  (save-match-data ;; runs in timer
    (mumamo-message-with-face
     "MuMaMo error, please look in the *Messages* buffer"
     'highlight)))

;; This code can't be used now because `debugger' is currently not
;; useable in timers. I keep it here since I hope someone will make it
;; possible in the future.
;;
;; (defmacro mumamo-get-backtrace-if-error (bodyform)
;;   "Evaluate BODYFORM, return a list with error message and backtrace.
;; If there is an error in BODYFORM then return a list with the
;; error message and the backtrace as a string.  Otherwise return
;; nil."
;;   `(let* ((debugger
;;            (lambda (&rest debugger-args)
;;              (let ((debugger-ret (with-output-to-string (backtrace))))
;;                ;; I believe we must put the result in a buffer,
;;                ;; otherwise `condition-case' might erase it:
;;                (with-current-buffer (get-buffer-create "TEMP GET BACKTRACE")
;;                  (erase-buffer)
;;                  (insert debugger-ret)))))
;;           (debug-on-error t)
;;           (debug-on-signal t))
;;      (mumamo-condition-case err
;;          (progn
;;            ,bodyform
;;            nil)
;;        (error
;;         (let* ((errmsg (error-message-string err))
;;                (dbg1-ret
;;                 (with-current-buffer
;;                     (get-buffer "TEMP GET BACKTRACE") (buffer-string)))
;;                ;; Remove lines from this routine:
;;                (debugger-lines (split-string dbg1-ret "\n"))
;;               (dbg-ret (mapconcat 'identity (nthcdr 6 debugger-lines) "\n"))
;;                )
;;           (list errmsg (concat errmsg "\n" dbg-ret)))))))

;;(mumamo-display-error 'test-lwarn-type "testing 1=%s, 2=%s" "one" 'two)
(defun mumamo-display-error (lwarn-type format-string &rest args)
  "Display a message plus traceback in the *Messages* buffer.
Use this for errors that happen during fontification or when
running a timer.

LWARN-TYPE is used as the type argument to `lwarn' if warnings
are displayed.  FORMAT-STRING and ARGS are used as the
corresponding arguments to `message' and `lwarn'.

All the output from this function in the *Messages* buffer is
displayed with the highlight face.  After the message printed by
`message' is traceback from where this function was called.
Note: There is no error generated, just a traceback that is put
in *Messages* as above.

Display an error message using `message' and colorize it using
the `highlight' face to make it more prominent.  Add a backtrace
colored with the `highlight' face to the buffer *Messages*.  Then
display the error message once again after this so that the user
can see it.

If `mumamo-display-error-lwarn' is non-nil, indicate the error by
calling `lwarn'.  This will display the `*Warnings*' buffer and
thus makes it much more easy to spot that there was an error.

If `mumamo-display-error-stop' is non-nil raise an error that may
stop fontification."

  ;; Warnings are sometimes disturbning, make it optional:
  (when mumamo-display-error-lwarn
    (apply 'lwarn lwarn-type :error format-string args))

  (let ((format-string2 (concat "%s: " format-string))
        (bt (with-output-to-string (backtrace))))

    (mumamo-message-with-face
     (concat
      (apply 'format format-string2 lwarn-type args)
      "\n"
      (format "** In buffer %s\n" (current-buffer))
      bt)
     'highlight)

    ;; Output message once again so the user can see it:
    (apply 'message format-string2 lwarn-type args)
    ;; But ... there might be more messages so wait until things has
    ;; calmed down and then show a message telling that there was an
    ;; error and that there is more information in the *Messages*
    ;; buffer.
    (run-with-idle-timer 1 nil 'mumamo-show-report-message)

    ;; Stop fontifying:
    (when mumamo-display-error-stop
      ;;(font-lock-mode -1)
      (setq font-lock-mode nil)
      (when (timerp jit-lock-context-timer)
        (cancel-timer jit-lock-context-timer))
      (when (timerp jit-lock-defer-timer)
        (cancel-timer jit-lock-defer-timer))
      (apply 'error format-string2 lwarn-type args))))


(defun mumamo-debug-to-backtrace (&rest debugger-args)
  "This function should give a backtrace during fontification errors.
The variable `debugger' should then be this function.  See the
function `debug' for an explanation of DEBUGGER-ARGS.

Fix-me: Can't use this function yet since the display routines
uses safe_eval and safe_call."
  (mumamo-display-error 'mumamo-debug-to-backtrace
                        "%s"
                        (nth 1 debugger-args)))

;; (defun my-test-err3 ()
;;   (interactive)
;;   (let ((debugger 'mumamo-debug-to-backtrace)
;;         (debug-on-error t))
;;     (my-err)
;;     ))
;;(my-test-err3()

;;(set-default 'mumamo-use-condition-case nil)
;;(set-default 'mumamo-use-condition-case t)
(defvar mumamo-use-condition-case t)
(make-variable-buffer-local 'mumamo-use-condition-case)
(put 'mumamo-use-condition-case 'permanent-local t)

(defvar mumamo-debugger 'mumamo-debug-to-backtrace)
(make-variable-buffer-local 'mumamo-debugger)
(put 'mumamo-debugger 'permanent-local t)

;; (defun my-test-err4 ()
;;   (interactive)
;;   (mumamo-condition-case err
;;       (my-errx)
;;     (arith-error (message "here"))
;;     (error (message "%s, %s" err (error-message-string err)))
;;     ))

(defvar mumamo-warned-once nil)
(make-variable-buffer-local 'mumamo-warned-once)
(put 'mumamo-warned-once 'permanent-local t)

                                        ; (append '(0 1) '(a b))
(defun mumamo-warn-once (type message &rest args)
  "Warn only once with TYPE, MESSAGE and ARGS.
If the same problem happens again then do not warn again."
  (let ((msgrec (append (list type message) args)))
    (unless (member msgrec mumamo-warned-once)
      (setq mumamo-warned-once
            (cons msgrec mumamo-warned-once))
      ;;(apply 'lwarn type :warning message args)
      (apply 'message (format "%s: %s" type message) args)
      )))

(defun mumamo-add-help-tabs ()
  "Add key bindings for moving between buttons.
Add bindings similar to those in `help-mode' for moving between
text buttons."
  (local-set-key [tab] 'forward-button)
  (local-set-key [(meta tab)]  'backward-button)
  (local-set-key [(shift tab)] 'backward-button)
  (local-set-key [backtab]     'backward-button))

(defun mumamo-insert-describe-button (symbol type)
  "Insert a text button that describes SYMBOL of type TYPE."
  (let ((func `(lambda (btn)
                 (funcall ',type ',symbol))))
    (mumamo-add-help-tabs)
    (insert-text-button
     (symbol-name symbol)
     :type 'help-function
     'face 'link
     'action func)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Custom group

;;;###autoload
(defgroup mumamo nil
  "Customization group for multiple major modes in a buffer."
  :group 'editing
  :group 'languages
  :group 'sgml
  :group 'nxhtml
  )

;;(setq mumamo-set-major-mode-delay -1)
;;(setq mumamo-set-major-mode-delay 5)
(defcustom mumamo-set-major-mode-delay idle-update-delay
  "Delay this number of seconds before setting major mode.
When point enters a region where the major mode should be
different than the current major mode, wait until Emacs has been
idle this number of seconds before switching major mode.

If negative switch major mode immediately.

Ideally the switching of major mode should occur immediately when
entering a region.  However this can make movements a bit unsmooth
for some major modes on a slow computer.  Therefore on a slow
computer use a short delay.

If you have a fast computer and want to use mode specific
movement commands then set this variable to -1.

I tried to measure the time for switching major mode in mumamo.
For most major modes it took 0 ms, but for `nxml-mode' and its
derivate it took 20 ms on a 3GHz CPU."
  :type 'number
  :group 'mumamo)


(defgroup mumamo-display nil
  "Customization group for mumamo chunk display."
  :group 'mumamo)

(defun mumamo-update-this-buffer-margin-use ()
  (mumamo-update-buffer-margin-use (current-buffer)))

(define-minor-mode mumamo-margin-info-mode
  "Display chunk info in margin when on.
Display chunk depth and major mode where a chunk begin in left or
right margin.  \(The '-mode' part of the major mode is stripped.)

See also `mumamo-margin-use'.

Note: When `linum-mode' is on the right margin is always used
now \(since `linum-mode' uses the left)."
  :group 'mumamo-display
  (mumamo-update-this-buffer-margin-use)
  (if mumamo-margin-info-mode
      (progn
        ;;(add-hook 'window-configuration-change-hook 'mumamo-update-this-buffer-margin-use nil t)
        (add-hook 'linum-mode-hook 'mumamo-update-this-buffer-margin-use nil t)
        )
    ;;(remove-hook 'window-configuration-change-hook 'mumamo-update-this-buffer-margin-use t)
    (remove-hook 'linum-mode-hook 'mumamo-update-this-buffer-margin-use t)
    ))
;;(put 'mumamo-margin-info-mode 'permanent-local t)

(defun mumamo-margin-info-mode-turn-off ()
  (mumamo-margin-info-mode -1))
(put 'mumamo-margin-info-mode-turn-off 'permanent-local-hook t)

(define-globalized-minor-mode mumamo-margin-info-global-mode mumamo-margin-info-mode
  (lambda () (when (and (boundp 'mumamo-multi-major-mode)
                        mumamo-multi-major-mode)
               (mumamo-margin-info-mode 1)))
  :group 'mumamo-display)

(defcustom mumamo-margin-use '(left-margin 13)
  "Display chunk info in left or right margin if non-nil."
  :type '(list (radio (const :tag "Display chunk info in left margin" left-margin)
                      (const :tag "Display chunk info in right margin" right-margin))
               (integer :tag "Margin width (when used)" :value 13))
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'mumamo-update-all-buffers-margin-use)
           (mumamo-update-all-buffers-margin-use)))
  :group 'mumamo-display)

(defun mumamo-update-all-buffers-margin-use ()
  (dolist (buf (buffer-list))
    (mumamo-update-buffer-margin-use buf)))

(define-minor-mode mumamo-no-chunk-coloring
  "Use no background colors to distinguish chunks.
When this minor mode is on in a buffer no chunk coloring is done
in that buffer.  This is overrides `mumamo-chunk-coloring'.  It
is meant for situations when you temporarily need to remove the
background colors."
  :lighter " ø"
  :group 'mumamo-display
  (font-lock-mode -1)
  (font-lock-mode 1))
(put 'mumamo-no-chunk-coloring 'permanent-local t)


;; (setq mumamo-chunk-coloring 4)
(defcustom mumamo-chunk-coloring 0
  "Color chunks with depth greater than or equal to this.
When 0 all chunks will be colored.  If 1 all sub mode chunks will
be colored, etc."
  :type '(integer :tag "Color chunks with depth greater than this")
  :group 'mumamo-display)

(defface mumamo-background-chunk-major
  '((((class color) (min-colors 88) (background dark))
     ;;:background "blue3")
     :background "MidnightBlue")
    (((class color) (min-colors 88) (background light))
     ;;:background "lightgoldenrod2")
     :background "cornsilk")
    (((class color) (min-colors 16) (background dark))
     :background "blue4")
    (((class color) (min-colors 16) (background light))
     :background "cornsilk")
    (((class color) (min-colors 8))
     :background "blue")
    (((type tty) (class mono))
     :inverse-video t)
    (t :background "gray"))
  "Background colors for chunks in sub modes.
You should only specify :background here, otherwise it will
interfere with syntax highlighting."
  :group 'mumamo-display)

(defface mumamo-background-chunk-submode1
  '((((class color) (min-colors 88) (background dark))
     ;;:background "blue3")
     :background "DarkGreen"
     ;;:background "#081010"
     )
    (((class color) (min-colors 88) (background light))
     ;;:background "lightgoldenrod2")
     :background "Azure")
    (((class color) (min-colors 16) (background dark))
     :background "blue3")
    (((class color) (min-colors 16) (background light))
     :background "azure")
    (((class color) (min-colors 8))
     :background "Blue")
    (((type tty) (class mono))
     :inverse-video t)
    (t :background "gray"))
  "Background colors for chunks in major mode.
You should only specify :background here, otherwise it will
interfere with syntax highlighting."
  :group 'mumamo-display)

(defface mumamo-background-chunk-submode2
  '((((class color) (min-colors 88) (background dark))
     ;;:background "blue3")
     :background "dark green")
    (((class color) (min-colors 88) (background light))
     ;;:background "lightgoldenrod2")
     :background "#e6ff96")
    (((class color) (min-colors 16) (background dark))
     :background "blue3")
    (((class color) (min-colors 16) (background light))
     :background "azure")
    (((class color) (min-colors 8))
     :background "blue")
    (((type tty) (class mono))
     :inverse-video t)
    (t :background "gray"))
  "Background colors for chunks in major mode.
You should only specify :background here, otherwise it will
interfere with syntax highlighting."
  :group 'mumamo-display)

(defface mumamo-background-chunk-submode3
  '((((class color) (min-colors 88) (background dark))
     ;;:background "blue3")
     :background "dark green")
    (((class color) (min-colors 88) (background light))
     ;;:background "lightgoldenrod2")
     :background "#f7d1f4")
    ;;:background "green")
    (((class color) (min-colors 16) (background dark))
     :background "blue3")
    (((class color) (min-colors 16) (background light))
     :background "azure")
    (((class color) (min-colors 8))
     :background "blue")
    (((type tty) (class mono))
     :inverse-video t)
    (t :background "gray"))
  "Background colors for chunks in major mode.
You should only specify :background here, otherwise it will
interfere with syntax highlighting."
  :group 'mumamo-display)

(defface mumamo-background-chunk-submode4
  '((((class color) (min-colors 88) (background dark))
     ;;:background "blue3")
     :background "dark green")
    (((class color) (min-colors 88) (background light))
     ;;:background "lightgoldenrod2")
     :background "orange")
    (((class color) (min-colors 16) (background dark))
     :background "blue3")
    (((class color) (min-colors 16) (background light))
     :background "azure")
    (((class color) (min-colors 8))
     :background "blue")
    (((type tty) (class mono))
     :inverse-video t)
    (t :background "gray"))
  "Background colors for chunks in major mode.
You should only specify :background here, otherwise it will
interfere with syntax highlighting."
  :group 'mumamo-display)

(defcustom mumamo-background-chunk-major 'mumamo-background-chunk-major
  "Background colors for chunks in major mode.
Pointer to face with background color.

If you do not want any special background color use the face named
default."
  :type 'face
  :group 'mumamo-display)

(defcustom mumamo-background-chunk-submode1 'mumamo-background-chunk-submode1
  "Background colors for chunks in sub modes.
Pointer to face with background color.

If you do not want any special background color use the face named
default."
  :type 'face
  :group 'mumamo-display)

(defcustom mumamo-background-chunk-submode2 'mumamo-background-chunk-submode2
  "Background colors for chunks in sub modes.
Pointer to face with background color.

If you do not want any special background color use the face named
default."
  :type 'face
  :group 'mumamo-display)

(defcustom mumamo-background-chunk-submode3 'mumamo-background-chunk-submode3
  "Background colors for chunks in sub modes.
Pointer to face with background color.

If you do not want any special background color use the face named
default."
  :type 'face
  :group 'mumamo-display)

(defcustom mumamo-background-chunk-submode4 'mumamo-background-chunk-submode4
  "Background colors for chunks in sub modes.
Pointer to face with background color.

If you do not want any special background color use the face named
default."
  :type 'face
  :group 'mumamo-display)

;; Fix-me: use and enhance this
(defcustom mumamo-background-colors '(mumamo-background-chunk-major
                                      mumamo-background-chunk-submode1
                                      mumamo-background-chunk-submode2
                                      mumamo-background-chunk-submode3
                                      mumamo-background-chunk-submode4
                                      )
  "List of background colors in order of use.
First color is for main major mode chunks, then for submode
chunks, sub-submode chunks etc.  Colors are reused in cyclic
order.

The default colors are choosen so that inner chunks has a more
standing out color the further in you get.  This is supposed to
be helpful when you make mistakes and the chunk nesting is not
what you intended.

Note: Only the light background colors have been set by me.  The
dark background colors might currently be unuseful.
Contributions and suggestions are welcome!

The values in the list should be symbols. Each symbol should either be

  1: a variable symbol pointing to a face (or beeing nil)
  2: a face symbol
  3: a function with one argument (subchunk depth) returning a
     face symbol"
  :type '(repeat symbol)
  :group 'mumamo-display)

;;(mumamo-background-color 0)
;;(mumamo-background-color 1)
;;(mumamo-background-color 2)
(defun mumamo-background-color (sub-chunk-depth)
  (when (and (not mumamo-no-chunk-coloring)
             (or (not (integerp mumamo-chunk-coloring)) ;; Old values
                 (>= sub-chunk-depth mumamo-chunk-coloring)))
    (let* ((idx (when mumamo-background-colors
                  (mod sub-chunk-depth (length mumamo-background-colors))))
           (sym (when idx (nth idx mumamo-background-colors)))
           fac)
      (when sym
        (when (boundp sym)
          (setq fac (symbol-value sym))
          (unless (facep fac) (setq fac nil)))
        (unless fac
          (when (facep sym)
            (setq fac sym)))
        (unless fac
          (when (fboundp sym)
            (setq fac (funcall sym sub-chunk-depth))))
        (when fac
          (unless (facep fac)
            (setq fac nil)))
        fac
        ))))

(defface mumamo-border-face-in
  '((t (:inherit font-lock-preprocessor-face :bold t :italic t :underline t)))
  "Face for marking borders."
  :group 'mumamo-display)

(defface mumamo-border-face-out
  '((t (:inherit font-lock-preprocessor-face :bold t :italic t :underline t)))
  "Face for marking borders."
  :group 'mumamo-display)


(defgroup mumamo-indentation nil
  "Customization group for mumamo chunk indentation."
  :group 'mumamo)

(defcustom mumamo-submode-indent-offset 2
  "Indentation of submode relative outer major mode.
If this is nil then indentation first non-empty line in a
subchunk will \(normally) be 0.  See however
`mumamo-indent-line-function-1' for special handling of first
line in subsequent subchunks.

See also `mumamo-submode-indent-offset-0'."
  :type '(choice integer
                 (const :tag "No special"))
  :group 'mumamo-indentation)

(defcustom mumamo-submode-indent-offset-0 0
  "Indentation of submode at column 0.
This value overrides `mumamo-submode-indent-offset' when the
outer major mode above has indentation 0."
  :type '(choice integer
                 (const :tag "No special"))
  :group 'mumamo-indentation)

(defcustom mumamo-indent-major-to-use
  '(
    ;;(nxhtml-mode html-mode)
    (html-mode nxhtml-mode)
    )
  "Major mode to use for indentation.
This is normally the major mode specified for the chunk. Here you
can make exceptions."
  :type '(repeat
          (list (symbol :tag "Major mode symbol specified")
                (command :tag "Major mode to use")))
  :group 'mumamo-indentation)

;;(mumamo-indent-get-major-to-use 'nxhtml-mode)
;;(mumamo-indent-get-major-to-use 'html-mode)
(defun mumamo-indent-get-major-to-use (major depth)
  (or (and (= depth 0)
           (cadr (assq major mumamo-indent-major-to-use)))
      major))

(defcustom mumamo-indent-widen-per-major
  '(
    (php-mode (use-widen))
    (nxhtml-mode (use-widen (html-mumamo-mode nxhtml-mumamo-mode)))
    (html-mode (use-widen (html-mumamo-mode nxhtml-mumamo-mode)))
    )
  "Wether do widen buffer during indentation.
If not then the buffer is narrowed to the current chunk when
indenting a line in a chunk."
  :type '(repeat
          (list (symbol :tag "Major mode symbol")
                (set
                 (const :tag "Widen buffer during indentation" use-widen)
                 (repeat (command :tag "Widen if multi major is any of those"))
                 )))
  :group 'mumamo-indentation)


;;;###autoload
(defgroup mumamo-hi-lock-faces nil
  "Faces for hi-lock that are visible in mumamo multiple modes.
This is a workaround for the problem that text properties are
always hidden behind overlay dito.

This faces are not as visible as those that defines background
colors.  However they use underlining so they are at least
somewhat visible."
  :group 'hi-lock
  :group 'mumamo-display
  :group 'faces)

(defface hi-mumamo-yellow
  '((((min-colors 88) (background dark))
     (:underline "yellow1"))
    (((background dark)) (:underline "yellow"))
    (((min-colors 88)) (:underline "yellow1"))
    (t (:underline "yellow")))
  "Default face for hi-lock mode."
  :group 'mumamo-hi-lock-faces)

(defface hi-mumamo-pink
  '((((background dark)) (:underline "pink"))
    (t (:underline "pink")))
  "Face for hi-lock mode."
  :group 'mumamo-hi-lock-faces)

(defface hi-mumamo-green
  '((((min-colors 88) (background dark))
     (:underline "green1"))
    (((background dark)) (:underline "green"))
    (((min-colors 88)) (:underline "green1"))
    (t (:underline "green")))
  "Face for hi-lock mode."
  :group 'mumamo-hi-lock-faces)

(defface hi-mumamo-blue
  '((((background dark)) (:underline "light blue"))
    (t (:underline "light blue")))
  "Face for hi-lock mode."
  :group 'mumamo-hi-lock-faces)

(defface hi-mumamo-black-b
  '((t (:weight bold :underline t)))
  "Face for hi-lock mode."
  :group 'mumamo-hi-lock-faces)

(defface hi-mumamo-blue-b
  '((((min-colors 88)) (:weight bold :underline "blue1"))
    (t (:weight bold :underline "blue")))
  "Face for hi-lock mode."
  :group 'mumamo-hi-lock-faces)

(defface hi-mumamo-green-b
  '((((min-colors 88)) (:weight bold :underline "green1"))
    (t (:weight bold :underline "green")))
  "Face for hi-lock mode."
  :group 'mumamo-hi-lock-faces)

(defface hi-mumamo-red-b
  '((((min-colors 88)) (:weight bold :underline "red1"))
    (t (:weight bold :underline "red")))
  "Face for hi-lock mode."
  :group 'mumamo-hi-lock-faces)


;; (defcustom mumamo-check-chunk-major-same nil
;;   "Check if main major mode is the same as normal mode."
;;   :type 'boolean
;;   :group 'mumamo)

;; (customize-option 'mumamo-major-modes)
;;(require 'django)

(defgroup mumamo-modes nil
  "Customization group for mumamo chunk modes."
  :group 'mumamo)

(defcustom mumamo-major-modes
  '(
    (asp-js-mode
     js-mode ;; Not autoloaded in the pretest
     javascript-mode
     espresso-mode
     ecmascript-mode)
    (asp-vb-mode
     visual-basic-mode)
    ;;(css-mode fundamental-mode)
    (javascript-mode
     js-mode ;; Not autoloaded in the pretest
     javascript-mode
     espresso-mode
     ;;js2-fl-mode
     ecmascript-mode)
    (java-mode
     jde-mode
     java-mode)
    (groovy-mode
     groovy-mode)
    ;; For Emacs 22 that do not have nxml by default
    ;; Fix me: fallback when autoload fails!
    (nxhtml-mode
     nxhtml-mode
     html-mode)
    )
  "Alist for conversion of chunk major mode specifier to major mode.
Each entry has the form

  \(MAJOR-SPEC MAJORMODE ...)

where the symbol MAJOR-SPEC specifies the code type and should
match the value returned from `mumamo-find-possible-chunk'.  The
MAJORMODE symbols are major modes that can be used for editing
that code type.  The first available MAJORMODE is the one that is
used.

The MAJOR-SPEC symbols are used by the chunk definitions in
`define-mumamo-multi-major-mode'.

The major modes are not specified directly in the chunk
definitions.  Instead a chunk definition contains a symbol that
is looked up in this list to find the chunk's major mode.

The reason for doing it this way is to make it possible to use
new major modes with existing multi major modes.  If for example
someone writes a new CSS mode that could easily be used instead
of the current one in `html-mumamo-mode'.

Lookup in this list is done by `mumamo-major-mode-from-modespec'."
  :type '(alist
          :key-type (symbol :tag "Symbol for major mode spec in chunk")
          :value-type (repeat (choice
                               (command :tag "Major mode")
                               (symbol :tag "Major mode (not yet loaded)")))
          )
  :group 'mumamo-modes)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; JIT lock functions

(defun mumamo-jit-lock-function (start)
  "This function is added to `fontification-functions' by mumamo.
START is a parameter given to functions in that hook."
  (mumamo-msgfntfy "mumamo-jit-lock-function %s, ff=%s, just-changed=%s"
                   start
                   (when start
                     (save-restriction
                       (widen)
                       (get-text-property start 'fontified)))
                   mumamo-just-changed-major)
  ;;(msgtrc "jit-lock-function %s, ff=%s, just-changed=%s" start (get-text-property start 'fontified) mumamo-just-changed-major)
  ;;(msgtrc "mumamo-jit-lock-function enter: font-lock-keywords-only def=%s" (default-value 'font-lock-keywords-only))
  (if mumamo-just-changed-major
      (setq mumamo-just-changed-major nil))
  (let ((ret (jit-lock-function start)))
    (mumamo-msgfntfy "mumamo-jit-lock-function EXIT %s, ff=%s, just-changed=%s"
                     start
                     (when start
                       (save-restriction
                         (widen)
                         (get-text-property start 'fontified)))
                     mumamo-just-changed-major)
    ;;(msgtrc "mumamo-jit-lock-function exit: font-lock-keywords-only def=%s" (default-value 'font-lock-keywords-only))
    ret))

(defun mumamo-jit-lock-register (fun &optional contextual)
  "Replacement for `jit-lock-register'.
Avoids refontification, otherwise same.  FUN and CONTEXTUAL has
the some meaning as there."
  (add-hook 'jit-lock-functions fun nil t)
  (when (and contextual jit-lock-contextually)
    (set (make-local-variable 'jit-lock-contextually) t))

  ;;(jit-lock-mode t)
  ;;
  ;; Replace this with the code below from jit-lock-mode t part:
  (setq jit-lock-mode t)

  ;; Mark the buffer for refontification.
  ;; This is what we want to avoid in mumamo:
  ;;(jit-lock-refontify)

  ;; Install an idle timer for stealth fontification.
  (when (and jit-lock-stealth-time (null jit-lock-stealth-timer))
    (setq jit-lock-stealth-timer
          (run-with-idle-timer jit-lock-stealth-time t
                               'jit-lock-stealth-fontify)))

  ;; Create, but do not activate, the idle timer for repeated
  ;; stealth fontification.
  (when (and jit-lock-stealth-time (null jit-lock-stealth-repeat-timer))
    (setq jit-lock-stealth-repeat-timer (timer-create))
    (timer-set-function jit-lock-stealth-repeat-timer
                        'jit-lock-stealth-fontify '(t)))

  ;; Init deferred fontification timer.
  (when (and jit-lock-defer-time (null jit-lock-defer-timer))
    (setq jit-lock-defer-timer
          (run-with-idle-timer jit-lock-defer-time t
                               'jit-lock-deferred-fontify)))

  ;; Initialize contextual fontification if requested.
  (when (eq jit-lock-contextually t)
    (unless jit-lock-context-timer
      (setq jit-lock-context-timer
            (run-with-idle-timer jit-lock-context-time t
                                 'jit-lock-context-fontify)))
    (setq jit-lock-context-unfontify-pos
          (or jit-lock-context-unfontify-pos (point-max))))

  ;; Setup our hooks.
  ;;(add-hook 'after-change-functions 'jit-lock-after-change t t)
  ;;(add-hook 'after-change-functions 'mumamo-jit-lock-after-change t t)
  (add-hook 'after-change-functions 'mumamo-after-change t t)
  ;; Set up fontification to call jit:
  (let ((ff (reverse fontification-functions)))
    (mapc (lambda (f)
            ;;(unless (eq f 'jit-lock-function)
            (remove-hook 'fontification-functions f t))
          ;;)
          ff))
  (add-hook 'fontification-functions 'mumamo-jit-lock-function nil t)
  )

;; Fix-me: integrate this with fontify-region!
(defvar mumamo-find-chunks-timer nil)
(make-variable-buffer-local 'mumamo-find-chunks-timer)
(put 'mumamo-find-chunks-timer 'permanent-local t)

(defvar mumamo-find-chunk-delay idle-update-delay)
(make-variable-buffer-local 'mumamo-find-chunk-delay)
(put 'mumamo-find-chunk-delay 'permanent-local t)

(defun mumamo-stop-find-chunks-timer ()
  "Stop timer that find chunks."
  (when (and mumamo-find-chunks-timer
             (timerp mumamo-find-chunks-timer))
    (cancel-timer mumamo-find-chunks-timer))
  (setq mumamo-find-chunks-timer nil))

(defun mumamo-start-find-chunks-timer ()
  "Start timer that find chunks."
  (mumamo-stop-find-chunks-timer)
  ;; (setq mumamo-find-chunks-timer
  ;;       (run-with-idle-timer mumamo-find-chunk-delay nil
  ;;                            'mumamo-find-chunks-in-timer (current-buffer)))
  )

(defun mumamo-find-chunks-in-timer (buffer)
  "Run `mumamo-find-chunks' in buffer BUFFER in a timer."
  (mumamo-msgfntfy "mumamo-find-chunks-in-timer %s" buffer)
  ;;(message "mumamo-find-chunks-in-timer %s" buffer)
  (condition-case err
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (mumamo-find-chunks nil "mumamo-find-chunks-in-timer")))
    (error (message "mumamo-find-chunks error: %s" err))))


(defvar mumamo-last-chunk nil)
(make-variable-buffer-local 'mumamo-last-chunk)
(put 'mumamo-last-chunk 'permanent-local t)

(defvar mumamo-last-change-pos nil)
(make-variable-buffer-local 'mumamo-last-change-pos)
(put 'mumamo-last-change-pos 'permanent-local t)

;; Fix-me: maybe this belongs to contextual fontification? Eh,
;; no. Unfortunately there is not way to make that handle more than
;; multiple lines.
(defvar mumamo-find-chunk-is-active nil
  "Protect from recursive calls.")

;; Fix-me: temporary things for testing new chunk routines.
(defvar mumamo-find-chunks-level 0)
(setq mumamo-find-chunks-level 0)

(defvar mumamo-old-tail nil)
(make-variable-buffer-local 'mumamo-old-tail)
(put 'mumamo-old-tail 'permanent-local t)

(defun mumamo-update-obscure (chunk pos)
  "Update obscure cache."
  (let ((obscured (overlay-get chunk 'obscured))
        region-info)
    (unless (and obscured (= (car obscured) pos))
      (setq region-info (mumamo-get-region-from pos))
      ;;(msgtrc "update-obscure:region-info=%s" region-info)
      ;; This should not be a chunk here
      (mumamo-put-obscure chunk pos region-info))))

(defun mumamo-put-obscure (chunk pos region-or-chunk)
  "Cache obscure info."
  (assert (overlayp chunk) t)
  (when pos (assert (or (markerp pos) (integerp pos)) t))
  (let* ((region-info (if (overlayp region-or-chunk)
                          (cons (overlay-start region-or-chunk)
                                (overlay-end region-or-chunk))
                        region-or-chunk))
         (obscured (when pos (list pos region-info))))
    ;;(msgtrc "put-obscure:region-info=%s, obscured=%s" region-info obscured)
    (when region-info (assert (consp region-info) t))
    (assert (not (overlayp region-info)) t)
    (overlay-put chunk 'obscured obscured)
    (setq obscured (overlay-get chunk 'obscured))
    ;;(msgtrc "                            obscured=%s" obscured)
    ))

(defun mumamo-get-region-from (point)
  "Return mumamo region values for POINT."
  ;; Note: `mumamo-get-region-from-1' is defined in mumamo-regions.el
  (when (fboundp 'mumamo-get-region-from-1)
    (mumamo-get-region-from-1 point)))

(defun mumamo-clear-chunk-ppss-cache (chunk)
  (overlay-put chunk 'mumamo-ppss-cache nil)
  (overlay-put chunk 'mumamo-ppss-last nil)
  (overlay-put chunk 'mumamo-ppss-stats nil))

(defun mumamo-find-chunks (end tracer)
  "Find or create chunks from last known chunk.
Ie, start from the end of `mumamo-last-chunk' if this is
non-nil, otherwise 1.

If END is nil then continue till end of buffer or until any input
is available.  In this case the return value is undefined.

Otherwise END must be a position in the buffer.  Return the
mumamo chunk containing the position.  If `mumamo-last-chunk'
ends before END then create chunks upto END."
  (when mumamo-multi-major-mode
    (let ((chunk (mumamo-find-chunks-1 end tracer))
          region-info)
      (when (and end chunk (featurep 'mumamo-regions))
        (setq region-info (mumamo-get-region-from end))
        ;;(msgtrc "find-chunks:region-info=%s" region-info)
        (if (overlayp region-info)
            (setq chunk region-info)
          ;;(overlay-put chunk 'obscured (list end region-info))))
          (mumamo-put-obscure chunk end region-info)))
      ;;(msgtrc "find-chunks ret chunk=%s" chunk)
      chunk)))

(defun mumamo-move-to-old-tail (first-check-from)
  "Divide the chunk list.
Make it two parts. The first, before FIRST-CHECK-FROM is still
correct but we want to check those after.  Put thosie in
`mumamo-old-tail'."
  (let ((while-n0 0))
    (while (and (mumamo-while 500 'while-n0 "mumamo-last-chunk first-check-from")
                mumamo-last-chunk
                first-check-from
                (< first-check-from (overlay-end mumamo-last-chunk)))
      (overlay-put mumamo-last-chunk 'mumamo-next-chunk mumamo-old-tail)
      (setq mumamo-old-tail mumamo-last-chunk)
      (overlay-put mumamo-old-tail 'mumamo-is-new nil)
      (when nil ;; For debugging
        (overlay-put mumamo-old-tail
                     'face
                     (list :background
                           (format "red%d" (overlay-get mumamo-old-tail 'mumamo-depth)))))
      (setq mumamo-last-chunk
            (overlay-get mumamo-last-chunk 'mumamo-prev-chunk)))))

(defun mumamo-delete-empty-chunks-at-end ()
  ;; fix-me: later? Delete empty chunks at end, will be recreated if really needed
  (let ((while-n1 0))
    (while (and (mumamo-while 500 'while-n1 "mumamo-last-chunk del empty chunks")
                mumamo-last-chunk
                ;;(= (point-max) (overlay-end mumamo-last-chunk))
                (= (overlay-end mumamo-last-chunk) (overlay-start mumamo-last-chunk)))
      ;;(msgtrc "delete-overlay at end")
      (delete-overlay mumamo-last-chunk)
      (setq mumamo-last-chunk (overlay-get mumamo-last-chunk 'mumamo-prev-chunk))
      (when mumamo-last-chunk (overlay-put mumamo-last-chunk 'mumamo-next-chunk nil)))))


(defun mumamo-delete-chunks-upto (ok-pos)
  "Delete old chunks upto OK-POS."
  (or (not mumamo-old-tail)
      (overlay-buffer mumamo-old-tail)
      (setq mumamo-old-tail nil))
  (let ((while-n2 0))
    (while (and (mumamo-while 500 'while-n2 "mumamo-old-tail")
                (and mumamo-old-tail (< (overlay-start mumamo-old-tail) ok-pos)))
      (mumamo-mark-for-refontification (overlay-start mumamo-old-tail) (overlay-end mumamo-old-tail))
      ;;(msgtrc "find-chunks:ok-pos=%s, not eq delete %s" ok-pos mumamo-old-tail)
      (delete-overlay mumamo-old-tail)
      (setq mumamo-old-tail (overlay-get mumamo-old-tail 'mumamo-next-chunk))
      (or (not mumamo-old-tail)
          (overlay-buffer mumamo-old-tail)
          (setq mumamo-old-tail nil)))))

(defun mumamo-reuse-old-tail-head ()
  ;;(msgtrc "reusing %S" mumamo-old-tail)
  (setq mumamo-last-chunk mumamo-old-tail)
  (overlay-put mumamo-last-chunk 'mumamo-is-new t)
  (mumamo-clear-chunk-ppss-cache mumamo-last-chunk)
  (overlay-put mumamo-last-chunk 'face (mumamo-background-color (overlay-get mumamo-last-chunk 'mumamo-depth)))
  (setq mumamo-old-tail (overlay-get mumamo-old-tail 'mumamo-next-chunk)))

(defun mumamo-old-tail-fits (this-new-values)
  (and mumamo-old-tail
       (overlay-buffer mumamo-old-tail)
       (mumamo-new-chunk-equal-chunk-values mumamo-old-tail this-new-values)))

(defun mumamo-find-chunks-1 (end tracer) ;; min max)
  ;; Note: This code must probably be reentrant.  The globals changed
  ;; here are `mumamo-last-chunk' and `mumamo-old-tail'.  They must be
  ;; handled as a pair.
  (mumamo-msgfntfy "")
  (setq mumamo-find-chunks-level (1+ mumamo-find-chunks-level))
  (unless (and (overlayp mumamo-last-chunk) (overlay-buffer mumamo-last-chunk)) (setq mumamo-last-chunk nil))
  (save-restriction
    (widen)
    (let* ((mumamo-find-chunks-1-active t)
           (here (point))
           ;; Any changes?
           (change-min (car mumamo-last-change-pos))
           (change-max (cdr mumamo-last-change-pos))
           (chunk-at-change-min (when change-min (mumamo-get-existing-new-chunk-at change-min nil)))
           (chunk-at-change-min-start (when chunk-at-change-min (overlay-start chunk-at-change-min)))
           ;; Check if change is near border
           (this-syntax-min-max
            (when chunk-at-change-min
              (mumamo-update-obscure chunk-at-change-min chunk-at-change-min-start)
              (mumamo-chunk-syntax-min-max chunk-at-change-min nil)))
           (this-syntax-min (car this-syntax-min-max))
           (in-min-border (when this-syntax-min (>= this-syntax-min change-min)))
           (first-check-from (if chunk-at-change-min
                                 (if (or in-min-border
                                         ;; Fix-me: 20?
                                         (> 20 (- change-min chunk-at-change-min-start)))
                                     (max 1
                                          (- chunk-at-change-min-start 1))
                                   chunk-at-change-min-start)
                               (when change-min
                                 (goto-char change-min)
                                 (skip-chars-backward "^\n")
                                 (unless (bobp) (backward-char))
                                 (prog1 (point) (goto-char here))))))
      (when (and chunk-at-change-min (= 0 (- (overlay-end chunk-at-change-min)
                                             (overlay-start chunk-at-change-min))))
        (assert in-min-border)) ;; 0 len must be in border
      (setq mumamo-last-change-pos nil)
      (when chunk-at-change-min
        (mumamo-move-to-old-tail first-check-from)
        (mumamo-delete-empty-chunks-at-end))
      ;; Now mumamo-last-chunk is the last in the top chain and
      ;; mumamo-old-tail the first in the bottom chain.

      (let* (
             ;;(last-chunk-is-closed (when mumamo-last-chunk (overlay-get mumamo-last-chunk 'mumamo-is-closed)))
             (last-chunk-is-closed t)
             (ok-pos (or (and mumamo-last-chunk
                              (- (overlay-end mumamo-last-chunk)
                                 ;;(or (and last-chunk-is-closed 1)
                                 (or (and (/= (overlay-end mumamo-last-chunk)
                                             (1+ (buffer-size)))
                                          1)
                                     0)))
                         0))
             (end-param end)
             (end (or end (point-max)))
             this-new-values
             this-new-chunk
             prev-chunk
             first-change-pos
             interrupted
             (while-n3 0))
        (when (>= ok-pos end)
          (setq this-new-chunk (mumamo-get-existing-new-chunk-at end nil))
          (unless this-new-chunk
            (error "Could not find new chunk ok-pos-new=%s > end=%s (ovls at end=%s), level=%d, old-tail=%s, %S"
                   ok-pos end (overlays-in end end)
                   mumamo-find-chunks-level mumamo-old-tail tracer)))
        (unless this-new-chunk
          (save-match-data
            (unless  mumamo-find-chunk-is-active
              ;;(setq  mumamo-find-chunk-is-active t)
              (mumamo-stop-find-chunks-timer)
              (mumamo-save-buffer-state nil
                (progn

                  ;; Loop forward until end or buffer end ...
                  (while (and (mumamo-while 1500 'while-n3 "until end")
                              (or (not end)
                                  (<= ok-pos end))
                              ;;(prog1 t (msgtrc "ok-pos=%s in while" ok-pos))
                              (< ok-pos (point-max))
                              (not (setq interrupted (and (not end)
                                                          (input-pending-p)))))
                    ;; Narrow to speed up. However the chunk divider may be
                    ;; before ok-pos here. Assume that the marker is not
                    ;; longer than 200 chars. fix-me.
                    (narrow-to-region (max (- ok-pos 200) 1)
                                      (1+ (buffer-size)))
                    ;; If this was after a change within one chunk then tell that:
                    (let ((use-change-max (when (and change-max
                                                     chunk-at-change-min
                                                     (overlay-buffer chunk-at-change-min)
                                                     (< change-max
                                                        (overlay-end chunk-at-change-min))
                                                     (or (not mumamo-last-chunk)
                                                         (> change-max (overlay-end mumamo-last-chunk))))
                                            change-max))
                          (use-chunk-at-change-min (when (or (not mumamo-last-chunk)
                                                             (not (overlay-buffer mumamo-last-chunk))
                                                             (not chunk-at-change-min)
                                                             (not (overlay-buffer chunk-at-change-min))
                                                             (> (overlay-end chunk-at-change-min)
                                                                (overlay-end mumamo-last-chunk)))
                                                     chunk-at-change-min
                                                     )))
                      (setq this-new-values (mumamo-find-next-chunk-values
                                             mumamo-last-chunk
                                             first-check-from
                                             use-change-max
                                             use-chunk-at-change-min)))
                    (if (not this-new-values)
                        (setq ok-pos (point-max))
                      (setq first-check-from nil)
                      (setq ok-pos (or (mumamo-new-chunk-value-max this-new-values) ;;(overlay-end this-chunk)
                                       (point-max)))
                      ;;(msgtrc "ok-pos=%s, point-max=%s max=%s" ok-pos (point-max) (mumamo-new-chunk-value-max this-new-values))
                      ;; With the new organization all chunks are created here.
                      (if (mumamo-old-tail-fits this-new-values)
                          (mumamo-reuse-old-tail-head)
                        (mumamo-delete-chunks-upto ok-pos)
                        ;; Create chunk and chunk links
                        (setq mumamo-last-chunk (mumamo-new-create-chunk this-new-values))
                        ;;(setq last-chunk-is-closed (overlay-get mumamo-last-chunk 'mumamo-is-closed))
                        (unless first-change-pos
                          (setq first-change-pos (mumamo-new-chunk-value-min this-new-values))))))
                  (setq this-new-chunk mumamo-last-chunk)))
              (widen)
              (when (or interrupted
                        (and mumamo-last-chunk
                             (overlayp mumamo-last-chunk)
                             (overlay-buffer mumamo-last-chunk)
                             (buffer-live-p (overlay-buffer mumamo-last-chunk))
                             (< (overlay-end mumamo-last-chunk) (point-max))))
                (mumamo-start-find-chunks-timer)
                )
              (when first-change-pos
                (setq jit-lock-context-unfontify-pos
                      (if jit-lock-context-unfontify-pos
                          (min jit-lock-context-unfontify-pos first-change-pos)
                        first-change-pos))))
            (goto-char here)
            (setq  mumamo-find-chunk-is-active nil)))

        ;; fix-me: continue here
        (when chunk-at-change-min (mumamo-clear-chunk-ppss-cache chunk-at-change-min))
        (setq mumamo-find-chunks-level (1- mumamo-find-chunks-level))
        ;; Avoid empty overlays at the end of the buffer. Those can
        ;; come from for example deleting to the end of the buffer.
        (when this-new-chunk
          ;; Fix-me: can this happen now?
          (setq prev-chunk (overlay-get this-new-chunk 'mumamo-prev-chunk))
          (when (and prev-chunk
                     (overlay-buffer prev-chunk)
                     (= (overlay-start this-new-chunk) (overlay-end this-new-chunk))
                     (= (overlay-start prev-chunk) (overlay-end prev-chunk)))
            (overlay-put prev-chunk 'mumamo-next-chunk nil)
            (overlay-put prev-chunk 'mumamo-prev-chunk nil)
            ;;(msgtrc "find-chunks:deleting this-new-chunk %s" this-new-chunk)
            (delete-overlay this-new-chunk)
            (setq this-new-chunk prev-chunk)
            )
          (while (and mumamo-old-tail
                      (overlay-buffer mumamo-old-tail)
                      (= (overlay-start mumamo-old-tail) (overlay-end mumamo-old-tail)))
            (assert (not (eq mumamo-old-tail (overlay-get mumamo-old-tail 'mumamo-next-chunk))) t)
            (setq prev-chunk mumamo-old-tail)
            (setq mumamo-old-tail (overlay-get mumamo-old-tail 'mumamo-next-chunk))
            ;;(msgtrc "mumamo-find-chunks-1:after mumamo-old-tail=%s" mumamo-old-tail)
            (delete-overlay prev-chunk)
            )
          )
        ;;(unless (overlay-get mumamo-last-chunk 'mumamo-is-closed)
        (unless t ;(= (overlay-end mumamo-last-chunk) (save-restriction (widen) (point-max)))
          ;; Check that there are no left-over old chunks
          (save-restriction
            (widen)
            (dolist (o (overlays-in (point-min) (point-max)))
              (when (and (overlay-get o 'mumamo-depth)
                         (not (overlay-get o 'mumamo-is-new)))
                (error "mumamo-find-chunks: left over chunk: %s end=%s, last-chunk=%s" o end mumamo-last-chunk)))))
        (when end-param
          ;;(msgtrc "find-chunks:Exit.end-param=%s, this-new-chunk=%s, point-max=%s, last=%s" end-param this-new-chunk (point-max) mumamo-last-chunk)
          (let* ((ret this-new-chunk)
                 (ret-beg (overlay-start ret))
                 (ret-end (overlay-end ret)))
            (unless (and (<= ret-beg end-param)
                         (<= end-param ret-end))
              (error "mumamo-find-chunks: Bad ret=%s, end=%s" ret end-param))
            ;;(msgtrc "find-chunks=>%S" ret)
            ret))))))

(defun mumamo-find-chunk-after-change (min max)
  "Save change position after a buffer change.
This should be run after a buffer change.  For MIN see
`after-change-functions'."
  ;; Fix-me: Maybe use a list of all min, max instead?
  (mumamo-start-find-chunks-timer)
  ;;(msgtrc "(mumamo-find-chunk-after-change %s %s)" min max)
  (setq min (copy-marker min nil))
  (setq max (copy-marker max t))
  (setq mumamo-last-change-pos
        (if mumamo-last-change-pos
            (let* ((old-min (car mumamo-last-change-pos))
                   (old-max (cdr mumamo-last-change-pos))
                   (new-min (min min old-min))
                   (new-max (max max old-max)))
              (cons new-min new-max))
          (cons min max))))

(defun mumamo-after-change (min max old-len)
  "Everything that needs to be done in mumamo after a change.
This is run in the `after-change-functions' hook.  For MIN, MAX
and OLD-LEN see that variable."
  ;;(msgtrc "mumamo-after-change BEGIN min/max/old-len=%s/%s/%s" min max old-len)
  ;;(msgtrc "mumamo-after-change BEGIN")
  (mumamo-find-chunk-after-change min max)
  (mumamo-jit-lock-after-change min max old-len)
  (mumamo-msgfntfy "mumamo-after-change EXIT")
  ;;(msgtrc "mumamo-after-change EXIT mumamo-last-change-pos=%s" mumamo-last-change-pos)
  )

(defun mumamo-jit-lock-after-change (min max old-len)
  ;; Fix-me: Should not this be on
  ;; jit-lock-after-change-externd-region-functions??
  "Replacement for `jit-lock-after-change'.
Does the nearly the same thing as that function, but takes
care of that there might be different major modes at MIN and MAX.
It also marks for refontification only in the current mumamo chunk.

OLD-LEN is the pre-change length.

Jit-lock after change functions is organized this way:

`jit-lock-after-change' (doc: Mark the rest of the buffer as not
fontified after a change) is added locally to the hook
`after-change-functions'.  This function runs
`jit-lock-after-change-extend-region-functions'."
  (when (and jit-lock-mode (not memory-full))
    (mumamo-msgfntfy "mumamo-jit-lock-after-change ENTER %s %s %s" min max old-len)
    ;; Why is this nil?:
    (mumamo-msgfntfy "  mumamo-jit-lock-after-change: font-lock-extend-after-change-region-function=%s" font-lock-extend-after-change-region-function)
    (let* ((ovl-min (mumamo-get-existing-new-chunk-at min nil))
           (ovl-max (when (or (not ovl-min)
                              (< (overlay-end ovl-min) max))
                      (mumamo-get-existing-new-chunk-at max nil)))
           (major-min (when ovl-min (mumamo-chunk-major-mode ovl-min)))
           (major-max (when ovl-max (mumamo-chunk-major-mode ovl-max)))
           (r-min nil)
           (r-max nil)
           (new-min min)
           (new-max max))
      (if (and major-min (eq major-min major-max))
          (setq r-min
                (when major-min
                  (mumamo-jit-lock-after-change-1 min max old-len major-min)))
        (setq r-min
              (when major-min
                (mumamo-jit-lock-after-change-1 min max old-len major-min)))
        (setq r-max
              (when major-max
                (mumamo-jit-lock-after-change-1 min max old-len major-max))))
      (mumamo-msgfntfy "mumamo-jit-lock-after-change r-min,max=%s,%s major-min,max=%s,%s" r-min r-max major-min major-max)
      (when r-min
        (setq new-min (min new-min (car r-min)))
        (setq new-max (max new-max (cdr r-min))))
      (when r-max
        (setq new-min (min new-min (car r-max)))
        (setq new-max (max new-max (cdr r-max))))
      (setq new-min (max new-min (point-min)))
      (setq new-max (min new-max (point-max)))
      ;; Make sure we change at least one char (in case of deletions).
      (setq new-max (min (max new-max (1+ new-min)) (point-max)))
      (mumamo-msgfntfy "mumamo-jit-lock-after-change new-min,max=%s,%s" new-min new-max)
      (mumamo-mark-for-refontification new-min new-max)

      ;; Mark the change for deferred contextual refontification.
      ;;(setq jit-lock-context-unfontify-pos nil) (setq message-log-max t)
      (when jit-lock-context-unfontify-pos
        (setq jit-lock-context-unfontify-pos
              ;; Here we use `start' because nothing guarantees that the
              ;; text between start and end will be otherwise refontified:
              ;; usually it will be refontified by virtue of being
              ;; displayed, but if it's outside of any displayed area in the
              ;; buffer, only jit-lock-context-* will re-fontify it.
              (min jit-lock-context-unfontify-pos new-min))
        ;;(with-current-buffer (get-buffer "*Messages*") (erase-buffer))
        (mumamo-msgfntfy "mumamo-jit-lock-after-change EXIT unfontify-pos=%s" jit-lock-context-unfontify-pos)
        ;;(message "mumamo-jit-lock-after-change.unfontify-pos=%s" jit-lock-context-unfontify-pos)
        ))))
;;(min jit-lock-context-unfontify-pos jit-lock-start))))))
;;(put 'mumamo-jit-lock-after-change 'permanent-local-hook t)
(put 'mumamo-after-change 'permanent-local-hook t)

(defun mumamo-jit-lock-after-change-1 (min max old-len major)
  "Extend the region the same way jit-lock does it.
This function tries to extend the region between MIN and MAX the
same way jit-lock does it after a change.  OLD-LEN is the
pre-change length.

The extending of the region is done as if MAJOR was the major
mode."
  (mumamo-with-major-mode-fontification major
    `(progn
       (let ((jit-lock-start ,min)
             (jit-lock-end   ,max))
         ;;(mumamo-msgfntfy "mumamo-mumamo-jit-lock-after-change-1 jlacer=%s" ,jit-lock-after-change-extend-region-functions)
         (mumamo-with-buffer-prepared-for-jit-lock
          ;;(font-lock-extend-jit-lock-region-after-change ,min ,max ,old-len)
          (run-hook-with-args 'jit-lock-after-change-extend-region-functions min max old-len)
          ;;(setq jit-lock-end (min (max jit-lock-end (1+ min)) (point-max)))

;;;           ;; Just run the buffer local function:
;;;           (dolist (extend-fun jit-lock-after-change-extend-region-functions)
;;;             (when (fboundp extend-fun)
;;;               (funcall extend-fun ,min ,max ,old-len)))
          )
         (setq min jit-lock-start)
         (setq max jit-lock-end)
         ;;(syntax-ppss-flush-cache min)
         )))
  (mumamo-msgfntfy "mumamo-mumamo-jit-lock-after-change-1 EXIT %s" (cons min max))
  (cons min max))

(defun mumamo-mark-chunk ()
  "Mark chunk and move point to beginning of chunk."
  (interactive)
  (let ((chunk (mumamo-find-chunks (point) "mumamo-mark-chunk")))
    (unless chunk (error "There is no MuMaMo chunk here"))
    (goto-char (overlay-start chunk))
    (push-mark (overlay-end chunk) t t)))

(defun mumamo-narrow-to-chunk-inner ()
  (interactive)
  (let* ((chunk (mumamo-find-chunks (point) "mumamo-narrow-to-chunk-innner"))
         (syntax-min-max (mumamo-chunk-syntax-min-max chunk t))
         (syntax-min (car syntax-min-max))
         (syntax-max (cdr syntax-min-max)))
    (narrow-to-region syntax-min syntax-max)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Font lock functions

(defadvice hi-lock-set-pattern (around use-overlays activate)
  (if mumamo-multi-major-mode
      (let ((font-lock-fontified nil))
        ad-do-it)
    ad-do-it))

;;;###autoload
(defun mumamo-mark-for-refontification (min max)
  "Mark region between MIN and MAX for refontification."
  ;;(msgtrc "mark-for-refontification A min,max=%s,%s point-min,max=%s,%s modified=%s" min max (point-min) (point-max) (buffer-modified-p) )
  ;;(mumamo-backtrace "mark-for-refontification")
  (mumamo-msgfntfy "mumamo-mark-for-refontification A min,max=%s,%s point-min,max=%s,%s modified=%s" min max (point-min) (point-max) (buffer-modified-p) )
  (assert (<= min max))
  (when (< min max)
    (save-restriction
      (widen)
      (mumamo-msgfntfy "mumamo-mark-for-refontification B min,max=%s,%s point-min,max=%s,%s modified=%s" min max (point-min) (point-max) (buffer-modified-p) )
      ;;(mumamo-with-buffer-prepared-for-jit-lock
      (mumamo-save-buffer-state nil
        (put-text-property min max 'fontified nil)
        ))))


;; Fix me: The functions in this list must be replaced by variables
;; pointing to anonymous functions for buffer local values of
;; fontification keywords to be supported. And that is of course
;; necessary for things like hi-lock etc. (Or..., perhaps some kind of
;; with-variable-values... as RMS suggested once... but that will not
;; help here...)
;;
;; Seems like font-lock-add-keywords must be advised...
(defvar mumamo-internal-major-modes-alist nil
  "Alist with info for different major modes.
Internal use only.  This is automatically set up by
`mumamo-get-major-mode-setup'.")
(setq mumamo-internal-major-modes-alist nil)
(put 'mumamo-internal-major-modes-alist 'permanent-local t)

(defvar mumamo-ppss-last-chunk nil
  "Internal variable used to avoid unnecessary flushing.")
(defvar mumamo-ppss-last-major nil
  "Internal variable used to avoid unnecessary flushing.")

;;(mumamo-get-major-mode-substitute 'nxhtml-mode 'fontification)
;;(mumamo-get-major-mode-substitute 'nxhtml-mode 'indentation)
;;(mumamo-get-major-mode-substitute 'css-mode 'fontification)
;;(mumamo-get-major-mode-substitute 'css-mode 'indentation)
;; (assq 'nxml-mode mumamo-major-mode-substitute)
(defconst mumamo-major-mode-substitute
  '(
    (nxhtml-mode (html-mode nxhtml-mode))
    ;;(nxhtml-mode (html-mode))
    (nxhtml-genshi-mode (html-mode nxhtml-mode))
    (nxhtml-mjt-mode (html-mode nxhtml-mode))
    (nxml-mode (sgml-mode))
    )
  "Major modes substitute to use for fontification and indentation.
The entries in this list has either of the formats

  \(MAJOR (FONT-MODE INDENT-MODE))
  \(MAJOR (FONT-MODE))

where major is the major mode in a mumamo chunk and FONT-MODE is
the major mode for fontification of that chunk and INDENT-MODE is
dito for indentation.  In the second form the same mode is used
for indentation as for fontification.")

;;(mumamo-get-major-mode-substitute 'nxhtml-mode 'indentation)
;;(mumamo-get-major-mode-substitute 'nxhtml-mode 'fontification)
(defun mumamo-get-major-mode-substitute (major for-what)
  "For major mode MAJOR return major mode to use for FOR-WHAT.
FOR-WHAT can be either 'fontification or indentation.

mumamo must handle fontification and indentation for `major-mode'
by using other major mode if the functions for this in
`major-mode' are not compatible with mumamo.  This functions
looks in the table `mumamo-major-mode-substitute' for get major
mode to use."
  ;;(when (eq for-what 'indentation) (message "subst.major=%s" major))
  (let ((m (assq major mumamo-major-mode-substitute))
        ret-major)
    (if (not m)
        (setq ret-major major)
      (setq m (nth 1 m))
      (setq ret-major
            (cond
             ((eq for-what 'fontification)
              (nth 0 m))
             ((eq for-what 'indentation)
              (nth 1 m))
             (t
              (mumamo-display-error 'mumamo-get-major-mode-substitute
                                    "Bad parameter, for-what=%s" for-what))))
      (unless ret-major (setq ret-major major)))
    (unless (commandp ret-major) (setq ret-major 'mumamo-bad-mode))
    ;;(when (eq for-what 'indentation) (message "ret.ind=%s, major=%s, m=%s" ret major m))
    ret-major))

(defun mumamo-assert-fontified-t (start end)
  "Assert that the region START to END has 'fontified t."
  (let ((start-ok (get-text-property start 'fontified))
        (first-not-ok
         (next-single-property-change (1+ start) 'fontified nil end)))
    (when (not start-ok)
      (message "==== mumamo-assert-fontified-t %s-%s start not ok" start end))
    (when (not (= first-not-ok end))
      (message "==== mumamo-assert-fontified-t %s-%s first not ok=%s"  start end first-not-ok))))

;; Keep this separate for easier debugging.
(defun mumamo-do-fontify (start end verbose chunk-syntax-min chunk-syntax-max chunk-major)
  "Fontify region between START and END.
If VERBOSE is non-nil then print status messages during
fontification.

CHUNK-SYNTAX-MIN, CHUNK-SYNTAX-MAX and CHUNK-MAJOR are the
chunk's min point, max point and major mode.

During fontification narrow the buffer to the chunk to make
syntactic fontification work.  If chunks starts or end with \"
then the first respective last char then exclude those chars from
from the narrowed part, since otherwise the syntactic
fontification can't find out where strings start and stop.

Note that this function is run under
`mumamo-with-major-mode-fontification'.

This function takes care of `font-lock-dont-widen' and
`font-lock-extend-region-functions'.  Normally
`font-lock-default-fontify-region' does this, but that function
is not called when mumamo is used!

PS: `font-lock-fontify-syntactically-region' is the main function
that does syntactic fontification."
  ;;(msgtrc "mumamo-do-fontify enter: font-lock-keywords-only def=%s" (default-value 'font-lock-keywords-only))
  ;;(msgtrc "mumamo-do-fontify <<<<<<< %s %s %s %s %s %s" start end verbose chunk-syntax-min chunk-syntax-max chunk-major)
  ;;(msgtrc "font-lock-keywords=%S" font-lock-keywords)
  ;;(mumamo-assert-fontified-t start end)
  (mumamo-condition-case err
      (let* ((font-lock-dont-widen t)
             (font-lock-extend-region-functions
              ;; nil
              font-lock-extend-region-functions
              )
             ;; Extend like in `font-lock-default-fontify-region':
             (funs font-lock-extend-region-functions)
             (font-lock-beg (max chunk-syntax-min start))
             (font-lock-end (min chunk-syntax-max end))
             (while-n1 0))
        ;;(while (and (> 500 (setq while-n1 (1+ while-n1)))
        (while (and (mumamo-while 500 'while-n1 "funs")
                    funs)
          (setq funs (if (or (not (funcall (car funs)))
                             (eq funs font-lock-extend-region-functions))
                         (cdr funs)
                       ;; If there's been a change, we should go through
                       ;; the list again since this new position may
                       ;; warrant a different answer from one of the fun
                       ;; we've already seen.
                       font-lock-extend-region-functions)))
        ;; But we must restrict to the chunk here:
        (let ((new-start (max chunk-syntax-min font-lock-beg))
              (new-end (min chunk-syntax-max font-lock-end)))
          ;;(msgtrc "do-fontify %s %s, chunk-syntax-min,max=%s,%s, new: %s %s" start end chunk-syntax-min chunk-syntax-max new-start new-end)
          ;; A new condition-case just to catch errors easier:
          (when (< new-start new-end)
            (mumamo-condition-case err
                (save-restriction
                  ;;(when (and (>= 625 (point-min)) (<= 625 (point-max))) (msgtrc "multi at 625=%s" (get-text-property 625 'font-lock-multiline)))
                  ;;(msgtrc "(narrow-to-region %s %s)" chunk-syntax-min chunk-syntax-max)
                  (when (< chunk-syntax-min chunk-syntax-max)
                    (narrow-to-region chunk-syntax-min chunk-syntax-max)
                    ;; Now call font-lock-fontify-region again but now
                    ;; with the chunk font lock parameters:
                    (setq font-lock-syntactically-fontified (1- new-start))
                    (mumamo-msgfntfy "ENTER font-lock-fontify-region %s %s %s" new-start new-end verbose)
                    ;;(msgtrc "mumamo-do-fontify: font-lock-keywords-only =%s in buffer %s, def=%s" font-lock-keywords-only (current-buffer) (default-value 'font-lock-keywords-only))
                    (let (font-lock-extend-region-functions)
                      (font-lock-fontify-region new-start new-end verbose))
                    (mumamo-msgfntfy "END font-lock-fontify-region %s %s %s" new-start new-end verbose)
                    )
                  )
              (error
               (mumamo-display-error 'mumamo-do-fontify-2
                                     "mumamo-do-fontify m=%s, s/e=%s/%s syn-min/max=%s/%s: %s"
                                     chunk-major
                                     start end
                                     chunk-syntax-min chunk-syntax-max
                                     (error-message-string err)))))))
    (error
     (mumamo-display-error 'mumamo-do-fontify
                           "mumamo-do-fontify m=%s, s=%s, e=%s: %s"
                           chunk-major start end (error-message-string err)))
    )
  (mumamo-msgfntfy "mumamo-do-fontify exit >>>>>>> %s %s %s %s %s %s" start end verbose chunk-syntax-min chunk-syntax-max chunk-major)
  ;;(msgtrc "mumamo-do-fontify exit: font-lock-keywords-only def=%s" (default-value 'font-lock-keywords-only))
  )

(defun mumamo-do-unfontify (start end)
  "Unfontify region between START and END."
  (mumamo-condition-case err
      (font-lock-unfontify-region start end)
    (error
     (mumamo-display-error 'mumamo-do-unfontify "%s"
                           (error-message-string err)))))

(defun mumamo-fontify-region-with (start end verbose major chunk-syntax-min chunk-syntax-max)
  "Fontify from START to END.
If VERBOSE is non-nil then print status messages during
fontification.

Do the fontification as in major mode MAJOR.

Narrow to region CHUNK-SYNTAX-MIN and CHUNK-SYNTAX-MAX during
fontification."
  ;; The text property 'fontified is always t here due to the way
  ;; jit-lock works!

  ;;(msgtrc "fontify-region-with %s %s %s %s, ff=%s" start end verbose major (get-text-property start 'fontified))
  ;;(mumamo-assert-fontified-t start end)
  ;;(msgtrc "mumamo-fontify-region-with enter: font-lock-keywords-only def=%s" (default-value 'font-lock-keywords-only))
  (mumamo-condition-case err
      (progn
        ;;(msgtrc "mumamo-fontify-region-with: font-lock-keywords-only =%s in buffer %s, def=%s" font-lock-keywords-only (current-buffer) (default-value 'font-lock-keywords-only))
        (mumamo-with-major-mode-fontification major
          `(mumamo-do-fontify ,start ,end ,verbose ,chunk-syntax-min ,chunk-syntax-max major))
        )
    (error
     (mumamo-display-error 'mumamo-fontify-region-with "%s"
                           (error-message-string err))))
  ;;(msgtrc "mumamo-fontify-region-with exit: font-lock-keywords-only def=%s" (default-value 'font-lock-keywords-only))
  )

(defun mumamo-unfontify-region-with (start end major)
  "Unfontify from START to END as in major mode MAJOR."
  (mumamo-msgfntfy "mumamo-unfontify-region-with %s %s %s, ff=%s"
                   start
                   end
                   major
                   (when start
                     (save-restriction
                       (widen)
                       (get-text-property start 'fontified))))
  (mumamo-with-major-mode-fontification major
    `(mumamo-do-unfontify ,start ,end)))



(defun mumamo-backtrace (label)
  (msgtrc "%s:backtrace in START buffer %s <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n%s"
          label (current-buffer) (with-output-to-string (backtrace)))
  (msgtrc "%s:backtrace in END   buffer %s >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" label (current-buffer)))

(defun mumamo-unfontify-buffer ()
  "Unfontify buffer.
This function is called when the minor mode function
`font-lock-mode' is turned off.  \(It is the value of
`font-lock-unfontify-uffer-function')."
  (when (and mumamo-multi-major-mode
             (not (and (boundp 'mumamo-find-chunks-1-active)
                       mumamo-find-chunks-1-active)))
    ;;(mumamo-backtrace "unfontify-buffer")
    ;;(msgtrc "mumamo-unfontify-buffer:\n%s" (with-output-to-string (backtrace)))
    (save-excursion
      (save-restriction
        (widen)
        (let ((ovls (overlays-in (point-min) (point-max)))
              (main-major (mumamo-main-major-mode)))
          (dolist (o ovls)
            (when (overlay-get o 'mumamo-is-new)
              (let ((major (mumamo-chunk-major-mode o)))
                (when major
                  (unless (mumamo-fun-eq major main-major)
                    (mumamo-unfontify-chunk o))
                  ;;(msgtrc "delete-overlay 1")
                  (delete-overlay o)
                  ))))
          (mumamo-unfontify-region-with (point-min) (point-max)
                                        (mumamo-main-major-mode)))))))


(defun mumamo-fontify-buffer ()
  "For `font-lock-fontify-buffer-function' call.
Not sure when this normally is done.  However some functions call
this to ensure that the whole buffer is fontified."
  (mumamo-msgfntfy "===> mumamo-fontify-buffer-function called")
  ;;(font-lock-default-fontify-buffer)
  (unless mumamo-set-major-running
    ;; This function is normally not called, but when new patterns
    ;; have been added by hi-lock it will be called.  In this case we
    ;; need to make buffer local fontification variables:
    (set (make-local-variable 'mumamo-internal-major-modes-alist) nil)
    (jit-lock-refontify)))


(defun mumamo-unfontify-chunk (chunk) ; &optional start end)
  "Unfontify mumamo chunk CHUNK."
  (let* ((major (mumamo-chunk-major-mode chunk))
         ;;(start (overlay-start chunk))
         ;;(end   (overlay-end   chunk))
         (syntax-min-max (mumamo-chunk-syntax-min-max chunk t))
         (syntax-min (car syntax-min-max))
         (syntax-max (cdr syntax-min-max))
         (font-lock-dont-widen t))
    (when (< syntax-min syntax-max)
      (save-restriction
        (narrow-to-region syntax-min syntax-max)
        (mumamo-unfontify-region-with syntax-min syntax-max major)))))

(defun mumamo-fontify-region (start end &optional verbose)
  "Fontify between START and END.
Take the major mode chunks into account while doing this.

If VERBOSE do the verbously.

The value of `font-lock-fontify-region-function' when
mumamo is used is this function."
  (mumamo-msgfntfy "++++++ mumamo-fontify-regionX %s %s %s, skip=%s" start end verbose mumamo-just-changed-major)
  ;;(msgtrc "mumamo-fontify-region: font-lock-keywords-only =%s in buffer %s, def=%s" font-lock-keywords-only (current-buffer) (default-value 'font-lock-keywords-only))
  ;;(mumamo-assert-fontified-t start end)
  ;; If someone else tries to fontify the buffer ...
  (if (and mumamo-just-changed-major
           ;; The above variable is reset in `post-command-hook' so
           ;; check if we are in a recursive search.  (Note: There are
           ;; other situation when this can occur.  It might be best to
           ;; remove this test later, or make it optional.)
           ;;
           ;; skip the test for now:
           nil
           (= 0 (recursion-depth)))
      (mumamo-display-error 'mumamo-fontify-region
                            "Just changed major, should not happen")
    (mumamo-condition-case err
        (mumamo-fontify-region-1 start end verbose)
      (error
       (mumamo-display-error 'mumamo-fontify-region "%s"
                             (error-message-string err))))))

(defconst mumamo-dbg-pretend-fontified nil
  "Set this to t to be able to debug more easily.
This is for debugging `mumamo-fontify-region-1' more easily by
just calling it.  It will make that function believe that the text
has a non-nil 'fontified property.")

(defun mumamo-exc-mode (chunk)
  "Return sub major mode for CHUNK.
If chunk is a main major mode chunk return nil, otherwise return
the major mode for the chunk."
  (let ((major (mumamo-chunk-major-mode chunk)))
    (unless (mumamo-fun-eq major (mumamo-main-major-mode))
      major)))

;;; Chunk in chunk needs push/pop relative prev chunk
(defun mumamo-chunk-push (chunk prop val)
  (let* ((prev-chunk (overlay-get chunk 'mumamo-prev-chunk))
         (prev-val (when prev-chunk (overlay-get prev-chunk prop))))
    (overlay-put chunk prop (cons val prev-val))))
(defun mumamo-chunk-pop (chunk prop)
  (overlay-put chunk prop (cdr (overlay-get (overlay-get chunk 'mumamo-prev-chunk)
                                            prop))))

;; (defvar mumamo-chunks-to-remove nil
;;   "Internal.  Chunk overlays marked for removal.")
;; (make-variable-buffer-local 'mumamo-chunks-to-remove)

(defun mumamo-flush-chunk-syntax (chunk chunk-min chunk-max)
  "Flush syntax cache for chunk CHUNK.
This includes removing text property 'syntax-table between
CHUNK-MIN and CHUNK-MAX."
  ;; syntax-ppss-flush-cache
  (overlay-put chunk 'syntax-ppss-last  nil)
  (overlay-put chunk 'syntax-ppss-cache nil)
  (overlay-put chunk 'syntax-ppss-stats nil)
  (mumamo-save-buffer-state nil
    (remove-list-of-text-properties chunk-min chunk-max '(syntax-table))))

;; Fix-me: If I open nxhtml-changes.html and then go to the bottom of
;; the file at once syntax-ppss seems to be upset. It is however cured
;; by doing some change above the region that is badly fontified.
(defun mumamo-fontify-region-1 (start end verbose)
  "Fontify region between START and END.
If VERBOSE is non-nil then print status messages during
fontification.

This is called from `mumamo-fontify-region' which is the value of
`font-lock-fontify-region-function' when mumamo is used.  \(This
means that it ties into the normal font lock framework in Emacs.)

Note: The purpose of extracting this function from
`mumamo-fontify-region' \(which is the only place where it is
called) is to make debugging easier.  Edebug will without this
function just step over the `condition-case' in
`mumamo-fontify-region'.

The fontification is done in steps:

- First a mumamo chunk is found or created at the start of the
  region with `mumamo-get-chunk-at'.
- Then this chunk is fontified according to the major mode for
  that chunk.
- If the chunk did not encompass the whole region then this
  procedure is repeated with the rest of the region.

If some mumamo chunk in the region between START and END has been
marked for removal \(for example by `mumamo-jit-lock-after-change') then
they are removed by this function.

For some main major modes \(see `define-mumamo-multi-major-mode') the
main major modes is first used to fontify the whole region.  This
is because otherwise the fontification routines for that mode may
have trouble finding the correct starting state in a chunk.

Special care has been taken for chunks that are strings, ie
surrounded by \"...\" since they are fontified a bit special in
most major modes."
  ;; Fix-me: unfontifying should be done using the correct syntax table etc.
  ;; Fix-me: refontify when new chunk
  ;;(msgtrc "fontify-region-1: font-lock-keywords-only =%s in buffer %s, def=%s" font-lock-keywords-only (current-buffer) (default-value 'font-lock-keywords-only))
  (save-match-data
    (let* ((old-point (point))
           (here start)
           (main-major (mumamo-main-major-mode))
           (fontified-t ;;(or mumamo-dbg-pretend-fontified
            ;;    (get-text-property here 'fontified))
            t)
           after-change-functions ;; Fix-me: tested adding this to avoid looping
           (first-new-ovl nil)
           (last-new-ovl nil)
           (chunk-at-start-1 (mumamo-find-chunks start "mumamo-fontify-region-1"))
           (while-n1 0)
           )
      (when chunk-at-start-1
        (unless (= start (1- (overlay-end chunk-at-start-1)))
          (setq chunk-at-start-1 nil)))
      ;;(while (and (> 500 (setq while-n1 (1+ while-n1)))
      (while (and (mumamo-while 9000 'while-n1 "fontified-t")
                  fontified-t
                  (< here end))
        ;;(msgtrc "mumamo-fontify-region-1 heree 1, here=%s, end=%s" here end)
        ;;(mumamo-assert-fontified-t here end)
        ;;(mumamo-assert-fontified-t start end)
        ;; Check where new chunks should be, adjust old chunks as
        ;; necessary.  Refontify inside end-start and outside of
        ;; start-end mark for refontification when major-mode has
        ;; changed or there was no old chunk.
        ;;
        ;; Fix-me: Join chunks!
        (let* ((chunk (mumamo-find-chunks here "mumamo-fontify-region-1 2"))
               (chunk-min (when chunk (overlay-start chunk)))
               (chunk-max (when chunk (overlay-end chunk)))
               (chunk-min-1 (when chunk (if (> chunk-min (point-min)) (1- chunk-min) (point-min))))
               (chunk-max-1 (when chunk (if (< chunk-max (point-max)) (1+ chunk-max) (point-max))))
               (chunk-min-face (when chunk (get-text-property chunk-min-1 'face)))
               (chunk-max-face (when chunk (get-text-property chunk-max-1 'face)))
               (chunk-major (when chunk (mumamo-chunk-major-mode chunk)))
               max                    ; (min chunk-max end))
               )
          (assert chunk)

          (setq chunk-min (when chunk (overlay-start chunk)))
          (setq chunk-max (when chunk (overlay-end chunk)))
          (setq chunk-min-1
                (when chunk
                  (if (> chunk-min (point-min)) (1- chunk-min) (point-min)))) ;chunk-min
          (setq chunk-max-1
                (when chunk
                  (if (< chunk-max (point-max)) (1+ chunk-max) (point-max)))) ;chunk-max
          (setq chunk-min-face
                (when chunk (get-text-property chunk-min-1 'face)))
          (setq chunk-max-face
                (when chunk (get-text-property chunk-max-1 'face)))
          (setq chunk-major (when chunk (mumamo-chunk-major-mode chunk)))

          (if (and first-new-ovl (overlay-buffer first-new-ovl))
              (setq last-new-ovl chunk)
            (setq last-new-ovl chunk)
            (setq first-new-ovl chunk))
          ;;(mumamo-assert-fontified-t chunk-min chunk-max)

          (setq max (min chunk-max end))

          (assert chunk) (assert (overlay-buffer chunk)) (assert chunk-min)
          (assert chunk-max) (assert chunk-major)
          ;; Fix-me: The next assertion sometimes fails.  Could it be
          ;; that this loop is continuing even after a change in the
          ;; buffer? How do I stop that? When?:
          ;;(assert (or (= here start) (= here chunk-min)) nil "h=%s, s=%s, cm=%s-%s, e=%s, chunk-major=%s" here start chunk-min chunk-max end chunk-major)
          ;;(assert (not (mumamo-fun-eq prev-major chunk-major)))
          ;;(when prev-chunk
          ;;  (assert (= (overlay-end prev-chunk) (overlay-start chunk))))

          ;; Fontify
          ;;(msgtrc "\nmumamo-fontify-region-1 before chunk=%s" chunk)
          (mumamo-update-obscure chunk here)
          (let* ((syntax-min-max (mumamo-chunk-syntax-min-max chunk nil))
                 (syntax-min (car syntax-min-max))
                 (syntax-max (cdr syntax-min-max))
                 (chunk-min (overlay-start chunk))
                 (chunk-max (overlay-end chunk))
                 (border-min-max (mumamo-chunk-syntax-min-max chunk t))
                 (border-min (car border-min-max))
                 (border-max (cdr border-min-max))
                 )
            ;;(msgtrc "fontify-region-1:syntax-min-max=%S, chunk=%S" syntax-min-max chunk)
            ;;(msgtrc "chunk mumamo-border-face: %s" chunk)
            (mumamo-msgfntfy "mumamo-fontify-region-1, here=%s chunk-min=%s syn-mn/mx=%s/%s" here chunk-min syntax-min syntax-max)
            (when (<= here syntax-min)
              (mumamo-flush-chunk-syntax chunk syntax-min syntax-max))
            (when (and (<= here syntax-min)
                       (< chunk-min border-min))
              ;;(msgtrc "face-in: %s-%s" chunk-min border-min)
              (put-text-property chunk-min border-min 'face 'mumamo-border-face-in)
              )
            (when (and (<= chunk-max max)
                       ;;(< (1+ border-max) chunk-max))
                       (< border-max chunk-max))
              ;;(put-text-property (1+ border-max) chunk-max
              (put-text-property border-max chunk-max
                                 'face 'mumamo-border-face-out))
            (mumamo-fontify-region-with here max verbose chunk-major
                                        syntax-min syntax-max))

          ;;(setq prev-major chunk-major)
          ;;(setq prev-chunk chunk)
          (setq here (if (= max here) (1+ max) max))
          ;;(setq fontified-t (or mumamo-dbg-pretend-fontified (get-text-property (1- here) 'fontified)))
          )
        ;;(msgtrc "ft here end=%s %s %s" fontified-t here end)
        )
      (goto-char old-point)
      ;;(msgtrc "b first-new-ovl=%s last-new-ovl=%s" first-new-ovl last-new-ovl)
      (unless fontified-t
        ;; Fix-me: I am not sure what to do here.  Probably just
        ;; refontify the rest between start and end.  But does not
        ;; this lead to unnecessary refontification?
        ;;(msgtrc "not sure, here=%s, end=%s" here end)
        (unless (= here (point-max))
          (mumamo-mark-for-refontification here end)))
      ))
  ;;(msgtrc "EXIT mumamo-fontify-region-1")
  )


(defvar mumamo-known-buffer-local-fontifications
  '(
    font-lock-mode-hook
    ;;
    css-color-mode
    hi-lock-mode
    hi-lock-file-patterns
    hi-lock-interactive-patterns
    wrap-to-fill-column-mode
    ))

(defconst mumamo-irrelevant-buffer-local-vars
  '(
    ;; This list was fetched with
    ;; emacs-Q, fundamental-mode
    after-change-functions
    ;;auto-composition-function
    ;;auto-composition-mode
    ;;auto-composition-mode-major-mode
    buffer-auto-save-file-format
    buffer-auto-save-file-name
    buffer-backed-up
    buffer-display-count
    buffer-display-time
    buffer-file-format
    buffer-file-name
    buffer-file-truename
    buffer-invisibility-spec
    buffer-read-only
    buffer-saved-size
    buffer-undo-list
    change-major-mode-hook
    ;;char-property-alias-alist
    cursor-type
    default-directory
    delay-mode-hooks
    enable-multibyte-characters
    ;;font-lock-mode
    ;;font-lock-mode-major-mode
    ;;major-mode
    mark-active
    mark-ring
    mode-name
    point-before-scroll
    ;; Handled by font lock etc
    font-lock-defaults
    font-lock-fontified
    font-lock-keywords
    ;;font-lock-keywords-only
    font-lock-keywords-case-fold-search
    font-lock-mode
    ;;font-lock-mode-major-mode
    font-lock-set-defaults
    font-lock-syntax-table
    font-lock-beginning-of-syntax-function
    fontification-functions
    jit-lock-context-unfontify-pos
    jit-lock-mode
    ;; Mumamo
    font-lock-fontify-buffer-function
    jit-lock-contextually
    jit-lock-functions
    ;; More symbols from visual inspection
    before-change-functions
    delayed-mode-hooks
    isearch-mode
    line-move-ignore-invisible
    local-abbrev-table
    ;;syntax-ppss-last
    ;;syntax-ppss-cache

    ;; Cua
    cua--explicit-region-start
    ;; Viper
    viper--intercept-key-maps
    viper--key-maps
    viper-ALPHA-char-class
    viper-current-state
    viper-emacs-global-user-minor-mode
    viper-emacs-intercept-minor-mode
    viper-emacs-kbd-minor-mode
    viper-emacs-local-user-minor-mode
    viper-emacs-state-modifier-minor-mode
    viper-insert-basic-minor-mode
    viper-insert-diehard-minor-mode
    viper-insert-global-user-minor-mode
    viper-insert-intercept-minor-mode
    viper-insert-kbd-minor-mode
    viper-insert-local-user-minor-mode
    viper-insert-minibuffer-minor-mode
    viper-insert-point
    viper-insert-state-modifier-minor-mode
    viper-intermediate-command
    viper-last-posn-while-in-insert-state
    viper-minibuffer-current-face
    viper-mode-string
    viper-non-word-characters
    viper-replace-minor-mode
    viper-replace-overlay
    viper-undo-functions
    viper-undo-needs-adjustment
    viper-vi-basic-minor-mode
    viper-vi-diehard-minor-mode
    viper-vi-global-user-minor-mode
    viper-vi-intercept-minor-mode
    viper-vi-kbd-minor-mode
    viper-vi-local-user-minor-mode
    viper-vi-minibuffer-minor-mode
    viper-vi-state-modifier-minor-mode
    ;; hs minor mode
    hs-adjust-block-beginning
    hs-block-start-mdata-select
    hs-block-start-regexp
    hs-c-start-regexp
    hs-forward-sexp-func
    hs-minor-mode
    ;; Imenu
    imenu-case-fold-search
    imenu-generic-expression
    ;; Fix-me: add more here
    ))

(defun mumamo-get-relevant-buffer-local-vars ()
  "Get list of buffer local variables to save.
Like `buffer-local-variables', but remove variables that are
known to not be necessary to save for fontification, indentation
or filling \(or that can even disturb things)."
  (let (var-vals)
    (dolist (vv (buffer-local-variables))
      (unless (or (not (listp vv))
                  (memq (car vv) mumamo-irrelevant-buffer-local-vars)
                  (let* ((sym (car vv))
                         (val (symbol-value sym)))
                    (or (markerp val)
                        (overlayp val))))
        (let ((ent (list (car vv) (custom-quote (cdr vv)))))
          (setq var-vals (cons ent var-vals)))))
    ;; Sorting is for debugging/testing
    (setq var-vals (sort var-vals
                         (lambda (a b)
                           (string< (symbol-name (car a))
                                    (symbol-name (car b))))))
    var-vals))

(defvar mumamo-major-modes-local-maps nil
  "An alist with major mode and local map.
An entry in the list looks like

  \(MAJOR-MODE LOCAL-KEYMAP)")

;; (defun mumamo-font-lock-keyword-hook-symbol (major)
;;   "Return hook symbol for adding font-lock keywords to MAJOR."
;;   (intern (concat "mumamo-" (symbol-name major) "-font-lock-keyword-hook")))

;; (defun mumamo-remove-font-lock-hook (major setup-fun)
;;   "For mode MAJOR remove function SETUP-FUN.
;; See `mumamo-add-font-lock-hook' for more information."
;;   (remove-hook (mumamo-font-lock-keyword-hook-symbol major) setup-fun))

(defun mumamo-refresh-multi-font-lock (major)
  "Refresh font lock information for mode MAJOR in chunks.
If multi fontification functions for major mode MAJOR is already
setup up they will be refreshed.

If MAJOR is nil then all font lock information for major modes
used in chunks will be refreshed.

After calling font-lock-add-keywords or changing the
fontification in other ways you must call this function for the
changes to take effect.  However already fontified buffers will
not be refontified.  You can use `normal-mode' to refontify
them.

Fix-me: Does not work yet."

  (setq mumamo-internal-major-modes-alist
        (if (not major)
            nil
          (assq-delete-all major mumamo-internal-major-modes-alist))))

;; RMS had the following idea:
;;
;; Suppose we add a Lisp primitive to bind a set of variables under
;; the control of an alist.  Would it be possible to eliminate these
;; helper functions and use that primitive instead?
;;
;;;     But wouldn't it be better to test this version first? There is
;;;     no hurry, this version works and someone might find that there
;;;     is a better way to do this than with helper functions.
;;
;; OK with me, as long as this point doesn't get forgotten.
(defun mumamo-fetch-major-mode-setup (major keywords mode-keywords add-keywords how)
  "Return a helper function to do fontification etc like in major mode MAJOR.
Fetch the variables affecting font locking, indentation and
filling by calling the major mode MAJOR in a temporary buffer.

Make a function with one parameter BODY which is elisp code to
eval.  The function should let bind the variables above, sets the
syntax table temporarily to the one used by the major mode
\(using the mode symbol name to find it) and then evaluates body.

Name this function mumamo-eval-in-MAJOR.  Put the code for this
function in the property `mumamo-defun' on this function symbol.


** Some notes about background etc.

The function made here is used in `mumamo-with-major-mode-setup'.
The code in the function parameter BODY is typically involved in
fontification, indentation or filling.

The main reasons for doing it this way is:

- It is faster and than setting the major mode directly.
- It does not affect buffer local variables."
  ;; (info "(elisp) Other Font Lock Variables")
  ;; (info "(elisp) Syntactic Font Lock)
  ;;(msgtrc "fetch-major 1: font-lock-keywords-only =%s" font-lock-keywords-only)
  (let ((func-sym (intern (concat "mumamo-eval-in-" (symbol-name major))))
        (func-def-sym (intern (concat "mumamo-def-eval-in-" (symbol-name major))))
        ;;(add-keywords-hook (mumamo-font-lock-keyword-hook-symbol major))
        byte-compiled-fun
        (fetch-func-definition `(lambda  (body))) ;;`(defun ,func-sym (body)))
        temp-buf-name
        temp-buf)
    ;; font-lock-mode can't be turned on in buffers whose names start
    ;; with a char with white space syntax.  Temp buffer names are
    ;; such and it is not possible to change name of a temp buffer.
    (setq temp-buf-name (concat "mumamo-fetch-major-mode-setup-" (symbol-name major)))
    (setq temp-buf (get-buffer temp-buf-name))
    (when temp-buf (kill-buffer temp-buf))
    (setq temp-buf (get-buffer-create temp-buf-name))
    ;;(msgtrc "fetch-major-mode-setup in buffer %s, after-chunk=%s, before with-current-buffer" (current-buffer) (when (boundp 'after-chunk) after-chunk))
    (with-current-buffer temp-buf

      (mumamo-msgfntfy "mumamo-fetch-major-mode-setup %s" major)
      (let ((mumamo-fetching-major t)
            mumamo-multi-major-mode)
        ;;(msgtrc "fetch-major-mode-setup in buffer %s, before (funcall %s)" (current-buffer) major)
        (funcall major)
        )

      (mumamo-msgfntfy ">>> mumamo-fetch-major-mode-setup A font-lock-mode=%s" font-lock-mode)
      (font-lock-mode 1)
      (mumamo-msgfntfy "<<< mumamo-fetch-major-mode-setup B font-lock-mode=%s" font-lock-mode)
      (mumamo-msgfntfy "mumamo-fetch-major-mode-setup: fetching jit-lock-after-change-extend-region-functions A=%s" jit-lock-after-change-extend-region-functions)

      ;; Note: font-lock-set-defaults must be called before adding
      ;; keywords. Otherwise Emacs loops. I have no idea why. Hm,
      ;; probably wrong, it is likely to be nxhtml-mumamo that is the
      ;; problem. Does not loop in html-mumamo.
      ;;(msgtrc "\n--------------------")
      (font-lock-set-defaults)
      ;; Fix-me: but hi-lock still does not work... what have I
      ;; forgotten??? font-lock-keywords looks ok...
      (when keywords
        (if add-keywords
            (progn
              ;;(msgtrc "fetch:font-lock-add-keywords %S %S %S" (if mode-keywords major nil) keywords how)
              (font-lock-add-keywords (if mode-keywords major nil) keywords how)
              ;;(font-lock-add-keywords major keywords how)
              ;;(msgtrc "fetch:font-lock-keywords=%S" font-lock-keywords)
              )
          (font-lock-remove-keywords (if mode-keywords major nil) keywords)
          ;;(font-lock-remove-keywords major keywords)
          )
        (unless mode-keywords (font-lock-mode -1) (font-lock-mode 1))
        ;;(msgtrc "fetch-major-mode-setup:font-lock-keywords=%S" font-lock-keywords)
        )
      ;;(run-hooks add-keywords-hook)

      (add-to-list 'mumamo-major-modes-local-maps
                   (let ((local-map (current-local-map)))
                     (cons major-mode (if local-map
                                          (copy-keymap local-map)
                                        'no-local-map))))

      (mumamo-msgfntfy "mumamo-fetch-major-mode-setup: fetching jit-lock-after-change-extend-region-functions B=%s" jit-lock-after-change-extend-region-functions)
      (let* ((syntax-sym (intern-soft (concat (symbol-name major) "-syntax-table")))
             (fetch-func-definition-let
              ;; Be XML compliant:
              (list
               (list 'sgml-xml-mode
                     ;;(when (mumamo-derived-from-mode ',major 'sgml-mode) t))
                     (when (mumamo-derived-from-mode major 'sgml-mode) t))

               ;; We need to copy the variables that we need and
               ;; that are not automatically buffer local, but
               ;; could be it. Arguably it is a bug if they are not
               ;; buffer local though we have to adapt.

               ;; From cc-mode.el:
               (list 'indent-line-function (custom-quote indent-line-function))
               (list 'indent-region-function (custom-quote indent-region-function))
               (list 'normal-auto-fill-function (custom-quote normal-auto-fill-function))
               (list 'comment-start (custom-quote comment-start))
               (list 'comment-end (custom-quote comment-end))
               (list 'comment-start-skip (custom-quote comment-start-skip))
               (list 'comment-end-skip (custom-quote comment-end-skip))
               (list 'comment-multi-line (custom-quote comment-multi-line))
               (list 'comment-line-break-function (custom-quote comment-line-break-function))
               (list 'paragraph-start (custom-quote paragraph-start))
               (list 'paragraph-separate (custom-quote paragraph-separate))
               (list 'paragraph-ignore-fill-prefix (custom-quote paragraph-ignore-fill-prefix))
               (list 'adaptive-fill-mode (custom-quote adaptive-fill-mode))
               (list 'adaptive-fill-regexp (custom-quote adaptive-fill-regexp))

                 ;;; Try doing the font lock things last, keywords really last
               (list 'font-lock-multiline (custom-quote font-lock-multiline))
               (list 'font-lock-extend-after-change-region-function (custom-quote font-lock-extend-after-change-region-function))
               (list 'font-lock-extend-region-functions (custom-quote font-lock-extend-region-functions))
               (list 'font-lock-comment-start-skip (custom-quote font-lock-comment-start-skip))
               (list 'font-lock-comment-end-skip (custom-quote font-lock-comment-end-skip))
               (list 'font-lock-syntactic-keywords (custom-quote font-lock-syntactic-keywords))

               (list 'font-lock-keywords (custom-quote font-lock-keywords))
               ;;(list 'font-lock-keywords-alist (custom-quote font-lock-keywords-alist))
               ;;(list 'font-lock-removed-keywords-alist (custom-quote font-lock-removed-keywords-alist))

               ;; Fix-me: uncommenting this line (as it should be)
               ;; sets font-lock-keywords-only to t globally...: bug 3467
               (list 'font-lock-keywords-only (custom-quote font-lock-keywords-only))

               (list 'font-lock-keywords-case-fold-search (custom-quote font-lock-keywords-case-fold-search))

               (list 'font-lock-set-defaults t) ; whether we have set up defaults.

               ;; Set from font-lock-defaults normally:
               (list 'font-lock-defaults (custom-quote (copy-tree font-lock-defaults)))
               ;; Syntactic Font Lock
               (list 'font-lock-syntax-table (custom-quote font-lock-syntax-table)) ;; See nXhtml bug 400415
               (list 'font-lock-beginning-of-syntax-function (custom-quote font-lock-beginning-of-syntax-function))
               (list 'font-lock-syntactic-face-function (custom-quote font-lock-syntactic-face-function))

               ;; Other Font Lock Variables
               (list 'font-lock-mark-block-function (custom-quote font-lock-mark-block-function))
               (list 'font-lock-extra-managed-props (custom-quote font-lock-extra-managed-props))
               ;; This value is fetched from font-lock:
               (list 'font-lock-fontify-buffer-function (custom-quote font-lock-fontify-buffer-function))
               (list 'font-lock-unfontify-buffer-function (custom-quote font-lock-unfontify-buffer-function))
               (list 'font-lock-fontify-region-function (custom-quote font-lock-fontify-region-function))
               (list 'font-lock-unfontify-region-function (custom-quote font-lock-unfontify-region-function))

               ;; Jit Lock Variables
               (list 'jit-lock-after-change-extend-region-functions (custom-quote jit-lock-after-change-extend-region-functions))

               ;;(list 'syntax-table (custom-quote (copy-syntax-table (syntax-table))))
               ;;(list 'mumamo-original-syntax-begin-function (custom-quote syntax-begin-function))
               (list 'syntax-begin-function (custom-quote syntax-begin-function))
               (list 'fill-paragraph-function (custom-quote fill-paragraph-function))
               (list 'fill-forward-paragraph-function
                     (when (boundp 'fill-forward-paragraph-function)
                       (custom-quote fill-forward-paragraph-function)))

               ;; newcomment
               (list 'comment-use-global-state (custom-quote (when (boundp 'comment-use-global-state) comment-use-global-state)))

               ;; parsing sexps
               (list 'multibyte-syntax-as-symbol (custom-quote multibyte-syntax-as-symbol))
               (list 'parse-sexp-ignore-comments (custom-quote parse-sexp-ignore-comments))
               (list 'parse-sexp-lookup-properties (custom-quote parse-sexp-lookup-properties))
               ;; fix-me: does not the next line work?
               (list 'forward-sexp-function (custom-quote forward-sexp-function))
               ))
             (relevant-buffer-locals (mumamo-get-relevant-buffer-local-vars))
             )
        ;;(append '(1 2) '(3 4) '((eval body)))
        (mumamo-msgfntfy "===========> before setq fetch-func-definition %s" func-sym)
        ;; Avoid doublets
        (dolist (fetched fetch-func-definition-let)
          (let ((fvar (car fetched)))
            (setq relevant-buffer-locals (assq-delete-all fvar relevant-buffer-locals))))
        (setq fetch-func-definition
              (append fetch-func-definition
                      `((let ,(append fetch-func-definition-let
                                      relevant-buffer-locals)
                          (with-syntax-table ,(if syntax-sym
                                                  syntax-sym
                                                '(standard-syntax-table));;'syntax-table
                            ;; fix-me: Protect against font-lock-keywords-only to t globally...: bug 3467
                            ;;(msgtrc "%s enter 1: font-lock-keywords-only def=%s, body=%S" ',major (default-value 'font-lock-keywords-only) body)
                            (let (;(font-lock-keywords-only font-lock-keywords-only)
                                  ret)
                              ;;(msgtrc "%s enter 2: font-lock-keywords-only def=%s" ',major (default-value 'font-lock-keywords-only))
                              (setq ret (eval body))
                              ;;(msgtrc "%s exit 1: font-lock-keywords-only def=%s" ',major (default-value 'font-lock-keywords-only))
                              ret))
                          ;;(msgtrc "in %s 1: font-lock-keywords-only =%s in buffer %s, def=%s" ',func-sym font-lock-keywords-only (current-buffer) (default-value 'font-lock-keywords-only))
                          )
                        ;;(msgtrc "in %s 2: font-lock-keywords-only =%s in buffer %s, def=%s" ',func-sym font-lock-keywords-only (current-buffer) (default-value 'font-lock-keywords-only))
                        ;;(message "backtrace there:\n%s" (with-output-to-string (backtrace)))
                        )))

        (setq byte-compiled-fun (let ((major-syntax-table))
                                  (byte-compile fetch-func-definition)))
        (assert (functionp byte-compiled-fun))
        (unless keywords
          (eval `(defvar ,func-sym nil))
          (eval `(defvar ,func-def-sym ,fetch-func-definition))
          (set func-sym byte-compiled-fun) ;; Will be used as default
          (assert (functionp (symbol-value func-sym)) t)
          (funcall (symbol-value func-sym) nil)
          (put func-sym 'permanent-local t)
          (put func-def-sym 'permanent-local t))))
    (kill-buffer temp-buf)
    ;; Use the new value in current buffer.
    (when  keywords
      ;;(set (make-local-variable func-sym) (symbol-value func-sym))
      ;;(msgtrc "fetch: major=%s func-def-sym=%s cb=%s fetch-func-definition=%s" major func-def-sym (current-buffer) fetch-func-definition)
      ;;(msgtrc "fetch: major=%s func-def-sym=%s cb=%s fetch-func-definition" major func-def-sym (current-buffer))
      (set (make-local-variable func-sym) byte-compiled-fun)
      (set (make-local-variable func-def-sym) fetch-func-definition)
      (put func-sym 'permanent-local t)
      (put func-def-sym 'permanent-local t))
    (assert (functionp (symbol-value func-sym)) t)
    ;; return a list def + fun
    (cons func-sym func-def-sym)))

;; Fix-me: maybe a hook in font-lock-add-keywords??
(defun mumamo-ad-font-lock-keywords-helper (major keywords how add-keywords)
  ;;(msgtrc "ad-font-lock-keywords-helper %s %s %s %s" major keywords how add-keywords)
  (if major
      (mumamo-fetch-major-mode-setup major keywords t t how)
    ;; Fix-me: Can't do that, need a list of all
    ;; mumamo-current-chunk-family chunk functions major
    ;; modes. But this is impossible since the major modes might
    ;; be determined dynamically. As a work around look in current
    ;; chunks.
    (let ((majors (list (mumamo-main-major-mode))))
      (dolist (entry mumamo-internal-major-modes-alist)
        (let ((major (car entry))
              (fun-var-sym (caadr entry)))
          (when (local-variable-p fun-var-sym)
            (setq majors (cons (car entry) majors)))))
      (dolist (major majors)
        (setq major (mumamo-get-major-mode-substitute major 'fontification))
        ;;(msgtrc "(fetch-major-mode-setup %s %s %s %s %s)" major keywords nil t how)
        (mumamo-fetch-major-mode-setup major keywords nil add-keywords how))
      ;;(font-lock-mode -1) (font-lock-mode 1)
      )))

;; Fix-me: This has stopped working again 2009-11-04, but I do not know when it began...
(defadvice font-lock-add-keywords (around
                                   mumamo-ad-font-lock-add-keywords
                                   activate
                                   compile)
  (if (or (boundp 'mumamo-fetching-major) (boundp 'mumamo-add-font-lock-called) (not mumamo-multi-major-mode))
      ad-do-it
    (let (mumamo-multi-major-mode
          mumamo-add-font-lock-called
          (major    (ad-get-arg 0))
          (keywords (ad-get-arg 1))
          (how      (ad-get-arg 2)))
      (mumamo-ad-font-lock-keywords-helper major keywords how t))))

(defadvice font-lock-remove-keywords (around
                                      mumamo-ad-font-lock-remove-keywords
                                      activate
                                      compile)
  (if (or (boundp 'mumamo-fetching-major) (boundp 'mumamo-add-font-lock-called) (not mumamo-multi-major-mode))
      ad-do-it
    (let (mumamo-multi-major-mode
          mumamo-add-font-lock-called
          (major    (ad-get-arg 0))
          (keywords (ad-get-arg 1)))
      (mumamo-ad-font-lock-keywords-helper major keywords nil nil))))

(defun mumamo-bad-mode ()
  "MuMaMo replacement for a major mode that could not be loaded."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'mumamo-bad-mode)
  (setq mode-name
        (propertize "Mumamo Bad Mode"
                    'face 'font-lock-warning-face)))

;;(mumamo-get-major-mode-setup 'css-mode)
;;(mumamo-get-major-mode-setup 'fundamental-mode)
(defun mumamo-get-major-mode-setup (use-major)
  "Return function for evaluating code in major mode USE-MAJOR.
Fix-me: This doc string is wrong, old:

Get local variable values for major mode USE-MAJOR.  These
variables are used for indentation and fontification.  The
variables are returned in a list with the same format as
`mumamo-fetch-major-mode-setup'.

The list of local variable values which is returned by this
function is cached in `mumamo-internal-major-modes-alist'.  This
avoids calling the major mode USE-MAJOR for each chunk during
fontification and speeds up fontification significantly."
  ;; Fix-me: Problems here can cause mumamo to loop badly when this
  ;; function is called over and over again. To avoid this add a
  ;; temporary entry using mumamo-bad-mode while trying to fetch the
  ;; correct mode.

  ;;(assq 'mumamo-bad-mode mumamo-internal-major-modes-alist)
  (let ((use-major-entry (assq use-major mumamo-internal-major-modes-alist))
        bad-mode-entry
        dummy-entry
        fun-var-sym
        fun-var-def-sym)
    (unless use-major-entry
      ;; Get mumamo-bad-mode entry and add a dummy entry based on
      ;; this to avoid looping.
      (setq bad-mode-entry
            (assq 'mumamo-bad-mode mumamo-internal-major-modes-alist))
      (unless bad-mode-entry
        ;; Assume it is safe to get the mumamo-bad-mode entry ;-)
        (add-to-list 'mumamo-internal-major-modes-alist
                     (list 'mumamo-bad-mode
                           (mumamo-fetch-major-mode-setup 'mumamo-bad-mode nil nil nil nil)))
        (setq bad-mode-entry
              (assq 'mumamo-bad-mode mumamo-internal-major-modes-alist)))
      (setq dummy-entry (list use-major (cadr bad-mode-entry)))
      ;; Before fetching setup add the dummy entry and then
      ;; immediately remove it.
      (add-to-list 'mumamo-internal-major-modes-alist dummy-entry)
      (setq use-major-entry (list use-major
                                  (mumamo-fetch-major-mode-setup use-major nil nil nil nil)))
      (setq mumamo-internal-major-modes-alist
            (delete dummy-entry
                    mumamo-internal-major-modes-alist))
      (add-to-list 'mumamo-internal-major-modes-alist use-major-entry))
    (setq fun-var-sym (caadr use-major-entry))
    (setq fun-var-def-sym (cdadr use-major-entry))
    (assert (functionp (symbol-value fun-var-sym)) t)
    (assert (eq 'lambda (car (symbol-value fun-var-def-sym))) t)
    ;; Always make a buffer local value for keywords.
    (unless (local-variable-p fun-var-sym)
      (set (make-local-variable fun-var-sym) (symbol-value fun-var-sym))
      (set (make-local-variable fun-var-def-sym) (symbol-value fun-var-def-sym)))
    (caadr (or (assq use-major mumamo-internal-major-modes-alist)
               ))))
;; (assq use-major
;;     (add-to-list 'mumamo-internal-major-modes-alist
;;                  (list use-major
;;                        (mumamo-fetch-major-mode-setup
;;                         use-major nil nil nil))))))))

(defun mumamo-remove-all-chunk-overlays ()
  "Remove all CHUNK overlays from the current buffer."
  (save-restriction
    (widen)
    (mumamo-delete-new-chunks)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Creating and accessing chunks

(defun mumamo-define-no-mode (mode-sym)
  "Fallback major mode when no major mode for MODE-SYM is found."
  (let ((mumamo-repl4 (intern (format "mumamo-4-%s" mode-sym)))
        (lighter (format "No %s" mode-sym))
        (doc (format "MuMaMo replacement for %s which was not found."
                     mode-sym)))
    (if (commandp mumamo-repl4)
        mumamo-repl4
      (eval `(defun ,mumamo-repl4 ()
               ,doc
               (interactive)
               (kill-all-local-variables)
               (setq major-mode ',mumamo-repl4)
               (setq mode-name
                     (propertize ,lighter
                                 'face 'font-lock-warning-face)))))))
;;(mumamo-define-no-mode 'my-ownB-mode)

;;(mumamo-major-mode-from-modespec 'javascript-mode)
(defun mumamo-major-mode-from-modespec (major-spec)
  "Translate MAJOR-SPEC to a major mode.
Translate MAJOR-SPEC used in chunk definitions of multi major
modes to a major mode.

See `mumamo-major-modes' for an explanation."
  (mumamo-major-mode-from-spec major-spec mumamo-major-modes))

(defun mumamo-major-mode-from-spec (major-spec table)
  (unless major-spec
    (mumamo-backtrace "mode-from-modespec, major-spec is nil"))
  (let ((modes (cdr (assq major-spec table)))
        (mode 'mumamo-bad-mode))
    (setq mode
          (catch 'mode
            (dolist (m modes)
              (when (functionp m)
                (let ((def (symbol-function m)))
                  (when (and (listp def)
                             (eq 'autoload (car def)))
                    (mumamo-condition-case err
                        (load (nth 1 def))
                      (error (setq m nil)))))
                (when m (throw 'mode m))))
            nil))
    (unless mode
      (if (functionp major-spec)
          ;; As a last resort allow spec to be a major mode too:
          (setq mode major-spec)
        (if modes
            (mumamo-warn-once '(mumamo-major-mode-from-modespec)
                              "Couldn't find an available major mode for specification %s,\n  alternatives are:\n    %s"
                              major-spec modes)
          (mumamo-warn-once '(mumamo-major-mode-from-modespec)
                            "Couldn't find an available major mode for spec %s"
                            major-spec))
        ;;(setq mode 'fundamental-mode)
        (setq mode (mumamo-define-no-mode major-spec))
        ))
    (mumamo-msgfntfy " mumamo-major-mode-from-modespec %s => %s" major-spec mode)
    mode))

(defun mumamo-get-existing-new-chunk-at (pos &optional first)
  "Return last existing chunk at POS if any.
However if FIRST get first existing chunk at POS instead."
  ;;(msgtrc "(mumamo-get-existing-new-chunk-at %s)" pos)
  (let ((chunk-ovl)
        (orig-pos pos))
    (when (= pos (point-max))
      (setq pos (1- pos)))
    (when (= pos 0) (setq pos 1))
    (dolist (o (overlays-in pos (1+ pos)))
      (when (and (overlay-get o 'mumamo-is-new)
                 ;; Because overlays-in need to have a range of length
                 ;; > 0 we might have got overlays that is after our
                 ;; orig-pos:
                 (<= (overlay-start o) orig-pos))
        ;; There can be two, choose the last or first depending on
        ;; FIRST.
        (if chunk-ovl
            ;; (when (or (> (overlay-end o) (overlay-start o))
            ;;           (overlay-get o 'mumamo-prev-chunk))
            (when (if first
                      (< (overlay-end o) (overlay-end chunk-ovl))
                    (> (overlay-end o) (overlay-end chunk-ovl))
                    )
              (setq chunk-ovl o))
          (setq chunk-ovl o))))
    chunk-ovl))

(defun mumamo-get-chunk-save-buffer-state (pos)
  "Return chunk overlay at POS.  Preserve state."
  (let (chunk)
    ;;(mumamo-save-buffer-state nil
      ;;(setq chunk (mumamo-get-chunk-at pos)))
      (setq chunk (mumamo-find-chunks pos "mumamo-get-chunk-save-buffer-state"))
      ;;)
    chunk))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chunk and chunk family properties

(defun mumamo-syntax-maybe-completable (pnt)
  "Return non-nil if at point PNT non-printable characters may occur.
This just considers existing chunks."
  (let* ((chunk (mumamo-find-chunks pnt "mumamo-syntax-maybe-completable"))
         syn-min-max)
    (if (not chunk)
        t
      (mumamo-update-obscure chunk pnt)
      (setq syn-min-max (mumamo-chunk-syntax-min-max chunk nil))
      ;;(and (> pnt (1+ (mumamo-chunk-syntax-min chunk)))
      (and (> pnt (1+ (car syn-min-max)))
           ;;(< pnt (1- (mumamo-chunk-syntax-max chunk)))))))
           (< pnt (1- (cdr syn-min-max)))))))

(defvar mumamo-current-chunk-family nil
  "The currently used chunk family.")
(make-variable-buffer-local 'mumamo-current-chunk-family)
(put 'mumamo-current-chunk-family 'permanent-local t)

;; (defvar mumamo-main-major-mode nil)
;; (make-variable-buffer-local 'mumamo-main-major-mode)
;; (put 'mumamo-main-major-mode 'permanent-local t)

(defun mumamo-main-major-mode ()
  "Return major mode used when there are no chunks."
  (let ((mm (cadr mumamo-current-chunk-family)))
    (if mm mm
      (msgtrc "main-major-mode => nil, mumamo-current-chunk-family=%s" mumamo-current-chunk-family))))
;;;   (let ((main (cadr mumamo-current-chunk-family)))
;;;     (if main
;;;         main
;;;       mumamo-main-major-mode)))

;; (defun mumamo-unset-chunk-family ()
;;   "Set chunk family to nil, ie undecided."
;;   (interactive)
;;   (setq mumamo-current-chunk-family nil))

;; (defun mumamo-define-chunks (chunk-family)
;;   "Set the CHUNK-FAMILY used to divide the buffer."
;;   (setq mumamo-current-chunk-family chunk-family))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; General chunk search routines

;; search start forward

;;(defun mumamo-search-fw-exc-start-str (pos max marker)
(defun mumamo-chunk-start-fw-str (pos max marker)
  "General chunk function helper.
A chunk function helper like this can be used in
`mumamo-find-possible-chunk' to find the borders of a chunk.
There are several functions like this that comes with mumamo.
Their names tell what they do.  Lets look at the parts of the
name of this function:

  mumamo-chunk: All this helper functions begins so
  -start-: Search for the start of a chunk
  -fw-: Search forward
  -str: Search for a string

Instead of '-start-' there could be '-end-', ie end.
Instead of '-fw-' there could be '-bw-', ie backward.
Instead of '-str' there could be '-re', ie regular expression.

There could also be a '-inc' at the end of the name.  If the name
ends with this then the markers should be included in the chunks,
otherwise not.

The argument POS means where to start the search.  MAX means how
far to search (when searching backwards the argument is called
'min' instead).  MARKER is a string or regular expression (see
the name) to search for."
  (assert (stringp marker))
  (let ((pm (point-min))
        (cb (current-buffer)))
    (message "cb=%s" cb)
    (goto-char (max pm (- pos (length marker)))))
  (search-forward marker max t))

(defun mumamo-chunk-start-fw-re (pos max marker)
  "General chunk function helper.
See `mumamo-chunk-start-fw-str' for more information and the
meaning of POS, MAX and MARKER."
  (assert (stringp marker))
  (goto-char (- pos (length marker)))
  (re-search-forward marker max t))

(defun mumamo-chunk-start-fw-str-inc (pos max marker)
  "General chunk function helper.
See `mumamo-chunk-start-fw-str' for more information and the
meaning of POS, MAX and MARKER."
  (assert (stringp marker))
  (goto-char pos)
  (let ((start (search-forward marker max t)))
    (when start (setq start (- start (length marker))))))

;; search start backward

;; (defun mumamo-chunk-start-bw-str (pos min marker)
;;   "General chunk function helper.
;; See `mumamo-chunk-start-fw-str' for more information and the
;; meaning of POS, MIN and MARKER."
;;   ;;(assert (stringp marker))
;;   (let (start-in)
;;     (goto-char pos)
;;     (setq start-in (search-backward marker min t))
;;     (when start-in
;;       ;; do not include the marker
;;       (setq start-in (+ start-in (length marker))))
;;     start-in))

;; (defun mumamo-chunk-start-bw-re (pos min marker)
;;   "General chunk function helper.
;; See `mumamo-chunk-start-fw-str' for more information and the
;; meaning of POS, MIN and MARKER."
;;   (assert (stringp marker))
;;   (let (start-in)
;;     (goto-char pos)
;;     (setq start-in (re-search-backward marker min t))
;;     (when start-in
;;       ;; do not include the marker
;;       (setq start-in (match-end 0)))
;;     start-in))

;; (defun mumamo-chunk-start-bw-str-inc (pos min marker)
;;   "General chunk function helper.
;; See `mumamo-chunk-start-fw-str' for more information and the
;; meaning of POS, MIN and MARKER."
;;   (assert (stringp marker))
;;   (goto-char (+ pos (length marker)))
;;   (search-backward marker min t))

;; search end forward

(defun mumamo-chunk-end-fw-str (pos max marker)
  "General chunk function helper.
See `mumamo-chunk-start-fw-str' for more information and the
meaning of POS, MAX and MARKER."
  (assert (stringp marker))
  ;;(goto-char (1+ pos)) ;; 1+ cause otherwise ?> is at point
  (goto-char pos)
  (let (end-in)
    (setq end-in (search-forward marker max t))
    (when end-in
      ;; do not include the marker
      (setq end-in (- end-in (length marker))))
    end-in))

(defun mumamo-chunk-end-fw-re (pos max marker)
  "General chunk function helper.
See `mumamo-chunk-start-fw-str' for more information and the
meaning of POS, MAX and MARKER."
  (assert (stringp marker))
  (goto-char (1+ pos)) ;; 1+ cause otherwise ?> is at point
  (let (end-in)
    (setq end-in (re-search-forward marker max t))
    (when end-in
      ;; do not include the marker
      (setq end-in (match-beginning 0)))
    end-in))

(defun mumamo-chunk-end-fw-str-inc (pos max marker)
  "General chunk function helper.
See `mumamo-chunk-start-fw-str' for more information and the
meaning of POS, MAX and MARKER."
  (assert (stringp marker))
  ;;(goto-char (1+ pos)) ;; 1+ cause otherwise ?> is at point
  (goto-char (1+ (- pos (length marker))))
  ;;(msgtrc "mumamo-chunk-end-fw-str-inc %s %s %s, point=%s point-max=%s" pos max marker (point) (point-max))
  (search-forward marker max t))

;; search end backward

;; (defun mumamo-chunk-end-bw-str (pos min marker)
;;   "General chunk function helper.
;; See `mumamo-chunk-start-fw-str' for more information and the
;; meaning of POS, MIN and MARKER."
;;   (assert (stringp marker))
;;   (goto-char (+ pos (length marker)))
;;   (search-backward marker min t))

;; (defun mumamo-chunk-end-bw-re (pos min marker)
;;   "General chunk function helper.
;; See `mumamo-chunk-start-fw-str' for more information and the
;; meaning of POS, MIN and MARKER."
;;   (assert (stringp marker))
;;   (goto-char (+ pos (length marker)))
;;   (re-search-backward marker min t))

(defun mumamo-chunk-end-bw-str-inc (pos min marker)
  "General chunk function helper.
See `mumamo-chunk-start-fw-str' for more information and the
meaning of POS, MIN and MARKER."
  (assert (stringp marker))
  (goto-char pos)
  (let ((end (search-backward marker min t)))
    (when end (setq end (+ end (length marker))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; General chunk routines

;; (defvar mumamo-known-chunk-start nil "Internal use only!.")

(defconst mumamo-string-syntax-table
  (let ((tbl (copy-syntax-table)))
    (modify-syntax-entry ?\" "\"" tbl)
    (modify-syntax-entry ?\' "\"" tbl)
    tbl)
  "Just for \"..\" and '...'.")

;; "..." '...' "..'.." '.."..'
(defun mumamo-guess-in-string (pos)
  "If POS is in a string then return string start position.
Otherwise return nil."
  (when (and (>= pos (point-min)))
    (let ((here (point))
          (inhibit-field-text-motion t)
          line-beg
          parsed
          str-char
          str-pos)
      (goto-char pos)
      (setq line-beg (line-beginning-position))
      (setq parsed (with-syntax-table mumamo-string-syntax-table
                     (parse-partial-sexp line-beg pos)))
      (setq str-char (nth 3 parsed))
      (when str-char
        (skip-chars-backward (string ?^ str-char))
        (setq str-pos (point)))
      (goto-char here)
      str-pos)))

;;; The main generic chunk routine

;; Fix-me: new routine that really search forward only. Rewrite
;; `mumamo-quick-static-chunk' first with this.
(defun mumamo-possible-chunk-forward (pos
                                      max
                                      chunk-start-fun
                                      chunk-end-fun
                                      &optional borders-fun)
  "Search forward from POS to MAX for possible chunk.
Return as a list with values

  \(START END CHUNK-MAJOR BORDERS PARSEABLE-BY CHUNK-END-FUN BORDERS-FUN)

START and END are start and end of the possible chunk.
CHUNK-MAJOR is the major mode specifier for this chunk.  \(Note
that this specifier is translated to a major mode through
`mumamo-major-modes'.)

START-BORDER and END-BORDER may be nil.  Otherwise they should be
the position where the border ends respectively start at the
corresponding end of the chunk.

BORDERS is the return value of the optional BORDERS-FUN which
takes three parameters, START, END and EXCEPTION-MODE in the
return values above.  BORDERS may be nil and otherwise has this
format:

  \(START-BORDER END-BORDER CHUNK-MAJOR CHUNK-END-FUN)

PARSEABLE-BY is a list of major modes with parsers that can parse
the chunk.

CHUNK-START-FUN and CHUNK-END-FUN should be functions that
searches forward from point for start and end of chunk.  They
both take two parameters, POS and MAX above.  If no possible
chunk is found both these functions should return nil, otherwise
see below.

CHUNK-START-FUN should return a list of the form below if a
possible chunk is found:

  (START CHUNK-MAJOR PARSEABLE-BY)

CHUNK-END-FUN should return the end of the chunk.

"
  ;;(msgtrc "possible-chunk-forward %s %s" pos max)
  (let ((here (point))
        start-rec
        start
        end
        chunk-major
        parseable-by
        borders
        ret
        )
    (goto-char pos)
    ;; Fix-me: check valid. Should this perhaps be done in the
    ;; function calling this instead?
    ;;(mumamo-end-in-code syntax-min syntax-max curr-major)
    (setq start-rec (funcall chunk-start-fun (point) max))
    (when start-rec
      (setq start        (nth 0 start-rec))
      (setq chunk-major  (nth 1 start-rec))
      (setq parseable-by (nth 2 start-rec))
      (goto-char start)
      ;; Fix-me: check valid
      ;;(setq end (funcall chunk-end-fun (point) max))
      (when borders-fun
        (let ((start-border (when start (unless (and (= 1 start)
                                                     (not chunk-major))
                                          start)))
              (end-border   (when end   (unless (and (= (point-max) end)
                                                     (not chunk-major))
                                          end))))
          (setq borders (funcall borders-fun start-border end-border chunk-major))))
      (setq ret (list start end chunk-major borders parseable-by chunk-end-fun borders-fun)))
    (goto-char here)
    ret))

;; Fix-me: This routine has some difficulties. One of the more
;; problematic things is that chunk borders may depend on the
;; surrounding chunks syntax. Patterns that possibly could be chunk
;; borders might instead be parts of comments or strings in cases
;; where they should not be valid borders there.
(defun mumamo-find-possible-chunk (pos
                                   min max
                                   bw-exc-start-fun ;; obsolete
                                   bw-exc-end-fun
                                   fw-exc-start-fun
                                   fw-exc-end-fun
                                   &optional find-borders-fun)
  (mumamo-find-possible-chunk-new pos
                                  ;;min
                                  max
                                  bw-exc-start-fun
                                  ;;bw-exc-end-fun
                                  fw-exc-start-fun
                                  fw-exc-end-fun
                                  find-borders-fun))

(defun mumamo-find-possible-chunk-new (pos
                                       ;;min
                                       max
                                       bw-exc-start-fun
                                       ;;bw-exc-end-fun
                                       fw-exc-start-fun
                                       fw-exc-end-fun
                                       &optional find-borders-fun)
  ;; This should return no end value!
  "Return list describing a possible chunk that starts after POS.
No notice is taken about existing chunks and no chunks are
created.  The description returned is for the smallest possible
chunk which is delimited by the function parameters.

POS must be less than MAX.

The function BW-EXC-START-FUN takes two parameters, POS and
MIN.  It should search backward from POS, bound by MIN, for
exception start and return a cons or a list:

  \(FOUND-POS . EXCEPTION-MODE)
  \(FOUND-POS EXCEPTION-MODE PARSEABLE-BY)

Here FOUND-POS is the start of the chunk.  EXCEPTION-MODE is the
major mode specifier for this chunk.  \(Note that this specifier
is translated to a major mode through `mumamo-major-modes'.)

PARSEABLE-BY is a list of parsers that can handle the chunk
beside the one that may be used by the chunks major mode.
Currently only the XML parser in `nxml-mode' is recognized.  In
this list it should be the symbol `nxml-mode'.

The functions FW-EXC-START-FUN and FW-EXC-END-FUN should search
for exception start or end, forward resp backward. Those two
takes two parameters, start position POS and max position MAX,
and should return just the start respectively the end of the
chunk.

For all three functions the position returned should be nil if
search fails.


Return as a list with values

  \(START END EXCEPTION-MODE BORDERS PARSEABLE-BY FR-EXC-FUN FIND-BORDERS-FUN)

**Fix-me: FIND-BORDERS-FUN must be split for chunks-in-chunks!

The bounds START and END are where the exception starts or stop.
Either of them may be nil, in which case this is equivalent to
`point-min' respectively `point-max'.

If EXCEPTION-MODE is non-nil that is the submode for this
range.  Otherwise the main major mode should be used for this
chunk.

BORDERS is the return value of the optional FIND-BORDERS-FUN
which takes three parameters, START, END and EXCEPTION-MODE in
the return values above.  BORDERS may be nil and otherwise has
this format:

  \(START-BORDER END-BORDER EXCEPTION-MODE FW-EXC-FUN)

START-BORDER and END-BORDER may be nil.  Otherwise they should be
the position where the border ends respectively start at the
corresponding end of the chunk.

PARSEABLE-BY is a list of major modes with parsers that can parse
the chunk.

FW-EXC-FUN is the function that finds the end of the chunk.  This
is either FW-EXC-START-FUN or FW-EXC-END-FUN.

---- * Note: This routine is used by to create new members for
chunk families.  If you want to add a new chunk family you could
most often do that by writing functions for this routine.  Please
see the many examples in mumamo-fun.el for how this can be done.
See also `mumamo-quick-static-chunk'."
  ;;(msgtrc "====")
  ;;(msgtrc "find-poss-new %s %s %s %s %s %s" pos max bw-exc-start-fun fw-exc-start-fun fw-exc-end-fun find-borders-fun)

  ;;(mumamo-condition-case err
  (progn
    (assert (and (<= pos max)) nil
            "mumamo-chunk: pos=%s, max=%s, bt=%S"
            pos max (with-output-to-string (backtrace)))
    ;; "in" refers to "in exception" and "out" is then in main
    ;; major mode.
    (let (start-in-cons
          exc-mode
          fw-exc-mode
          fw-exc-fun
          parseable-by
          start-in start-out
          end-in end-out
          start end
          ;;end-of-exception
          wants-end-type
          found-valid-end
          (main-major (mumamo-main-major-mode))
          borders
          border-beg
          border-end)
      ;;;; find start of range
      ;;
      ;; start normal
      ;;
      ;;(setq start-out (funcall bw-exc-end-fun pos min))
      ;; Do not check end here!
      ;;(setq start-out (funcall fw-exc-end-fun pos max))
      ;;(msgtrc "find-poss-new.start-out=%s" start-out)
      ;; start exception
      (setq start-in (funcall fw-exc-start-fun pos max))
      ;;(msgtrc "find-poss-new.start-in=%s" start-in)
      (when (listp start-in)
        (setq fw-exc-mode (nth 1 start-in))
        (setq start-in (car start-in)))
      ;; compare
      (when (and start-in start-out)
        (if (> start-in start-out)
            (setq start-in nil)
          (setq start-out nil)))
      (cond
       (start-in
        (setq start-in-cons (funcall bw-exc-start-fun start-in pos))
        ;;(msgtrc "find-poss-new.start-in=%s start-in-cons=%s" start-in start-in-cons)
        (when start-in-cons
          (assert (= start-in (car start-in-cons)))
          (setq exc-mode (cdr start-in-cons)))
        (setq start start-in))
       (start-out
        (setq start start-out))
       )
      (when (and exc-mode
                 (listp exc-mode))
        (setq parseable-by (cadr exc-mode))
        (setq exc-mode (car exc-mode)))
      ;; borders
      (when find-borders-fun
        (let ((start-border (when start (unless (and (= 1 start)
                                                     (not exc-mode))
                                          start)))
              (end-border (when end (unless (and (= (point-max) end)
                                                 (not exc-mode))
                                      end))))
          (setq borders (funcall find-borders-fun start-border end-border exc-mode))))
      ;; check
      (setq border-beg (nth 0 borders))
      (setq border-end (nth 1 borders))
      ;;(when start (assert (<= start pos)))
      ;;(assert (or (not start) (= start pos)))
      (when border-beg
        (assert (<= start border-beg)))
      ;; Fix-me: This is just totally wrong in some pieces and a
      ;; desperate try after seeing the problems with wp-app.php
      ;; around line 1120.  Maybe this can be used when cutting chunks
      ;; from top to bottom however.
      (when nil ;end
        (let ((here (point))
              end-line-beg
              end-in-string
              start-in-string
              (start-border (or (nth 0 borders) start))
              (end-border   (or (nth 1 borders) end)))
          ;; Check if in string
          ;; Fix-me: add comments about why and examples + tests
          ;; Fix-me: must loop to find good borders ....
          (when end
            ;; Fix-me: more careful positions for guess
            (setq end-in-string
                  (mumamo-guess-in-string
                   ;;(+ end 2)
                   (1+ end-border)
                   ))
            (when end-in-string
              (when start
                (setq start-in-string
                      (mumamo-guess-in-string
                       ;;(- start 2)
                       (1- start-border)
                       )))
              (if (not start-in-string)
                  (setq end nil)
                (if exc-mode
                    (if (and start-in-string end-in-string)
                        ;; If both are in a string and on the same line then
                        ;; guess this is actually borders, otherwise not.
                        (unless (= start-in-string end-in-string)
                          (setq start nil)
                          (setq end nil))
                      (when start-in-string (setq start nil))
                      (when end-in-string (setq end nil)))
                  ;; Fix-me: ???
                  (when start-in-string (setq start nil))
                  ))
              (unless (or start end)
                (setq exc-mode nil)
                (setq borders nil)
                (setq parseable-by nil))))))

      (when (or start end exc-mode borders parseable-by)
        (setq fw-exc-fun (if exc-mode
                             ;; Fix-me: this is currently correct,
                             ;; but will change if exc mode in exc
                             ;; mode is allowed.
                             fw-exc-end-fun
                           ;; Fix-me: these should be collected later
                           ;;fw-exc-start-fun
                           nil
                           ))
        (mumamo-msgfntfy "--- mumamo-find-possible-chunk-new %s" (list start end exc-mode borders parseable-by fw-exc-fun))
        ;;(message "--- mumamo-find-possible-chunk-new %s" (list start end exc-mode borders parseable-by fw-exc-fun))
        (when fw-exc-mode
          (unless (eq fw-exc-mode exc-mode)
            ;;(message "fw-exc-mode=%s NEQ exc-mode=%s" fw-exc-mode exc-mode)
            ))
        ;;(msgtrc "find-poss-new returns %s" (list start end exc-mode borders parseable-by fw-exc-fun find-borders-fun))
        (when fw-exc-fun
          (list start end exc-mode borders parseable-by fw-exc-fun find-borders-fun)))))
  ;;(error (mumamo-display-error 'mumamo-chunk "%s" (error-message-string err)))

  ;;)
  )

;; (defun temp-overlays-here ()
;;   (interactive)
;;   (let* ((here (point))
;;          (ovl-at (overlays-at here))
;;          (ovl-in (overlays-in here (1+ here)))
;;          (ovl-in0 (overlays-in here here))
;;          )
;;     (with-output-to-temp-buffer (help-buffer)
;;       (help-setup-xref (list #'temp-overlays-at) (interactive-p))
;;       (with-current-buffer (help-buffer)
;;         (insert (format "overlays-at %s:\n%S\n\n" here ovl-at))
;;         (insert (format "overlays-in %s-%s:\n%S\n\n" here (1+ here) ovl-in))
;;         (insert (format "overlays-in %s-%s:\n%S\n\n" here here ovl-in0))
;;         ))))
;; (defun temp-cursor-pos ()
;;   (interactive)
;;   (what-cursor-position t))
;; ;;(global-set-key [f9] 'temp-cursor-pos)
;; (defun temp-test-new-create-chunk ()
;;   (interactive)
;;   (mumamo-delete-new-chunks)
;;   ;;(setq x1 nil)
;;   (let (x1
;;         (first t))
;;     (while (or first x1)
;;       (setq first nil)
;;       (setq x1 (mumamo-new-create-chunk (mumamo-find-next-chunk-values x1 nil nil nil)))))
;;   )

;; (defun temp-create-last-chunk ()
;;   (interactive)
;;   (mumamo-new-create-chunk (mumamo-find-next-chunk-values mumamo-last-chunk nil nil nil)))

(defun mumamo-delete-new-chunks ()
  (setq mumamo-last-chunk nil)
  (save-restriction
    (widen)
    (let ((ovls (overlays-in (point-min) (point-max))))
      (dolist (ovl ovls)
        (when (overlay-get ovl 'mumamo-is-new)
          ;;(msgtrc "delete-overlay %s delete-new-chunks" ovl)
          (delete-overlay ovl))))))

(defun mumamo-new-create-chunk (new-chunk-values)
  "Create and return a chunk from NEW-CHUNK-VALUES.
When doing this store the functions for creating the next chunk
after this in the properties below of the now created chunk:

- 'mumamo-next-major: is nil or the next chunk's major mode.
- 'mumamo-next-end-fun: function that searches for end of AFTER-CHUNK
- 'mumamo-next-border-fun: functions that finds borders"
  ;;((1 696 nxhtml-mode nil nil nil nil) (696 nil php-mode nil nil nil nil))
  ;;(current (list curr-min curr-max curr-major curr-border-min curr-border-max curr-parseable curr-fw-exc-fun))
  ;;(msgtrc "######new-create.chunk.new-chunk-values=%s" new-chunk-values)
  (when new-chunk-values
    (let* ((this-values (nth 0 new-chunk-values))
           (next-values (nth 1 new-chunk-values))
           (next-major      (nth 0 next-values))
           (next-end-fun    (nth 1 next-values))
           (next-border-fun (nth 2 next-values))
           (next-depth-diff (nth 3 next-values))
           (next-indent     (nth 4 next-values))
           (this-beg         (nth 0 this-values))
           (this-end         (nth 1 this-values))
           (this-maj         (nth 2 this-values))
           (this-bmin        (nth 3 this-values))
           (this-bmax        (nth 4 this-values))
           (this-pable       (nth 5 this-values))
           (this-after-chunk (nth 7 this-values))
           ;;(this-is-closed   (nth 8 this-values))
           (this-insertion-type-beg   (nth 8 this-values))
           (this-insertion-type-end   (nth 9 this-values))
           ;;(this-is-closed (and this-end (< 1 this-end)))
           (this-after-chunk-depth (when this-after-chunk
                                (overlay-get this-after-chunk 'mumamo-depth)))
           (depth-diff (if this-after-chunk
                           (overlay-get this-after-chunk 'mumamo-next-depth-diff)
                         1))
           (depth (if this-after-chunk-depth
                      (+ this-after-chunk-depth depth-diff)
                    0))
           ;;(fw-funs (nth 6 this-values))
           ;;(borders-fun (nth 7 this-values))
           ;;(this-is-closed (when (or this-end (mumamo-fun-eq this-maj (mumamo-main-major-mode))) t))
           (use-this-end (if this-end this-end (1+ (buffer-size)))) ;(save-restriction (widen) (point-max))))
           (this-chunk (when (and (<= this-beg use-this-end)
                                  ;; Avoid creating two empty overlays
                                  ;; at the this-end - but what if we are
                                  ;; not creating, just changing the
                                  ;; last overlay ...
                                  ;;
                                  ;; (not (and (= this-beg use-this-end)
                                  ;;           (= use-this-end (1+ (buffer-size)))
                                  ;;           this-after-chunk
                                  ;;           (= 0 (- (overlay-end this-after-chunk) (overlay-start this-after-chunk)))
                                  ;;           ))
                                  )
                         (when (= this-beg 1)
                           (if (= use-this-end 1)
                               (assert (mumamo-fun-eq (mumamo-main-major-mode) this-maj) t)
                             (if this-after-chunk ;; not first
                                 (assert (not (mumamo-fun-eq (mumamo-main-major-mode) this-maj)) t)
                               (assert (mumamo-fun-eq (mumamo-main-major-mode) this-maj) t))))
                         ;;(message "Create chunk %s - %s" this-beg use-this-end)
                         ;;(make-overlay this-beg use-this-end nil nil (not this-is-closed))
                         (make-overlay this-beg use-this-end nil this-insertion-type-beg this-insertion-type-end)
                         ))
           ;; Fix-me: move to mumamo-find-next-chunk-values
           (this-border-fun (when (and this-chunk this-after-chunk)
                              ;;(overlay-get this-after-chunk 'mumamo-next-border-fun)
                              (mumamo-chunk-car this-after-chunk 'mumamo-next-border-fun)
                              ))
           (this-borders (when this-border-fun
                           ;;(msgtrc "(funcall %s %s %s %s)" this-border-fun this-beg this-end this-maj)
                           (funcall this-border-fun this-beg this-end this-maj)))
           ;; Fix-me, check: there is no first border when moving out.
           (this-borders-min (when (= 1 depth-diff)
                               (nth 0 this-borders)))
           ;; Fix-me, check: there is no bottom border when we move
           ;; further "in" since borders are now always inside
           ;; sub-chunks (if I remember correctly...).
           ;;(this-borders-max (when (and this-is-closed
           (this-borders-max (when (and (not this-insertion-type-end)
                                        (/= 1 next-depth-diff))
                               (nth 1 this-borders)))
           )
      ;;(msgtrc "created %s, major=%s" this-chunk this-maj)
      (when (> depth 4) (error "Chunk depth > 4"))
      (setq this-bmin nil)
      (setq this-bmax nil)
      (when this-borders-min (setq this-bmin (- this-borders-min this-beg)))
      (when this-borders-max (setq this-bmax (- this-end this-borders-max)))
      ;;(when this-after-chunk (message "this-after-chunk.this-end=%s, this-beg=%s, this-end=%s" (overlay-end this-after-chunk) this-beg this-end))
      ;;(message "fw-funs=%s" fw-funs)
      (when this-chunk
        (overlay-put this-chunk 'mumamo-is-new t)
        (overlay-put this-chunk 'face (mumamo-background-color depth))
        (overlay-put this-chunk 'mumamo-depth depth)
        ;; Values for next chunk
        (overlay-put this-chunk 'mumamo-next-depth-diff next-depth-diff)
        (assert (symbolp next-major) t)
        (overlay-put this-chunk 'mumamo-next-major next-major)
        ;; Values for this chunk
        ;;(overlay-put this-chunk 'mumamo-is-closed this-is-closed)
        (overlay-put this-chunk 'mumamo-insertion-type-end this-insertion-type-end)
        (overlay-put this-chunk 'mumamo-syntax-min-d this-bmin)
        (overlay-put this-chunk 'mumamo-syntax-max-d this-bmax)
        (overlay-put this-chunk 'mumamo-prev-chunk this-after-chunk)
        (overlay-put this-chunk 'mumamo-next-indent next-indent)
        (when this-after-chunk (overlay-put this-after-chunk 'mumamo-next-chunk this-chunk))

        ;;(msgtrc "\n<<<<<<<<<<<<<<<<< next-depth-diff/depth-diff=%s/%s, this-maj=%s, this-after-chunk=%s" next-depth-diff depth-diff this-maj this-after-chunk)
        ;;(overlay-put this-chunk 'mumamo-next-end-fun next-end-fun)
        (cond
         ((= 1 next-depth-diff)
          (mumamo-chunk-push this-chunk 'mumamo-next-border-fun next-border-fun)
          (mumamo-chunk-push this-chunk 'mumamo-next-end-fun next-end-fun))
         ((= -1 next-depth-diff)
          (mumamo-chunk-pop this-chunk 'mumamo-next-border-fun)
          (mumamo-chunk-pop  this-chunk 'mumamo-next-end-fun))
         ((= 0 next-depth-diff)
          nil)
         (t (error "next-depth-diff=%s" next-depth-diff)))
        ;;(msgtrc "mumamo-next-end-fun=%S" (overlay-get this-chunk 'mumamo-next-end-fun))

        ;; Fix-me: replace 'mumamo-major-mode with multi major mode to make it more flexible.
        (cond
         ((= 1 depth-diff)
          (mumamo-chunk-push this-chunk 'mumamo-major-mode this-maj))
         ((= -1 depth-diff)
          (mumamo-chunk-pop  this-chunk 'mumamo-major-mode)
          )
         (t (error "depth-diff=%s" depth-diff)))

        (overlay-put this-chunk 'mumamo-parseable-by this-pable)
        (overlay-put this-chunk 'created (current-time-string))
        (mumamo-update-chunk-margin-display this-chunk)
        (setq mumamo-last-chunk this-chunk) ;; Use this chunk!!!!
        ;; Get syntax-begin-function for syntax-ppss:
        (let* ((syntax-begin-function
                (mumamo-with-major-mode-fontification this-maj
                  ;; Do like in syntax.el:
                  '(if syntax-begin-function
                       (progn
                         syntax-begin-function)
                     (when (and (not syntax-begin-function)
                                ;; fix-me: How to handle boundp here?
                                (boundp 'font-lock-beginning-of-syntax-function)
                                font-lock-beginning-of-syntax-function)
                       font-lock-beginning-of-syntax-function)))))
          (mumamo-msgfntfy "Got syntax-begin-function, modified=%s" (buffer-modified-p))
          (overlay-put this-chunk 'syntax-begin-function syntax-begin-function))
        )
      ;;(msgtrc "Created %s, this=%s, next=%s" this-chunk this-values next-values)
      this-chunk
      )
    ))

(defun mumamo-update-chunk-margin-display (chunk)
  "Set before-string of CHUNK as spec by `mumamo-margin-use'."
  ;; Fix-me: This is not displayed. Emacs bug?
  ;;(overlay-put this-chunk 'before-string `((margin left-margin) ,(format "%d %s" depth maj)))
  (if (not mumamo-margin-info-mode)
      (overlay-put chunk 'before-string nil)
    (let* ((depth (overlay-get chunk 'mumamo-depth))
           (maj   (mumamo-chunk-car chunk 'mumamo-major-mode))
           (strn (propertize (format "%d" depth)
                             'face (list :inherit (or (mumamo-background-color depth)
                                                      'default)
                                         :foreground "#505050"
                                         :underline t
                                         :slant 'normal
                                         :weight 'normal
                                         )))
           (maj-name (substring (symbol-name maj) 0 -5))
           (strm (propertize maj-name 'face
                             (list :foreground "#a0a0a0" :underline nil
                                   :background (frame-parameter nil 'background-color)
                                   :weight 'normal
                                   :slant 'normal)))
           str
           (margin (mumamo-margin-used)))
      (when (> (length strm) 5) (setq strm (substring strm 0 5)))
      (setq str (concat strn
                        strm
                        (propertize " " 'face 'default)
                        ))
      (overlay-put chunk 'before-string
                   (propertize " " 'display
                               `((margin ,margin) ,str))))))

(defun mumamo-update-chunks-margin-display (buffer)
  "Apply `update-chunk-margin-display' to all chunks in BUFFER."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (let ((chunk (mumamo-find-chunks 1 "margin-disp"))
            (while-n0 0))
        (while (and (mumamo-while 1500 'while-n0 "chunk")
                    chunk)
          (mumamo-update-chunk-margin-display chunk)
          (setq chunk (overlay-get chunk 'mumamo-next-chunk)))))))

(defvar mumamo-margin-used nil)
(make-variable-buffer-local 'mumamo-margin-used)
(put 'mumamo-margin-used 'permanent-local t)

(defun mumamo-margin-used ()
  (setq mumamo-margin-used
        (if (and (boundp 'linum-mode) linum-mode) 'right-margin (nth 0 mumamo-margin-use))))

;; (defun mumamo-set-window-margins-used (win)
;;   "Set window margin according to `mumamo-margin-use'."
;;   ;; Fix-me: old-margin does not work, break it up
;;   (let* ((old-margin-used mumamo-margin-used)
;;          (margin-used  (mumamo-margin-used))
;;          (width  (nth 1 mumamo-margin-use))
;;          (both-widths (window-margins win))
;;          (old-left (eq old-margin-used 'left-margin))
;;          (left (eq margin 'left-margin)))
;;     ;; Change only the margin we used!
;;     (if (not mumamo-margin-info-mode)
;;         (progn
;;           (set-window-margins win
;;                               (if left nil (car both-widths))
;;                               (if (not left) nil (cdr both-widths)))
;;           )
;;       ;;(msgtrc "set-window-margins-used margin-info-mode=t")
;;       (case margin-used
;;         ('left-margin  (set-window-margins win width (when old-left (cdr both-widths))))
;;         ('right-margin (set-window-margins win (car both-widths) width))))))

(defun mumamo-update-buffer-margin-use (buffer)
  ;;(msgtrc "update-buffer-margin-use %s" buffer)
  (when (fboundp 'mumamo-update-chunks-margin-display)
    (with-current-buffer buffer
      (when mumamo-multi-major-mode
        (let* ((old-margin-used mumamo-margin-used)
               (margin-used  (mumamo-margin-used))
               (old-is-left (eq old-margin-used 'left-margin))
               (is-left (eq margin-used 'left-margin))
               (width  (nth 1 mumamo-margin-use))
               (need-update nil))
          (if (not mumamo-margin-info-mode)
              (when old-margin-used
                (setq need-update t)
                (setq old-margin-used nil)
                (if old-is-left
                    (setq left-margin-width 0)
                  (setq right-margin-width 0)))
            (unless (and (eq old-margin-used margin-used)
                         (= width (if old-is-left left-margin-width right-margin-width)))
              (setq need-update t)
              (if is-left
                  (setq left-margin-width width)
                (setq right-margin-width width))
              (unless (eq old-margin-used margin-used)
                (if old-is-left
                    (setq left-margin-width 0)
                  (setq right-margin-width 0)))))
          (when need-update
            (mumamo-update-chunks-margin-display buffer)
            (dolist (win (get-buffer-window-list buffer))
              (set-window-buffer win buffer)))
          )
        ;; Note: window update must be before buffer update because it
        ;; uses old-margin from the call to function margin-used.
        ;; (dolist (win (get-buffer-window-list buffer))
        ;;   (mumamo-set-window-margins-used win))
        ;; (mumamo-update-chunks-margin-display buffer)
        ))))

(defun mumamo-new-chunk-value-min (values)
  (let ((this-values (nth 0 values)))
    (nth 0 this-values)))

(defun mumamo-new-chunk-value-max (values)
  (let ((this-values (nth 0 values)))
    (nth 1 this-values)))

(defun mumamo-new-chunk-equal-chunk-values (chunk values)
  ;;(msgtrc "eq? chunk=%S, values=%S" chunk values)
  (let* (;; Chunk
         (chunk-is-new          (overlay-get chunk 'mumamo-is-new))
         ;;(chunk-is-closed       (overlay-get chunk 'mumamo-is-closed))
         (chunk-insertion-type-end       (overlay-get chunk 'mumamo-insertion-type-end))
         (chunk-next-major      (overlay-get chunk 'mumamo-next-major))
         (chunk-next-end-fun    (mumamo-chunk-car chunk 'mumamo-next-end-fun))
         (chunk-next-border-fun (mumamo-chunk-car chunk 'mumamo-next-border-fun))
         (chunk-next-chunk-diff (overlay-get chunk 'mumamo-next-depth-diff))
         (chunk-beg (overlay-start chunk))
         (chunk-end (overlay-end chunk))
         (chunk-bmin       (overlay-get chunk 'mumamo-syntax-min-d))
         (chunk-bmax       (overlay-get chunk 'mumamo-syntax-max-d))
         (chunk-prev-chunk (overlay-get chunk 'mumamo-prev-chunk))
         (chunk-major-mode (mumamo-chunk-car chunk 'mumamo-major-mode))
         (chunk-pable      (overlay-get chunk 'mumamo-parseable-by))
         (chunk-depth-diff (if chunk-prev-chunk
                               (overlay-get chunk-prev-chunk 'mumamo-next-depth-diff)
                             0))
         ;; Values
         (this-values (nth 0 values))
         (next-values (nth 1 values))
         (values-next-major      (nth 0 next-values))
         (values-next-end-fun    (nth 1 next-values))
         (values-next-border-fun (nth 2 next-values))
         (values-next-depth-diff (nth 3 next-values))
         (values-beg         (nth 0 this-values))
         (values-end         (nth 1 this-values))
         (values-major-mode  (nth 2 this-values))
         (values-bmin        (nth 3 this-values))
         (values-bmax        (nth 4 this-values))
         (values-pable       (nth 5 this-values))
         (values-prev-chunk  (nth 7 this-values))
         (values-insertion-type-beg   (nth 8 this-values))
         (values-insertion-type-end   (nth 9 this-values))
         ;;(values-is-closed   (when values-end t))
         )
    ;;(msgtrc "values=%S" values)
    (and t ;chunk-is-new
         (eq chunk-next-major         values-next-major)

         ;; Can't check chunk-next-end-fun or chunk-next-border-fun
         ;; here since they are fetched from prev chunk:
         ;;(progn (message "eq-c-v: here b: %s /= %s" chunk-next-end-fun values-next-end-fun) t)
         ;;(eq chunk-next-end-fun       values-next-end-fun)
         ;;(progn (message "eq-c-v: here c, %s /= %s" chunk-next-border-fun values-next-border-fun) t)
         ;;(eq chunk-next-border-fun    values-next-border-fun)

         (= chunk-next-chunk-diff     values-next-depth-diff)
         (= chunk-beg values-beg)
         ;;(progn (message "eq-c-v: here b") t)
         ;; (and (equal chunk-is-closed values-is-closed)
         ;;      (or (not chunk-is-closed)
         (and (equal chunk-insertion-type-end values-insertion-type-end)
              (or ;;chunk-insertion-type-end
                  (= chunk-end values-end)))
         ;;(progn (message "eq-c-v: here c, %s /= %s" chunk-major-mode values-major-mode) t)
         (or (= -1 chunk-depth-diff)
             (eq chunk-major-mode values-major-mode))
         ;;(progn (message "eq-c-v: here d") t)
         (equal chunk-pable values-pable)
         ;;(progn (message "eq-c-v: here e") t)
         (eq chunk-prev-chunk values-prev-chunk)
         ;;(progn (message "eq-c-v: here f") t)
         ;;(eq chunk-is-closed values-is-closed)
         (eq chunk-insertion-type-end values-insertion-type-end)
         ;; fix-me: bmin bmax
         ;;(and chunk-bmin values-bmin (= chunk-bmin values-bmin))
         ;;(and chunk-bmax values-bmax (= chunk-bmax values-bmax))
         )
    ))

(defvar mumamo-sub-chunk-families nil
  "Chunk dividing routines for sub chunks.
A major mode in a sub chunk can inherit chunk dividing routines
from multi major modes.  This is the way chunks in chunks is
implemented.

This variable is an association list with entries of the form

  \(CHUNK-MAJOR CHUNK-FAMILY)

where CHUNK-MAJOR is the major mode in a chunk and CHUNK-FAMILY
is a chunk family \(ie the third argument to
`define-mumamo-multi-major-mode'.

You can use the function `mumamo-inherit-sub-chunk-family' to add
to this list.")

(defvar mumamo-multi-local-sub-chunk-families nil
  "Multi major mode local chunk dividing rourines for sub chunks.
Like `mumamo-sub-chunk-families' specific additions for multi
major modes. The entries have the form

  \((CHUNK-MAJOR . MULTI-MAJOR) CHUNK-FAMILY)

Use the function `mumamo-inherit-sub-chunk-family-locally' to add
to this list.")

;;(mumamo-get-sub-chunk-funs 'html-mode)
(defun mumamo-get-sub-chunk-funs (major)
  "Get chunk family sub chunk with major mode MAJOR."
  (let ((rec (or
              (assoc (cons major mumamo-multi-major-mode) mumamo-multi-local-sub-chunk-families)
              (assoc major mumamo-sub-chunk-families))))
    (caddr (cadr rec))))

(defun mumamo-inherit-sub-chunk-family-locally (multi-major multi-using)
  "Add chunk dividing routines from MULTI-MAJOR locally.
The dividing routines from multi major mode MULTI-MAJOR can then
be used in sub chunks in buffers using multi major mode
MULTI-USING."
  (let* ((chunk-family (get multi-major 'mumamo-chunk-family))
         (major      (nth 1 chunk-family)))
    (let ((major-mode major))
      (when (derived-mode-p 'nxml-mode)
        (error "Major mode %s major can't be used in sub chunks" major)))
    (add-to-list 'mumamo-multi-local-sub-chunk-families
                 (list (cons major multi-using) chunk-family))))

(defun mumamo-inherit-sub-chunk-family (multi-major)
  "Inherit chunk dividing routines from multi major modes.
Add chunk family from multi major mode MULTI-MAJOR to
`mumamo-sub-chunk-families'.

Sub chunks with major mode the same as MULTI-MAJOR mode will use
this chunk familyu to find subchunks."
  (let* ((chunk-family (get multi-major 'mumamo-chunk-family))
         (major      (nth 1 chunk-family)))
    (let ((major-mode major))
      (when (derived-mode-p 'nxml-mode)
        (error "Major mode %s major can't be used in sub chunks" major)))
    (add-to-list 'mumamo-sub-chunk-families (list major chunk-family))))

(defun mumamo-find-next-chunk-values (after-chunk from after-change-max chunk-at-after-change)
  "Search forward for start of next chunk.
Return a list with chunk values for next chunk after AFTER-CHUNK
and some values for the chunk after it.

For the first chunk AFTER-CHUNK is nil.  Otherwise the values in stored in AFTER-CHUNK
is used to find the new chunk, its border etc.


See also `mumamo-new-create-chunk' for more information."
  ;;(msgtrc "(find-next-chunk-values %s %s %s %s)" after-chunk from after-change-max chunk-at-after-change)
  ;;(mumamo-backtrace "find-next")
  (when after-chunk
    (unless (eq (overlay-buffer after-chunk)
                (current-buffer))
      (error "mumamo-find-next-chunk-values: after-chunk=%S, cb=%S" after-chunk (current-buffer))))
  (let* ((here (point))
         (max (point-max))
         ;;(after-chunk-is-closed (when after-chunk-valid (overlay-get after-chunk 'mumamo-is-closed)))
         (after-chunk-insertion-type-end (when after-chunk (overlay-get after-chunk 'mumamo-insertion-type-end)))
         ;; Note that "curr-*" values are fetched from "mumamo-next-*" values in after-chunk
         (curr-min (if after-chunk (overlay-end after-chunk) 1))
         (curr-end-fun (when after-chunk
                         (mumamo-chunk-car after-chunk 'mumamo-next-end-fun)))
         (curr-border-fun (when curr-end-fun (mumamo-chunk-car after-chunk 'mumamo-next-border-fun)))
         (curr-syntax-min-max (when curr-border-fun (funcall curr-border-fun
                                                             (overlay-end after-chunk)
                                                             nil nil)))
         (curr-syntax-min (or (car curr-syntax-min-max)
                              (when after-chunk (overlay-end after-chunk))
                              1))
         (search-from (or nil ;from
                          curr-syntax-min))
         ;;(dummy (msgtrc "search-from=%s" search-from))
         (main-chunk-funs (let ((chunk-info (cdr mumamo-current-chunk-family)))
                            (cadr chunk-info)))
         (curr-major (if after-chunk
                         (or
                          ;; 'mumamo-next-major is used when we are going into a sub chunk.
                          (overlay-get after-chunk 'mumamo-next-major)
                          ;; We are going out of a sub chunk.
                          (mumamo-chunk-cadr after-chunk 'mumamo-major-mode))
                       (mumamo-main-major-mode)))
         ;;(dummy (msgtrc "curr-major=%s" curr-major))
         (curr-chunk-funs
          (if (or (not after-chunk)
                  (= 0 (+ (overlay-get after-chunk 'mumamo-depth)
                          (overlay-get after-chunk 'mumamo-next-depth-diff))))
              main-chunk-funs
            (mumamo-get-sub-chunk-funs curr-major)))
         curr-max
         next-max
         curr-max-found
         next-min
         curr-border-min
         curr-border-max
         curr-parseable
         next-fw-exc-fun
         next-indent
         next-major
         curr-end-fun-end
         next-border-fun
         ;; The insertion types for the new chunk
         (curr-insertion-type-beg (when after-chunk after-chunk-insertion-type-end))
         curr-insertion-type-end
         next-depth-diff
         r-point
         )
    (unless (and after-chunk-insertion-type-end
                 (= (1+ (buffer-size)) ;; ie point-max
                    (overlay-end after-chunk)))
      (when (>= max search-from)
        (when curr-end-fun
          ;; If after-change-max is non-nil here then this function has
          ;; been called after changes that are all in one chunk. We
          ;; need to check if the chunk right border have been changed,
          ;; but we do not have to look much longer than the max point
          ;; of the change.
          ;;(message "set after-change-max nil") (setq after-change-max nil)
          (let* ((use-max (if nil ;;after-change-max
                              (+ after-change-max 100)
                            max))
                 (chunk-end (and chunk-at-after-change
                                 (overlay-end chunk-at-after-change)))
                 ;;(use-min (max (- search-from 2) (point-min)))
                 (use-min curr-syntax-min)
                 (possible-end-fun-end t)
                 (end-search-pos use-min))
            ;; The code below takes care of the case when to subsequent
            ;; chunks have the same ending delimiter. (Maybe a while
            ;; loop is bit overkill here.)
            (while (and possible-end-fun-end
                        (not curr-end-fun-end)
                        (< end-search-pos use-max))
              (setq curr-end-fun-end (funcall curr-end-fun end-search-pos use-max))
              (if (not curr-end-fun-end)
                  (setq possible-end-fun-end nil)
                (cond ((and t ;after-chunk-is-closed
                            (< curr-end-fun-end (overlay-end after-chunk)))
                       (setq curr-end-fun-end nil)
                       (setq end-search-pos (1+ end-search-pos)))
                      ;; See if the end is in code
                      ((let* ((syn2-min-max (when curr-border-fun
                                              (funcall curr-border-fun
                                                       (overlay-end after-chunk)
                                                       curr-end-fun-end
                                                       nil)))
                              (syn2-max (or (cadr syn2-min-max)
                                            curr-end-fun-end)))
                         (not (mumamo-end-in-code use-min syn2-max curr-major)))
                       (setq end-search-pos (1+ curr-end-fun-end))
                       (setq curr-end-fun-end nil)
                       ))))
            (unless curr-end-fun-end
              ;; Use old end if valid
              (and after-change-max
                   chunk-end
                   (= -1 (overlay-get chunk-at-after-change 'mumamo-next-depth-diff))
                   (< after-change-max chunk-end)
                   chunk-end))
            ;; Fix-me: Check if old chunk is valid. It is not valid if
            ;; depth-diff = -1 and curr-end-fun-end is not the same as
            ;; before.

            ;; Fix-me: this test should also be made for other chunks
            ;; searches, but this catches most problems I think.
            ;; (or (not curr-end-fun-end)
            ;;     ;; Fix-me: The bug in wiki-090804-js.html indicates that
            ;;     ;; we should not subtract 1 here.  The subchunk there
            ;;     ;; ends with </script> and this can't be in column 1
            ;;     ;; when the line before ends with a // style js comment
            ;;     ;; unless we don't subtract 1.
            ;;     ;;
            ;;     ;; However wiki-strange-hili-080629.html does not work
            ;;     ;; then because then the final " in style="..." is
            ;;     ;; included in the scan done in mumamo-end-in-code.
            ;;     ;;
            ;;     ;; The solution is to check for the syntax borders here.
            ;;     (let* ((syn2-min-max (when curr-border-fun
            ;;                            (funcall curr-border-fun
            ;;                                     (overlay-end after-chunk)
            ;;                                     curr-end-fun-end
            ;;                                     nil)))
            ;;            (syntax-max (or (cadr syn2-min-max)
            ;;                            curr-end-fun-end)))
            ;;       ;;(mumamo-end-in-code syntax-min (- curr-end-fun-end 1) curr-major)
            ;;       ;;
            ;;       ;; fix-me: This should be really in the individual
            ;;       ;; routines that finds possible chunks. Mabye this is
            ;;       ;; possible to fix now when just looking forward for
            ;;       ;; chunks?
            ;;       (mumamo-end-in-code curr-syntax-min syntax-max curr-major)
            ;;       )
            ;;     (setq curr-end-fun-end nil))
            ;; Use old result if valid
            ;; (and nil ;(not curr-end-fun-end)
            ;;      chunk-at-after-change
            ;;      (= -1 (overlay-get chunk-at-after-change 'mumamo-next-depth-diff))
            ;;      (setq curr-end-fun-end (overlay-end chunk-at-after-change)))
            ;;(msgtrc "find-next-chunk-values:curr-end-fun-end after end-in-code=%s" curr-end-fun-end)
            ))
        ;;(msgtrc "find-next-chunk-values:here d, curr-min=%s, after-chunk=%s" curr-min after-chunk)
        (when (listp curr-chunk-funs)
          ;;(msgtrc "find-next-chunk-values:curr-chunk-funs=%s" curr-chunk-funs)
          (setq r-point (point))
          (dolist (fn curr-chunk-funs)
            ;;(msgtrc "find-next-chunk-values:before (r (funcall fn search-from search-from max)), fn=%s search-from=%s, max=%s" fn search-from max)
            (assert (= r-point (point)) t)
            (let* ((r (funcall fn search-from search-from max))
                   (rmin        (nth 0 r))
                   (rmax        (nth 1 r))
                   (rmajor-sub  (nth 2 r))
                   (rborder     (nth 3 r))
                   (rparseable  (nth 4 r))
                   (rfw-exc-fun (nth 5 r))
                   (rborder-fun (nth 6 r))
                   (rindent     (nth 7 r))
                   (rborder-min (when rborder (nth 0 rborder)))
                   (rborder-max (when rborder (nth 1 rborder)))
                   ;;(rmin-found rmin)
                   )
              ;;(msgtrc "find-next-chunk-values:fn=%s, r=%s" fn r)
              (goto-char r-point)
              (when r
                (when rmax (message "mumamo warning: Bad r=%s, nth 1 should be nil" r))
                (unless (or rmin rmax)
                  (error "Bad r=%s, fn=%s" r fn))
                (unless rfw-exc-fun
                  (error "No fw-exc-fun returned from fn=%s, r=%s" fn r))
                (unless rmajor-sub
                  (error "No major mode for sub chunk, fn=%s, r=%s" fn r)))
              (when r
                (mumamo-msgfntfy "  fn=%s, r=%s" fn r)
                (unless rmin (setq rmin (point-max)))
                ;;(unless rmax (setq rmax (point-min)))
                ;; Do not allow zero length chunks
                (unless rmax (setq rmax (point-max)))
                (unless (and (> rmin 1)
                             rmax
                             (= rmin rmax))
                  ;; comparision have to be done differently if we are in an
                  ;; exception part or not.  since we are doing this from top to
                  ;; bottom the rules are:
                  ;;
                  ;; - exception parts always outrules non-exception part.  when
                  ;;   in exception part the min start point should be used.
                  ;; - when in non-exception part the max start point and the
                  ;;   min end point should be used.
                  ;;
                  ;; check if first run:

                  ;; Fix-me: there is some bug here when borders are not
                  ;; included and are not 0 width.
                  (if (not next-min)
                      (progn
                        (setq next-min rmin)
                        (setq curr-border-min rborder-min)
                        (setq next-max rmax)
                        (setq curr-border-max rborder-max)
                        ;;(setq curr-max-found rmin-found)
                        (setq curr-parseable rparseable)
                        (setq next-fw-exc-fun rfw-exc-fun)
                        (setq next-border-fun rborder-fun)
                        (setq next-indent rindent)
                        (setq next-major rmajor-sub))
                    (if rmajor-sub
                        (if next-major
                            (when (or (not next-min)
                                      (< rmin next-min))
                              (setq next-min rmin)
                              (setq curr-border-min rborder-min)
                              (when rmax (setq max rmax))
                              (setq curr-border-max rborder-max)
                              ;;(when rmin-found (setq curr-max-found t))
                              (setq curr-parseable rparseable)
                              (setq next-fw-exc-fun rfw-exc-fun)
                              (setq next-border-fun rborder-fun)
                              (setq next-indent rindent)
                              (setq next-major rmajor-sub))
                          (setq next-min rmin)
                          (setq curr-border-min rborder-min)
                          (when rmax (setq max rmax))
                          (setq curr-border-max rborder-max)
                          ;;(when rmin-found (setq curr-max-found t))
                          (setq curr-parseable rparseable)
                          (setq next-fw-exc-fun rfw-exc-fun)
                          (setq next-border-fun rborder-fun)
                          (setq next-indent rindent)
                          (setq next-major rmajor-sub))
                      (unless next-major
                        (when (> next-min rmin)
                          (setq next-min rmin)
                          (setq curr-border-min rborder-min))
                        (when (and rmax max
                                   (> rmax max))
                          ;;(setq max-found rmin-found)
                          ;;(when rmin-found (setq curr-max-found t))
                          (when rmax (setq max rmax))
                          (setq curr-border-max rborder-max))
                        ))))
                (mumamo-msgfntfy "next-min/max=%s/%s border=%s/%s search-from=%s" next-min max curr-border-min curr-border-max search-from)
                ;; check!
                (when (and next-min max)
                  ;;(assert (>= next-min search-from) t)
                  (assert (<= search-from max) t)
                  (when curr-border-min
                    (assert (<= next-min curr-border-min) t)
                    (assert (<= curr-border-min max) t))
                  (when curr-border-max
                    (assert (<= next-min curr-border-max) t)
                    (assert (<= curr-border-max max) t))))
              )))
        (goto-char here)
        (setq curr-max-found (or curr-max-found curr-end-fun-end))
        (when t ;curr-max-found
          (setq curr-max (if max max (point-max)))
          (setq curr-max (min (if next-min next-min curr-max)
                              (if curr-end-fun-end curr-end-fun-end curr-max))))
        ;;(setq curr-max nil)
        (setq next-depth-diff (cond
                               ( (and curr-max curr-end-fun-end
                                       (= curr-max curr-end-fun-end))
                                 -1)
                               ( (= curr-max (1+ (buffer-size)))
                                 0)
                               ( t 1)))
        (when (= -1 next-depth-diff) ;; We will pop it from 'mumamo-major-mode
          (setq next-major nil))
        (when curr-max
          (unless (>= curr-max curr-min)
            (error "curr-max is not >= curr-min")))
        ;;(setq curr-is-closed (and curr-max (< 1 curr-max)))
        (when (and curr-max (= 1 curr-max))
          (assert (mumamo-fun-eq curr-major (mumamo-main-major-mode)) t)
          )
        (assert (symbolp next-major) t)
        ;; Fix-me: see for example rr-min8.php
        (when (or ;;(not after-chunk)
                  (= curr-max (1+ (buffer-size)))
                  (cond
                   ((= next-depth-diff 1)
                    next-border-fun)
                   ((= next-depth-diff -1)
                    next-border-fun)
                   ((= next-depth-diff 0)
                    t)
                   (t (error "next-depth-diff=%s" next-depth-diff))))
          (setq curr-insertion-type-end t))
        (let ((current (list curr-min curr-max curr-major curr-border-min curr-border-max curr-parseable
                             curr-chunk-funs after-chunk
                             ;;curr-is-closed
                             curr-insertion-type-beg
                             curr-insertion-type-end
                             ))
              (next    (list next-major next-fw-exc-fun next-border-fun next-depth-diff next-indent)))
          ;;(msgtrc "find-next-chunk-values=> current=%s, next=%s" current next)
          (list current next))))))

;; Fix-me: This should check if the new chunk should be
;; parsed or not
;; (defsubst mumamo-chunk-nxml-parseable (chunk)
;;   (mumamo-fun-eq (mumamo-main-major-mode)
;;       (mumamo-chunk-major-mode xml-chunk)))

(defun mumamo-valid-nxml-point (pos)
  "Return non-nil if position POS is in an XML chunk."
  (memq 'nxml-mode (get-text-property pos 'mumamo-parseable-by)))

(defun mumamo-valid-nxml-chunk (chunk)
  "Return t if chunk CHUNK should be valid XML."
  (when chunk
    (let ((major-mode (mumamo-chunk-major-mode chunk))
          (region (overlay-get chunk 'mumamo-region))
          (parseable-by (overlay-get chunk 'mumamo-parseable-by)))
      ;;(message "mumamo-valid-nxml-chunk: major-mode=%s, parseble-by=%s" major-mode parseable-by)
      (or region
          (derived-mode-p 'nxml-mode)
          (memq 'nxml-mode parseable-by)))))

;; A good test case for the use of this is the troublesome code in the
;; first line of xml-as-string.php in nxml/nxhtml/bug-tests. Currently
;; this test code is however splitted and it looks like the code below
;; can't handle the line above if the line looks like below. The ?> is
;; still thought to be a border.  Does this mean that ' is not treated
;; as a string separator?
;;
;; <?php header("Content-type:application/xml; charset=utf-8"); echo '<?xml version="1.0" encoding="utf-8"?>'; ?>
;;
;; However there are the reverse cases also, in lines like
;;
;;   href="<?php $this->url($url); ?>"
;;   <!--    <td><?php insert_a_lot_of_html(); ?>
;;
;; These are supposedly handled by using this test at the right
;; place... However it is not very clear in all cases whether chunk
;; dividers in comments and strings should be valid or not...
;;
;; For example in the first case above the php divider should be
;; valid.  Probably it should be that in the second case too, but how
;; should mumamo know that?
;;
;; Fix-me: I think a per "chunk divider function + context" flag is
;; needed to handle this. Probably this will work the same for all web
;; dev things, ie the opening sub chunk divider is ALWAYS
;; valid. However that is not true for things like CSS, Javascript etc
;; in (X)HTML.

(defun mumamo-end-in-code (syntax-start syntax-end major)
  "Return t if possible syntax end is not in a string or comment.
Assume that the sexp syntax is nil at SYNTAX-START return t if
position SYNTAX-END is not in a string or comment according to
the sexp syntax using major mode MAJOR."
  ;; Fix-me: This can't always detect html comments: <!--
  ;; ... -->. Could this be solved by RMS suggestion with a
  ;; function/defmacro that binds variables to their global values?
  (mumamo-msgfntfy "point-min,max=%s,%s syntax-start,end=%s,%s, major=%s" (point-min) (point-max) syntax-start syntax-end major)
  ;;(msgtrc "end-in-code:here a  after-chunk=%s" (when (boundp 'after-chunk) after-chunk))
  (assert (and syntax-start syntax-end) t)
  (let ((doesnt-here (point))
        doesnt-ret)
    (save-restriction
      (widen)
      ;;(msgtrc "end-in-code:here a2  after-chunk=%s" (when (boundp 'after-chunk) after-chunk))
      (mumamo-with-major-mode-fontification major
        `(let (ppss)
           ;; fix-me: Use main major mode, and `syntax-ppss'. Change the
           ;; defadvice of this to make that possible.
           ;;(msgtrc "end-in-code:here b  after-chunk=%s" (when (boundp 'after-chunk) after-chunk))
           (setq ppss (parse-partial-sexp ,syntax-start (+ ,syntax-end 0)))
           ;;(msgtrc "end-in-code %s %s %s:ppss=%S" ,syntax-start ,syntax-end ',major ppss)
           ;;(msgtrc "end-in-code:here c  after-chunk=%s" (when (boundp 'after-chunk) after-chunk))
           ;; If inside a string or comment then the end marker is
           ;; invalid:
           ;;(msgtrc "mumamo-end-in-code:ppss=%s" ppss)
           (if (or (nth 3 ppss)
                   (nth 4 ppss))
               (progn
                 ;;(msgtrc "invalid end, syntax-end =%s" syntax-end)
                 (setq doesnt-ret nil)
                 (if (nth 4 ppss) ;; in comment, check if single line comment
                     (let ((here (point))
                           eol-pos)
                       ;;(msgtrc "end-in-code, was in comment, ppss=%S" ppss)
                       (goto-char ,syntax-end)
                       (setq eol-pos (line-end-position))
                       (goto-char here)
                       (setq ppss (parse-partial-sexp ,syntax-start (+ eol-pos 1)))
                       ;;(msgtrc "end-in-code, in comment, new ppss %s %s=%S" ,syntax-start (+ eol-pos 1) ppss)
                       (unless (nth 4 ppss)
                         (setq doesnt-ret t)))))
             (setq doesnt-ret t)
             ;;(msgtrc "valid end, syntax-end =%s" syntax-end)
             ))))
    (goto-char doesnt-here)
    ;;(msgtrc "end-in-code:ret=%s" doesnt-ret)
    doesnt-ret))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Easy chunk defining

(defun mumamo-quick-chunk-forward (pos
                                   min max
                                   begin-mark end-mark inc mode
                                   mark-is-border)
  ;;(msgtrc "quick-chunk-forward %s %s %s" pos min max)
  (let ((search-fw-exc-start
         `(lambda (pos max)
            (let ((exc-start
                   (if ,inc
                       (mumamo-chunk-start-fw-str-inc pos max ,begin-mark)
                     (mumamo-chunk-start-fw-str pos max ,begin-mark))))
              (when exc-start
                (list exc-start mode nil)))))
        (search-fw-exc-end
         `(lambda (pos max)
            ;;(msgtrc "search-fw-exc-end %s %s, inc=%s, end-mark=%s" pos max ,inc ,end-mark)
            (save-match-data
              (let ((ret (if ,inc
                             (mumamo-chunk-end-fw-str-inc pos max ,end-mark)
                           (mumamo-chunk-end-fw-str pos max ,end-mark))))
                ;;(msgtrc "search-fw-exc-end ret=%s" ret)
                ret))))
        (find-borders
         (when mark-is-border
           `(lambda (start end exc-mode)
              (let ((start-border)
                    (end-border))
                (if (and ,inc);; exc-mode)
                    (progn
                      (when start
                        (setq start-border
                              (+ start (length ,begin-mark))))
                      (when end
                        (setq end-border
                              (- end (length ,end-mark)))))
                  (if (and (not ,inc) (not exc-mode))
                      (progn
                        (when start
                          (setq start-border
                                (+ start (length ,end-mark))))
                        (when end
                          (setq end-border
                                (- end (length ,begin-mark)))))))
                (when (or start-border end-border)
                  (mumamo-msgfntfy "quick.start-border/end=%s/%s, start/end=%s/%s exc-mode=%s" start-border end-border start end exc-mode)
                  (list start-border end-border)))))))
    (mumamo-possible-chunk-forward pos max
                                   search-fw-exc-start
                                   search-fw-exc-end
                                   find-borders)))

(defun mumamo-quick-static-chunk (pos
                                  min max
                                  begin-mark end-mark inc mode
                                  mark-is-border)
  (if t
      (mumamo-quick-chunk-forward pos min max begin-mark end-mark inc mode mark-is-border)
    ;; (let ((old (mumamo-quick-static-chunk-old pos min max begin-mark end-mark inc mode mark-is-border))
    ;;       (new (mumamo-quick-chunk-forward pos min max begin-mark end-mark inc mode mark-is-border)))
    ;;   (unless (equal old new) (msgtrc "equal=%s\n\told=%S\n\tnew=%S" (equal old new) old new))
    ;;   (if nil old new))
    ))

;; (defun mumamo-quick-static-chunk-old (pos
;;                                       min max
;;                                       begin-mark end-mark inc mode
;;                                       mark-is-border)
;;   "Quick way to make a chunk function with static dividers.
;; Here is an example of how to use it:

;;   (defun mumamo-chunk-embperl-<- (pos min max)
;;     \"Find [- ... -], return range and perl-mode.\"
;;     (mumamo-quick-static-chunk pos min max \"[-\" \"-]\" nil 'perl-mode))

;; As you can see POS, MIN and MAX comes from argument of the
;; function you define.

;; BEGIN-MARK should be a string that begins the chunk.
;; END-MARK should be a string that ends the chunk.

;; If INC is non-nil then the dividers are included in the chunk.
;; Otherwise they are instead made parts of the surrounding chunks.

;; MODE should be the major mode for the chunk.

;; If MARK-IS-BORDER is non-nil then the marks are just borders and
;; not supposed to have the same syntax as the inner part of the

;; Fix-me: This can only be useful if the marks are included in the
;; chunk, ie INC is non-nil.  Should not these two arguments be
;; mixed then?
;; "
;;   (mumamo-msgfntfy "quick.pos=%s min,max=%s,%s begin-mark/end=%s/%s mark-is-border=%s" pos min max begin-mark end-mark mark-is-border)
;;   (let ((search-bw-exc-start
;;          `(lambda (pos min)
;;             (let ((exc-start
;;                    (if ,inc
;;                        (mumamo-chunk-start-bw-str-inc pos min begin-mark)
;;                      (mumamo-chunk-start-bw-str pos min begin-mark))))
;;               (when (and exc-start
;;                          (<= exc-start pos))
;;                 (cons exc-start mode)))))
;;         (search-bw-exc-end
;;          `(lambda (pos min)
;;             (if ,inc
;;                 (mumamo-chunk-end-bw-str-inc pos min ,end-mark)
;;               (mumamo-chunk-end-bw-str pos min ,end-mark))))
;;         (search-fw-exc-start
;;          `(lambda (pos max)
;;             (if ,inc
;;                 (mumamo-chunk-start-fw-str-inc pos max ,begin-mark)
;;               (mumamo-chunk-start-fw-str pos max ,begin-mark))))
;;         (search-fw-exc-end
;;          `(lambda (pos max)
;;             (save-match-data
;;               (if ,inc
;;                   (mumamo-chunk-end-fw-str-inc pos max ,end-mark)
;;                 (mumamo-chunk-end-fw-str pos max ,end-mark)))))
;;         (find-borders
;;          (when mark-is-border
;;            `(lambda (start end exc-mode)
;;               (let ((start-border)
;;                     (end-border))
;;                 (if (and ,inc exc-mode)
;;                     (progn
;;                       (when start
;;                         (setq start-border
;;                               (+ start (length ,begin-mark))))
;;                       (when end
;;                         (setq end-border
;;                               (- end (length ,end-mark)))))
;;                   (if (and (not ,inc) (not exc-mode))
;;                       (progn
;;                         (when start
;;                           (setq start-border
;;                                 (+ start (length ,end-mark))))
;;                         (when end
;;                           (setq end-border
;;                                 (- end (length ,begin-mark)))))))
;;                 (when (or start-border end-border)
;;                   (mumamo-msgfntfy "quick.start-border/end=%s/%s, start/end=%s/%s exc-mode=%s" start-border end-border start end exc-mode)
;;                   (list start-border end-border)))))))
;;     (mumamo-find-possible-chunk pos min max
;;                                 search-bw-exc-start
;;                                 search-bw-exc-end
;;                                 search-fw-exc-start
;;                                 search-fw-exc-end
;;                                 find-borders)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Changing the major mode that the user sees

(defvar mumamo-unread-command-events-timer nil)
(make-variable-buffer-local 'mumamo-unread-command-events-timer)

(defun mumamo-unread-command-events (command-keys new-major old-last-command)
  "Sync new keymaps after changing major mode in a timer.
Also tell new major mode.

COMMAND-KEYS is the keys entered after last command and the call
to `mumamo-idle-set-major-mode' \(which is done in an idle
timer).  Those keys are added to `unread-command-events' so they
can be used in the new keymaps.  They should be in the format
returned by

  \(listify-key-sequence (this-command-keys-vector))

NEW-MAJOR mode is the new major mode.

OLD-LAST-COMMAND is the value of `last-command' after switching
major mode.  \(This is cleared by the function `top-level' so
this function will not see it since it is run in a timer.)"
  (mumamo-condition-case err
      (progn
        ;; last-command seems to be cleared by top-level so set it
        ;; back here.
        (unless last-command
          (setq last-command old-last-command))
        (when (< 0 (length command-keys))
          ;;(setq last-command-char nil) ;; For `viper-command-argument'
          (setq unread-command-events (append command-keys nil)))
        (message "Switched to %s" new-major))
    (error
     (let ((mumamo-display-error-lwarn t))
       (mumamo-display-error 'mumamo-unread-command-events "err=%s" err)))))

(defvar mumamo-idle-set-major-mode-timer nil)
(make-variable-buffer-local 'mumamo-idle-set-major-mode-timer)
(put 'mumamo-idle-set-major-mode-timer 'permanent-local t)

(defun mumamotemp-pre-command ()
  "Temporary command for debugging."
  (message "mumamotemp-pre 1: modified=%s %s" (buffer-modified-p) (current-buffer)))
(defun mumamotemp-post-command ()
  "Temporary command for debugging."
  (message "mumamotemp-post 1: modified=%s %s" (buffer-modified-p) (current-buffer)))
(put 'mumamotemp-pre-command 'permanent-local-hook t)
(put 'mumamotemp-post-command 'permanent-local-hook t)
(defun mumamotemp-start ()
  "Temporary command for debugging."
  (add-hook 'post-command-hook 'mumamotemp-post-command nil t)
  (add-hook 'pre-command-hook 'mumamotemp-pre-command nil t))

(defsubst mumamo-cancel-idle-set-major-mode ()
  (when (timerp mumamo-idle-set-major-mode-timer)
    (cancel-timer mumamo-idle-set-major-mode-timer))
  (setq mumamo-idle-set-major-mode-timer nil))

(defun mumamo-request-idle-set-major-mode ()
  "Setup to change major mode from chunk when Emacs is idle."
  (mumamo-cancel-idle-set-major-mode)
  (setq mumamo-idle-set-major-mode-timer
        (run-with-idle-timer
         mumamo-set-major-mode-delay
         nil
         'mumamo-idle-set-major-mode (current-buffer) (selected-window))))

(defvar mumamo-done-first-set-major nil)
(make-variable-buffer-local 'mumamo-done-first-set-major)
(put 'mumamo-done-first-set-major 'permanent-local t)

;; Fix-me: Add a property to the symbol instead (like in CUA).
(defvar mumamo-safe-commands-in-wrong-major
  '(self-insert-command
    fill-paragraph ;; It changes major mode
    forward-char
    viper-forward-char
    backward-char
    viper-backward-char
    next-line
    viper-next-line
    previous-line
    viper-previous-line
    scroll-down
    cua-scroll-down
    scroll-up
    cua-scroll-up
    move-beginning-of-line
    move-end-of-line
    nonincremental-search-forward
    nonincremental-search-backward
    mumamo-backward-chunk
    mumamo-forward-chunk
    ;; Fix-me: add more
    )
  )

(defun mumamo-fetch-local-map (major)
  "Fetch local keymap for major mode MAJOR.
Do that by turning on the major mode in a new buffer.  Add the
keymap to `mumamo-major-modes-local-maps'.

Return the fetched local map."
  (let (temp-buf-name
        temp-buf
        local-map)
    (setq temp-buf-name (concat "mumamo-fetch-major-mode-local-"
                                (symbol-name major)))
    (setq temp-buf (get-buffer temp-buf-name))
    (when temp-buf (kill-buffer temp-buf))
    (setq temp-buf (get-buffer-create temp-buf-name))
    (with-current-buffer temp-buf
      (let ((mumamo-fetching-major t))
        (funcall major))
      (setq local-map (current-local-map))
      (when local-map (setq local-map (copy-keymap (current-local-map))))
      (add-to-list 'mumamo-major-modes-local-maps
                   (cons major-mode local-map)))
    (kill-buffer temp-buf)
    local-map))

(defvar mumamo-post-command-chunk nil)
(make-variable-buffer-local 'mumamo-post-command-chunk)

(defun mumamo-post-command-get-chunk (pos)
  "Get chunk at POS fast."
  (let ((have-regions (and (boundp 'mumamo-regions)
                           mumamo-regions)))
    (when have-regions (setq mumamo-post-command-chunk nil))
    (if (and mumamo-post-command-chunk
             (overlayp mumamo-post-command-chunk)
             ;;(progn (message "here a=%s" mumamo-post-command-chunk) t)
             (overlay-buffer mumamo-post-command-chunk)
             ;;(progn (message "here b=%s" mumamo-post-command-chunk) t)
             (< pos (overlay-end mumamo-post-command-chunk))
             ;;(progn (message "here c=%s" mumamo-post-command-chunk) t)
             (>= pos (overlay-start mumamo-post-command-chunk))
             ;;(progn (message "here d=%s" mumamo-post-command-chunk) t)
             (mumamo-chunk-major-mode mumamo-post-command-chunk)
             ;;(progn (msgtrc "here e=%s" mumamo-post-command-chunk) t)
             )
        mumamo-post-command-chunk
      ;;(msgtrc "--------------- new post-command-chunk")
      (setq mumamo-post-command-chunk
            (or (unless have-regions (mumamo-get-existing-new-chunk-at (point) nil))
                (mumamo-find-chunks (point) "post-command-get-chunk"))))))

;; (setq mumamo-set-major-mode-delay 10)
(defun mumamo-set-major-post-command ()
  "Change major mode if necessary after a command.
If the major mode for chunk at `window-point' differ from current
major mode then change major mode to that for the chunk.  If
however `mumamo-set-major-mode-delay' is greater than 0 just
request a change of major mode when Emacs is idle that long.

See the variable above for an explanation why a delay might be
needed \(and is the default)."
  ;;(msgtrc "set-major-post-command here")
  (let* ((in-pre-hook (memq 'mumamo-set-major-pre-command pre-command-hook))
         (ovl (unless in-pre-hook (mumamo-post-command-get-chunk (point))))
         (major (when ovl (mumamo-chunk-major-mode ovl)))
         (set-it-now (not (or in-pre-hook (mumamo-fun-eq major major-mode)))))
    ;;(msgtrc "set-major-post-command ovl=%s, in-pre-hook=%s" ovl in-pre-hook)
    (if (not set-it-now)
        (unless (mumamo-fun-eq major major-mode)
          (when mumamo-idle-set-major-mode-timer
            (mumamo-request-idle-set-major-mode)))
      (if mumamo-done-first-set-major
          (if (<= 0 mumamo-set-major-mode-delay)
              ;; Window point has been moved to a new chunk with a new
              ;; major mode. Major mode will not be changed directly,
              ;; but in an idle timer or in pre-command-hook. To avoid
              ;; that the user get the wrong key bindings for the new
              ;; chunk fetch the local map directly and apply that.
              (let* ((map-rec (assoc major mumamo-major-modes-local-maps))
                     (map (cdr map-rec)))
                (unless map
                  (setq map (mumamo-fetch-local-map major)))
                (unless (eq map 'no-local-map)
                  (use-local-map map))
                (add-hook 'pre-command-hook 'mumamo-set-major-pre-command nil t)
                (mumamo-request-idle-set-major-mode))
            (mumamo-set-major major ovl)
            (message "Switched to %s" major-mode))
        (mumamo-set-major major ovl)))))

(defun mumamo-set-major-pre-command ()
  "Change major mode if necessary before a command.
When the key sequence that invoked the command is in current
local map and major mode is not the major mode for the current
mumamo chunk then set major mode to that for the chunk."
  (mumamo-condition-case err
      ;; First see if we can avoid changing major mode
      (if (memq this-command mumamo-safe-commands-in-wrong-major)
          (mumamo-request-idle-set-major-mode)
        ;;(message "pre point=%s" (point))
        (let* ((ovl (mumamo-find-chunks (point) "mumamo-set-major-pre-command"))
               (major (mumamo-chunk-major-mode ovl)))
          ;;(message "pre point=%s" (point))
          (if (not major)
              (lwarn '(mumamo-set-major-pre-command) :error "major=%s" major)
            (when (or (not (mumamo-fun-eq major-mode major))
                      (not (mumamo-set-major-check-keymap)))
              (setq major-mode nil)
              (mumamo-set-major major ovl)
              ;; Unread the last command key sequence
              (setq unread-command-events
                    (append (listify-key-sequence (this-command-keys-vector))
                            unread-command-events))
              ;; Some commands, like `viper-command-argument' need to
              ;; know the last command, so tell them.
              (setq this-command (lambda ()
                                   (interactive)
                                   (setq this-command last-command)))))))
    (error
     (mumamo-display-error 'mumamo-set-major-pre-command
                           "cb:%s, %s" (current-buffer) (error-message-string err)))))

(defun mumamo-idle-set-major-mode (buffer window)
  "Set major mode from mumamo chunk when Emacs is idle.
Do this only if current buffer is BUFFER and then do it in window
WINDOW.

See the variable `mumamo-set-major-mode-delay' for an
explanation."
  (save-match-data ;; runs in idle timer
    (mumamo-msgfntfy "mumamo-idle-set-major-mode b=%s, window=%s" buffer window)
    (with-selected-window window
      ;; According to Stefan Monnier we need to set the buffer too.
      (with-current-buffer (window-buffer window)
        (when (eq buffer (current-buffer))
          (mumamo-condition-case err
              ;;(let* ((ovl (mumamo-get-chunk-at (point)))
              ;;(message "idle point=%s" (point))
              (let* ((ovl (mumamo-find-chunks (point) "mumamo-idle-set-major-mode"))
                     (major (mumamo-chunk-major-mode ovl))
                     (modified (buffer-modified-p)))
                ;;(message "idle point=%s" (point))
                (unless (mumamo-fun-eq major major-mode)
                  ;;(message "mumamo-set-major at A")
                  (mumamo-set-major major ovl)
                  ;; Fix-me: This is a bug workaround. Possibly in Emacs.
                  (when (and (buffer-modified-p)
                             (not modified))
                    (set-buffer-modified-p nil))
                  ;; sync keymap
                  (when (timerp mumamo-unread-command-events-timer)
                    (cancel-timer mumamo-unread-command-events-timer))
                  (when unread-command-events
                    ;; Save unread keys before calling `top-level' which
                    ;; will clear them.
                    (setq mumamo-unread-command-events-timer
                          (run-with-idle-timer
                           0 nil
                           'mumamo-unread-command-events
                           unread-command-events
                           major last-command))
                    (top-level)
                    )))
            (error
             (mumamo-display-error 'mumamo-idle-set-major-mode
                                   "cb=%s, err=%s" (current-buffer) err))))))))

(defun mumamo-post-command-1 (&optional no-debug)
  "See `mumamo-post-command'.
Turn on `debug-on-error' unless NO-DEBUG is nil."
  (unless no-debug (setq debug-on-error t))
  (setq mumamo-find-chunks-level 0)
  (mumamo-msgfntfy "mumamo-post-command-1 ENTER: font-lock-mode=%s" font-lock-mode)
  (if font-lock-mode
      (mumamo-set-major-post-command)
    ;;(mumamo-on-font-lock-off)
    )
  ;;(msgtrc "mumamo-post-command-1 EXIT: font-lock-keywords-only =%s" (default-value 'font-lock-keywords-only))
  )




(defvar mumamo-bug-3467-w14 41)
(defvar mumamo-bug-3467-w15 51)
;;(mumamo-check-has-bug3467 t)
;;(kill-local-variable 'mumamo-bug-3467-w14)
(defun mumamo-check-has-bug3467 (verbose)
  (let ((has-bug nil))
    (with-temp-buffer
      (let ((mumamo-bug-3467-w14 42)
            (mumamo-bug-3467-w15 52))
        (when verbose (message "mumamo-bug-3467-w14 maybe let: in buffer %s=%S, global=%S" (current-buffer) mumamo-bug-3467-w14 (default-value 'mumamo-bug-3467-w14)))
        (when verbose (message "mumamo-bug-3467-w15 maybe let: in buffer %s=%S, global=%S" (current-buffer) mumamo-bug-3467-w15 (default-value 'mumamo-bug-3467-w15)))
        (set (make-local-variable 'mumamo-bug-3467-w14) 43)
        (set-default 'mumamo-bug-3467-w14 44)
        (set-default 'mumamo-bug-3467-w15 54)
        (when verbose (message "mumamo-bug-3467-w14 maybe let: in buffer %s=%S, global=%S" (current-buffer) mumamo-bug-3467-w14 (default-value 'mumamo-bug-3467-w14)))
        (when verbose (message "mumamo-bug-3467-w15 maybe let: in buffer %s=%S, global=%S" (current-buffer) mumamo-bug-3467-w15 (default-value 'mumamo-bug-3467-w15))))
      (when verbose (message "mumamo-bug-3467-w14 top level: in buffer %s=%S, global=%S" (current-buffer) mumamo-bug-3467-w14 (default-value 'mumamo-bug-3467-w14)))
      (when (/= mumamo-bug-3467-w14 43) (setq has-bug t))
      (when (/= (default-value 'mumamo-bug-3467-w14) 41) (setq has-bug t))
      (when verbose (message "mumamo-bug-3467-w15 top level: in buffer %s=%S, global=%S" (current-buffer) mumamo-bug-3467-w15 (default-value 'mumamo-bug-3467-w15)))
      )
    (when verbose (message "mumamo-bug-3467-w14 top level: in buffer %s=%S, global=%S" (current-buffer) mumamo-bug-3467-w14 (default-value 'mumamo-bug-3467-w14)))
    (when verbose (message "mumamo-bug-3467-w15 top level: in buffer %s=%S, global=%S" (current-buffer) mumamo-bug-3467-w15 (default-value 'mumamo-bug-3467-w15)))
    (or has-bug
        (local-variable-p 'mumamo-bug-3467-w14)
        (/= (default-value 'mumamo-bug-3467-w14) 41)
        )
    ))

(defvar mumamo-has-bug3467 (mumamo-check-has-bug3467 nil))

(defun mumamo-emacs-start-bug3467-timer-if-needed ()
  "Work around for Emacs bug 3467. The only one I have found."
  (when mumamo-has-bug3467
    (run-with-idle-timer 0 nil 'mumamo-emacs-bug3467-workaround)))

(defun mumamo-emacs-bug3467-workaround ()
  "Work around for Emacs bug 3467. The only one I have found."
  (set-default 'font-lock-keywords-only nil))




(defun mumamo-post-command ()
  "Run this in `post-command-hook'.
Change major mode if necessary."
  ;;(msgtrc "mumamo-post-command")
  (when mumamo-multi-major-mode
    (mumamo-condition-case err
        (mumamo-post-command-1 t)
      (error
       (mumamo-msgfntfy "mumamo-post-command %S" err)
       ;; Warnings are to disturbing when run in post-command-hook,
       ;; but this message is important so show it with an highlight.
       (message
        (propertize
         "%s\n- Please try M-: (mumamo-post-command-1) to see what happened."
         'face 'highlight)
        (error-message-string err))))))

(defun mumamo-change-major-function ()
  "Function added to `change-major-mode-hook'.
Remove mumamo when changing to a new major mode if the change is
not done because point was to a new chunk."
  (unless mumamo-set-major-running
    (mumamo-turn-off-actions)))

(defun mumamo-derived-from-mode (major from-mode)
  "Return t if major mode MAJOR is derived from FROM-MODE."
  (let ((major-mode major))
    (derived-mode-p from-mode)))

;; This is the new version of add-hook. For its origin see
;; http://lists.gnu.org/archive/html/emacs-devel/2007-12/msg00169.html
;;
;;(unless (> emacs-major-version 22)
(defvar mumamo-test-add-hook nil
  "Internal use.")
(unless (and t
             (let ((has-it nil))
               ;;(add-hook 'mumamo-test-add-hook 'mumamo-jit-lock-after-change nil t)
               (add-hook 'mumamo-test-add-hook 'mumamo-after-change nil t)
               (setq has-it (eq 'permanent-local-hook
                                (get 'mumamo-test-add-hook 'permanent-local)))
               has-it))
  (defun add-hook (hook function &optional append local)
    "Add to the value of HOOK the function FUNCTION.
FUNCTION is not added if already present.
FUNCTION is added (if necessary) at the beginning of the hook list
unless the optional argument APPEND is non-nil, in which case
FUNCTION is added at the end.

The optional fourth argument, LOCAL, if non-nil, says to modify
the hook's buffer-local value rather than its default value.
This makes the hook buffer-local if needed, and it makes t a member
of the buffer-local value.  That acts as a flag to run the hook
functions in the default value as well as in the local value.

HOOK should be a symbol, and FUNCTION may be any valid function.  If
HOOK is void, it is first set to nil.  If HOOK's value is a single
function, it is changed to a list of functions."
    (or (boundp hook) (set hook nil))
    (or (default-boundp hook) (set-default hook nil))
    (if local (unless (local-variable-if-set-p hook)
                (set (make-local-variable hook) (list t)))
      ;; Detect the case where make-local-variable was used on a hook
      ;; and do what we used to do.
      (unless (and (consp (symbol-value hook)) (memq t (symbol-value hook)))
        (setq local t)))
    (let ((hook-value (if local (symbol-value hook) (default-value hook))))
      ;; If the hook value is a single function, turn it into a list.
      (when (or (not (listp hook-value)) (eq (car hook-value) 'lambda))
        (setq hook-value (list hook-value)))
      ;; Do the actual addition if necessary
      (unless (member function hook-value)
        (setq hook-value
              (if append
                  (append hook-value (list function))
                (cons function hook-value))))
      ;; Set the actual variable
      (if local
          (progn
            ;; If HOOK isn't a permanent local,
            ;; but FUNCTION wants to survive a change of modes,
            ;; mark HOOK as partially permanent.
            (and (symbolp function)
                 (get function 'permanent-local-hook)
                 (not (get hook 'permanent-local))
                 (put hook 'permanent-local 'permanent-local-hook))
            (set hook hook-value))
        (set-default hook hook-value))))
  )


(defvar mumamo-survive-hooks
  '(
    ;;     activate-mark-hook after-change-functions after-save-hook
    ;;     before-save-functions auto-save-hook before-revert-hook
    ;;     buffer-access-fontify-functions calendar-load-hook
    ;;     command-line-functions compilation-finish-function
    ;;     deactivate-mark-hook find-file-hook
    ;;     find-file-not-found-functions first-change-hook
    ;;     kbd-macro-termination-hook kill-buffer-hook
    ;;     kill-buffer-query-functions menu-bar-update-hook
    ;;     post-command-hook pre-abbrev-expand-hook pre-command-hook
    ;;     write-contents-functions write-file-functions
    ;;     write-region-annotate-functions
    ;;     c-special-indent-hook
    ))

;;
;; Emulation modes
;;
;; These variables should have 'permanant-local t set in their
;; packages IMO, but now they do not have that.
(eval-after-load 'viper-cmd
  (progn
    (put 'viper-after-change-functions 'permanent-local t)
    (put 'viper-before-change-functions 'permanent-local t)
    ))
(eval-after-load 'viper
  (progn
    (put 'viper-post-command-hooks 'permanent-local t)
    (put 'viper-pre-command-hooks 'permanent-local t)
    ;;minor-mode-map-alist
    ;; viper-mode-string -- is already buffer local, globally void
    (put 'viper-mode-string 'permanent-local t)
    ))
;;viper-tut--part
(eval-after-load 'viper-init
  (progn
    (put 'viper-d-com 'permanent-local t)
    (put 'viper-last-insertion 'permanent-local t)
    (put 'viper-command-ring 'permanent-local t)
    (put 'viper-vi-intercept-minor-mode 'permanent-local t)
    (put 'viper-vi-basic-minor-mode 'permanent-local t)
    (put 'viper-vi-local-user-minor-mode 'permanent-local t)
    (put 'viper-vi-global-user-minor-mode 'permanent-local t)
    (put 'viper-vi-state-modifier-minor-mode 'permanent-local t)
    (put 'viper-vi-diehard-minor-mode 'permanent-local t)
    (put 'viper-vi-kbd-minor-mode 'permanent-local t)
    (put 'viper-insert-intercept-minor-mode 'permanent-local t)
    (put 'viper-insert-basic-minor-mode 'permanent-local t)
    (put 'viper-insert-local-user-minor-mode 'permanent-local t)
    (put 'viper-insert-global-user-minor-mode 'permanent-local t)
    (put 'viper-insert-state-modifier-minor-mode 'permanent-local t)
    (put 'viper-insert-diehard-minor-mode 'permanent-local t)
    (put 'viper-insert-kbd-minor-mode 'permanent-local t)
    (put 'viper-replace-minor-mode 'permanent-local t)
    (put 'viper-emacs-intercept-minor-mode 'permanent-local t)
    (put 'viper-emacs-local-user-minor-mode 'permanent-local t)
    (put 'viper-emacs-global-user-minor-mode 'permanent-local t)
    (put 'viper-emacs-kbd-minor-mode 'permanent-local t)
    (put 'viper-emacs-state-modifier-minor-mode 'permanent-local t)
    (put 'viper-vi-minibuffer-minor-mode 'permanent-local t)
    (put 'viper-insert-minibuffer-minor-mode 'permanent-local t)
    (put 'viper-automatic-iso-accents 'permanent-local t)
    (put 'viper-special-input-method 'permanent-local t)
    (put 'viper-intermediate-command 'permanent-local t)
    ;; already local: viper-undo-needs-adjustment
    (put 'viper-began-as-replace 'permanent-local t)
    ;; already local: viper-replace-overlay
    ;; already local: viper-last-posn-in-replace-region
    ;; already local: viper-last-posn-while-in-insert-state
    ;; already local: viper-sitting-in-replace
    (put 'viper-replace-chars-to-delete 'permanent-local t)
    (put 'viper-replace-region-chars-deleted 'permanent-local t)
    (put 'viper-current-state 'permanent-local t)
    (put 'viper-cted 'permanent-local t)
    (put 'viper-current-indent 'permanent-local t)
    (put 'viper-preserve-indent 'permanent-local t)
    (put 'viper-auto-indent 'permanent-local t)
    (put 'viper-electric-mode 'permanent-local t)
    ;; already local: viper-insert-point
    ;; already local: viper-pre-command-point
    (put 'viper-com-point 'permanent-local t)
    (put 'viper-ex-style-motion 'permanent-local t)
    (put 'viper-ex-style-editing 'permanent-local t)
    (put 'viper-ESC-moves-cursor-back 'permanent-local t)
    (put 'viper-delete-backwards-in-replace 'permanent-local t)
    ;; already local: viper-related-files-and-buffers-ring
    (put 'viper-local-search-start-marker 'permanent-local t)
    (put 'viper-search-overlay 'permanent-local t)
    (put 'viper-last-jump 'permanent-local t)
    (put 'viper-last-jump-ignore 'permanent-local t)
    (put 'viper-minibuffer-current-face 'permanent-local t)
    ;; already local: viper-minibuffer-overlay
    (put 'viper-command-ring 'permanent-local t)
    (put 'viper-last-insertion 'permanent-local t)
    ))
(eval-after-load 'viper-keym
  (progn
    ;; already local: viper-vi-local-user-map
    ;; already local: viper-insert-local-user-map
    ;; already local: viper-emacs-local-user-map
    (put 'viper--key-maps 'permanent-local t)
    (put 'viper--intercept-key-maps 'permanent-local t)
    ;; already local: viper-need-new-vi-local-map
    ;; already local: viper-need-new-insert-local-map
    ;; already local: viper-need-new-emacs-local-map
    ))
(eval-after-load 'viper-mous
  (progn
    (put 'viper-mouse-click-search-noerror 'permanent-local t)
    (put 'viper-mouse-click-search-limit 'permanent-local t)
    ))
(eval-after-load 'viper-util
  (progn
    (put 'viper-syntax-preference 'permanent-local t)
    (put 'viper-non-word-characters 'permanent-local t)
    (put 'viper-ALPHA-char-class 'permanent-local t)
    ))

(eval-after-load 'cua-base
  (progn
    (put 'cua-inhibit-cua-keys 'permanent-local t)
    (put 'cua--explicit-region-start 'permanent-local t)
    (put 'cua--status-string 'permanent-local t)
    ))
;; This is for the defvar in ido.el:
(eval-after-load 'ido
  (progn
    (put 'cua-inhibit-cua-keys 'permanent-local t)
    ))
(eval-after-load 'cua-rect
  (progn
    (put 'cua--rectangle 'permanent-local t)
    (put 'cua--rectangle-overlays 'permanent-local t)
    ))
(eval-after-load 'edt
  (progn
    (put 'edt-select-mode 'permanent-local t)
    ))
(eval-after-load 'tpu-edt
  (progn
    (put 'tpu-newline-and-indent-p 'permanent-local t)
    (put 'tpu-newline-and-indent-string 'permanent-local t)
    (put 'tpu-saved-delete-func 'permanent-local t)
    (put 'tpu-buffer-local-map 'permanent-local t)
    (put 'tpu-mark-flag 'permanent-local t)
    ))
(eval-after-load 'vi
  (progn
    (put 'vi-add-to-mode-line 'permanent-local t)
    ;;Warning (mumamo-per-buffer-local-vars): Not a local variable: vi-scroll-amount
    ;;Warning (mumamo-per-buffer-local-vars): Not a local variable: vi-shift-width
    ;;Warning (mumamo-per-buffer-local-vars): Not a local variable: vi-ins-point
    ;;Warning (mumamo-per-buffer-local-vars): Not a local variable: vi-ins-length
    ;;Warning (mumamo-per-buffer-local-vars): Not a local variable: vi-ins-repetition
    ;;Warning (mumamo-per-buffer-local-vars): Not a local variable: vi-ins-overwrt-p
    ;;Warning (mumamo-per-buffer-local-vars): Not a local variable: vi-ins-prefix-code
    ;;Warning (mumamo-per-buffer-local-vars): Not a local variable: vi-last-change-command
    ;;Warning (mumamo-per-buffer-local-vars): Not a local variable: vi-last-shell-command
    ;;Warning (mumamo-per-buffer-local-vars): Not a local variable: vi-last-find-char
    ;;Warning (mumamo-per-buffer-local-vars): Not a local variable: vi-mark-alist
    ;;Warning (mumamo-per-buffer-local-vars): Not a local variable: vi-insert-state
    ;;Warning (mumamo-per-buffer-local-vars): Not a local variable: vi-mode-old-local-map
    ;;Warning (mumamo-per-buffer-local-vars): Not a local variable: vi-mode-old-mode-name
    ;;Warning (mumamo-per-buffer-local-vars): Not a local variable: vi-mode-old-major-mode
    ;;Warning (mumamo-per-buffer-local-vars): Not a local variable: vi-mode-old-case-fold
    ;;
    ))
(eval-after-load 'vi
  (progn
    (put 'vip-emacs-local-map 'permanent-local t)
    (put 'vip-insert-local-map 'permanent-local t)
    (put 'vip-insert-point 'permanent-local t)
    (put 'vip-com-point 'permanent-local t)
    (put 'vip-current-mode 'permanent-local t)
    (put 'vip-emacs-mode-line-buffer-identification 'permanent-local t)
    (put 'vip-current-major-mode 'permanent-local t)
    ))

(eval-after-load 'hi-lock
  (progn
    (put 'hi-lock-mode 'permanent-local t)
    ))

;;
;; Minor modes that are not major mode specific
;;

(put 'visual-line-mode 'permanent-local t)

(eval-after-load 'flymake
  (progn
    ;; hook functions:
    (put 'flymake-after-change-function 'permanent-local-hook t)
    (put 'flymake-after-save-hook 'permanent-local-hook t)
    (put 'flymake-kill-buffer-hook 'permanent-local-hook t)
    ;; hooks:
;;;     (put 'after-change-functions 'permanent-local 'permanent-local-hook)
;;;     (put 'after-save-hook 'permanent-local 'permanent-local-hook)
;;;     (put 'kill-buffer-hook 'permanent-local 'permanent-local-hook)
    ;; vars:
    (put 'flymake-mode 'permanent-local t)
    (put 'flymake-is-running 'permanent-local t)
    (put 'flymake-timer 'permanent-local t)
    (put 'flymake-last-change-time 'permanent-local t)
    (put 'flymake-check-start-time 'permanent-local t)
    (put 'flymake-check-was-interrupted 'permanent-local t)
    (put 'flymake-err-info 'permanent-local t)
    (put 'flymake-new-err-info 'permanent-local t)
    (put 'flymake-output-residual 'permanent-local t)
    (put 'flymake-mode-line 'permanent-local t)
    (put 'flymake-mode-line-e-w 'permanent-local t)
    (put 'flymake-mode-line-status 'permanent-local t)
    (put 'flymake-temp-source-file-name 'permanent-local t)
    (put 'flymake-master-file-name 'permanent-local t)
    (put 'flymake-temp-master-file-name 'permanent-local t)
    (put 'flymake-base-dir 'permanent-local t)))

;; (eval-after-load 'imenu
;;   (progn
;;     ;; Fix-me: imenu is only useful for main major mode.  The menu
;;     ;; disappears in sub chunks because it is tighed to
;;     ;; local-map.  Don't know what to do about that.  I do not
;;     ;; understand the reason for binding it to local-map, but I
;;     ;; suspect the intent is to have different menu items for
;;     ;; different modes.  Could not that be achieved by deleting the
;;     ;; menu and creating it again when changing major mode? (That must
;;     ;; be implemented in imenu.el of course.)
;;     ;;
;;     ;; hook functions:
;; ;;;     (put 'imenu-update-menubar 'permanent-local-hook t)
;;     ;; hooks:
;;     (put 'menu-bar-update-hook 'permanent-local 'permanent-local-hook)
;;     ;; vars:
;;     (put 'imenu-generic-expression 'permanent-local t)
;;     (put 'imenu-create-index-function 'permanent-local t)
;;     (put 'imenu-prev-index-position-function 'permanent-local t)
;;     (put 'imenu-extract-index-name-function 'permanent-local t)
;;     (put 'imenu-name-lookup-function 'permanent-local t)
;;     (put 'imenu-default-goto-function 'permanent-local t)
;;     (put 'imenu--index-alist 'permanent-local t)
;;     (put 'imenu--last-menubar-index-alist 'permanent-local t)
;;     (put 'imenu-syntax-alist 'permanent-local t)
;;     (put 'imenu-case-fold-search 'permanent-local t)
;;     (put 'imenu-menubar-modified-tick 'permanent-local t)
;;     ))

(eval-after-load 'longlines
  (progn
    ;; Fix-me: take care of longlines-mode-off
    (put 'longlines-mode 'permanent-local t)
    (put 'longlines-wrap-beg 'permanent-local t)
    (put 'longlines-wrap-end 'permanent-local t)
    (put 'longlines-wrap-point 'permanent-local t)
    (put 'longlines-showing 'permanent-local t)
    (put 'longlines-decoded 'permanent-local t)
    ;;
    (put 'longlines-after-change-function 'permanent-local-hook t)
    (put 'longlines-after-revert-hook 'permanent-local-hook t)
    (put 'longlines-before-revert-hook 'permanent-local-hook t)
    (put 'longlines-decode-buffer 'permanent-local-hook t)
    (put 'longlines-decode-region 'permanent-local-hook t)
    (put 'longlines-mode-off 'permanent-local-hook t)
    (put 'longlines-post-command-function 'permanent-local-hook t)
    (put 'longlines-window-change-function 'permanent-local-hook t)
    ;;(put 'mail-indent-citation 'permanent-local-hook t)
    ))


;; Fix-me: Rails, many problematic things:

;;; Fix-me: No idea about these, where are they used?? Add them to
;;; mumamo-per-buffer-local-vars?:
;; predictive-main-dict
;; predictive-prog-mode-main-dict
;; predictive-use-auto-learn-cache
;; predictive-dict-autosave-on-kill-buffer
(eval-after-load 'inf-ruby
  (progn
    (put 'inferior-ruby-first-prompt-pattern 'permanent-local t)
    (put 'inferior-ruby-prompt-pattern 'permanent-local t)
    ))

;;; These are for the output buffer (no problems):
;; font-lock-keywords-only
;; font-lock-defaults -- always buffer local
;; scroll-margin
;; scroll-preserve-screen-position

(eval-after-load 'rails-script
  (progn
    (put 'rails-script:run-after-stop-hook 'permanent-local t)
    (put 'rails-script:show-buffer-hook 'permanent-local t)
    (put 'rails-script:output-mode-ret-value 'permanent-local t)
    ))

;;; No problems I believe (it is in output buffer):
;; compilation-error-regexp-alist-alist
;; compilation-error-regexp-alist

;;; Fix-me: This is in the minor mode, what to do? Looks like it
;;; should have 'permanent-local t - in this case.  I have added it to
;;; mumamo-per-buffer-local-vars for now.
;; tags-file-name

(eval-after-load 'rails
  (progn
    (put 'rails-primary-switch-func 'permanent-local t)
    (put 'rails-secondary-switch-func 'permanent-local t)
    ))

;; (defun test-js-perm ()
;;   (put 'js--quick-match-re 'permanent-local t)
;;   (put 'js--quick-match-re-func 'permanent-local t)
;;   (put 'js--cache-end 'permanent-local t)
;;   (put 'js--last-parse-pos 'permanent-local t)
;;   (put 'js--state-at-last-parse-pos 'permanent-local t)
;;   (put 'js--tmp-location 'permanent-local t))
;; (test-js-perm)

(defvar mumamo-per-buffer-local-vars
  '(
    buffer-file-name
    left-margin-width
    right-margin-width
    ;; Fix-me: This is to prevent font-lock-mode turning off/on, but
    ;; is it necessary?
    ;;font-lock-mode-major-mode
    tags-file-name
    nxhtml-menu-mode
    ;; Fix-me: adding rng timers here stops Emacs from looping after
    ;; indenting in ind-0-error.php, but I have no clue why. Hm. This
    ;; problem is gone, but I forgot why.
    rng-c-current-token ;;rng-cmpct.el:132:(make-variable-buffer-local 'rng-c-current-token)
    rng-c-escape-positions ;;rng-cmpct.el:341:(make-variable-buffer-local 'rng-c-escape-positions)
    rng-c-file-name ;;rng-cmpct.el:344:(make-variable-buffer-local 'rng-c-file-name)
    rng-current-schema-file-name ;;rng-loc.el:37:(make-variable-buffer-local 'rng-current-schema-file-name)
    rng-current-schema ;;rng-pttrn.el:71:(make-variable-buffer-local 'rng-current-schema)
    ;;rng-validate-timer is permanent-local t
    ;;rng-validate-timer ;;rng-valid.el:141:(make-variable-buffer-local 'rng-validate-timer)
    ;;rng-validate-quick-timer is permanent-local t
    ;;rng-validate-quick-timer ;;rng-valid.el:146:(make-variable-buffer-local 'rng-validate-quick-timer)
    rng-error-count ;;rng-valid.el:153:(make-variable-buffer-local 'rng-error-count)
    rng-message-overlay ;;rng-valid.el:158:(make-variable-buffer-local 'rng-message-overlay)
    rng-message-overlay-inhibit-point ;;rng-valid.el:165:(make-variable-buffer-local 'rng-message-overlay-inhibit-point)
    rng-message-overlay-current ;;rng-valid.el:169:(make-variable-buffer-local 'rng-message-overlay-current)
    rng-validate-up-to-date-end ;;rng-valid.el:188:(make-variable-buffer-local 'rng-validate-up-to-date-end)
    rng-conditional-up-to-date-start ;;rng-valid.el:199:(make-variable-buffer-local 'rng-conditional-up-to-date-start)
    rng-conditional-up-to-date-end ;;rng-valid.el:205:(make-variable-buffer-local 'rng-conditional-up-to-date-end)
    rng-validate-mode ;;rng-valid.el:212:(make-variable-buffer-local 'rng-validate-mode)
    rng-dtd ;;rng-valid.el:215:(make-variable-buffer-local 'rng-dtd)

    nxml-syntax-highlight-flag ;; For pre-Emacs nxml
    ;;nxml-ns-state - not buffer local currently
    nxml-prolog-regions ;;snxml-mode.el:362:(make-variable-buffer-local 'nxml-prolog-regions)
    nxml-last-fontify-end ;;dnxml-mode.el:367:(make-variable-buffer-local 'nxml-last-fontify-end)
    nxml-degraded ;;dnxml-mode.el:373:(make-variable-buffer-local 'nxml-degraded)
    nxml-char-ref-extra-display ;;ynxml-mode.el:397:(make-variable-buffer-local 'nxml-char-ref-extra-display)
    nxml-prolog-end ;;dnxml-rap.el:92:(make-variable-buffer-local 'nxml-prolog-end)
    nxml-scan-end ;;dnxml-rap.el:107:(make-variable-buffer-local 'nxml-scan-end)

    ;;buffer-invisibility-spec
    ;;header-line-format

    ;; Fix-me: These must be handled with 'permanent-local since they may be changed:
    line-move-visual ;;simple.el:4537:    (kill-local-variable 'line-move-visual)
    word-wrap ;;simple.el:4538:    (kill-local-variable 'word-wrap)
    truncate-lines ;;simple.el:4539:    (kill-local-variable 'truncate-lines)
    truncate-partial-width-windows ;;simple.el:4540:    (kill-local-variable 'truncate-partial-width-windows)
    fringe-indicator-alist ;;simple.el:4541:    (kill-local-variable 'fringe-indicator-alist)
    visual-line--saved-state ;;simple.el:4544:    (kill-local-variable 'visual-line--saved-state)))
    vis-mode-saved-buffer-invisibility-spec ;;simple.el:6237:    (kill-local-variable 'vis-mode-saved-buffer-invisibility-spec))

    )
  "Per buffer local variables.
See also `mumamo-per-main-major-local-vars'.")

;; Fix-me: use this, but how exactly? I think the var values must be
;; picked up at every change from main major mode. And restored after
;; changing to the new major mode - but maybe a bit differently if
;; this is the main major mode.
(defvar mumamo-per-main-major-local-vars
  '(
    buffer-invisibility-spec
    header-line-format
    )
  "Per main major local variables.
Like `mumamo-per-buffer-local-vars', but this is fetched from the
main major mode.")

;; (when nil
;;   (make-variable-buffer-local 'mumamo-survive-minor-modes)
;;   (put 'mumamo-survive-minor-modes 'permanent-local t)
;;   (defvar mumamo-survive-minor-modes nil
;;     "Hold local minor mode variables specific major modes.
;;   Those values are saved when leaving a chunk with a certain
;;   major mode and restored when entering a chunk with the same
;;   major mode again.

;;   The value of this variable is an associative list where the key
;;   is a list with

;;     \(MAJOR-MODE MINOR-MODE)

;;   and the value is a stored value for the minor mode.")
;;   )

(defun mumamo-make-variable-buffer-permanent (var)
  "Make buffer local value of VAR survive when moving point to a new chunk.
When point is moved between chunks in a multi major mode the
major mode will be changed.  This will by default kill all local
variables unless they have a non-nil `permanent-local' property
\(see info node `(elisp)Creating Buffer-Local').

If you do not want to put a `permanent-local' property on a
variable you can instead use this function to make variable VAR
survive chunk switches in all mumamo multi major mode buffers."
  ;; If you want it to survive chunk switches only in the current
  ;; buffer then use `mumamo-make-local-permanent' instead."
  (pushnew var (default-value 'mumamo-per-buffer-local-vars)))

;; ;; Fix-me: use local value
;; ;; Fix-me: delelete local value when exiting mumamo
;; (defun mumamo-make-local-permanent (var)
;;   "Make buffer local value of VAR survive when moving point to a new chunk.
;; This is for the current buffer only.
;; In most cases you almost certainly want to use
;; `mumamo-make-variable-buffer-permanent' instead."
;;   (pushnew var mumamo-per-buffer-local-vars))

(defvar mumamo-per-buffer-local-vars-done-by-me nil
  "Variables set by mumamo already.
Used to avoid unnecessary warnings if setting major mode fails.")

;; (mumamo-hook-p 'viper-pre-command-hooks)
;; (mumamo-hook-p 'viper-before-change-functions)
;; (mumamo-hook-p 'c-special-indent-hook)
(defun mumamo-hook-p (sym)
  "Try to detect if SYM is a hook variable.
Just check the name."
  (let ((name (symbol-name sym)))
    (or (string= "-hook" (substring name -5))
        (string= "-hooks" (substring name -6))
        (string= "-functions" (substring name -10)))))

(defvar mumamo-major-mode nil)
(make-variable-buffer-local 'mumamo-major-mode)
(put 'mumamo-major-mode 'permanent-local t)

(defvar mumamo-change-major-mode-no-nos
  '((font-lock-change-mode t)
    (longlines-mode-off t)
    global-font-lock-mode-cmhh
    (nxml-cleanup t)
    (turn-off-hideshow t))
  "Avoid running these in `change-major-mode-hook'.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Remove things from hooks temporarily

;; Fix-me: This is a bit disorganized, could not decide which level I
;; wanted this on.

(defvar mumamo-after-change-major-mode-no-nos
  '(;;nxhtml-global-minor-mode-enable-in-buffers
    global-font-lock-mode-enable-in-buffers)
  "Avoid running these in `after-change-major-mode-hook'.")

(defvar mumamo-removed-from-hook nil)

(defun mumamo-remove-from-hook (hook remove)
  "From hook HOOK remove functions in list REMOVE.
Save HOOK and the list of functions removed to
`mumamo-removed-from-hook'."
  (let (did-remove
        removed)
    (dolist (rem remove)
      ;;(message "rem.rem=%s" rem)
      (setq did-remove nil)
      (if (listp rem)
          (when (memq (car rem) (symbol-value hook))
            (setq did-remove t)
            (remove-hook hook (car rem) t))
        (when (memq rem (symbol-value hook))
          (setq did-remove t)
          (remove-hook hook rem)))
      (when did-remove
        (setq removed (cons rem removed))))
    (setq mumamo-removed-from-hook
          (cons (cons hook removed)
                mumamo-removed-from-hook))))

(defun mumamo-addback-to-hooks ()
  "Add back what was removed by `mumamo-remove-from-hook'."
  ;;(message "mumamo-removed-from-hook=%s" mumamo-removed-from-hook)
  (dolist (rem-rec mumamo-removed-from-hook)
    (mumamo-addback-to-hook (car rem-rec) (cdr rem-rec))))

(defun mumamo-addback-to-hook (hook removed)
  "Add to hook HOOK the list of functions in REMOVED."
  ;;(message "addback: hook=%s, removed=%s" hook removed)
  (dolist (rem removed)
    ;;(message "add.rem=%s" rem)
    (if (listp rem)
        (add-hook hook (car rem) nil t)
      (add-hook hook rem))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Compare mumamo-irrelevant-buffer-local-vars
(defvar mumamo-buffer-locals-dont-set
  '(
    adaptive-fill-mode
    adaptive-fill-first-line-regexp
    adaptive-fill-regexp
    add-log-current-defun-header-regexp
    auto-composition-function
    auto-composition-mode
    auto-composition-mode-major-mode
    auto-fill-chars

    beginning-of-defun-function
    buffer-auto-save-file-format
    buffer-auto-save-file-name
    buffer-backed-up
    buffer-display-count
    buffer-display-time
    buffer-file-coding-system
    buffer-file-format
    buffer-file-name
    buffer-file-truename
    buffer-invisibility-spec
    buffer-read-only
    buffer-saved-size
    buffer-undo-list

    c++-template-syntax-table
    c-<-op-cont-regexp
    c-<>-multichar-token-regexp
    c->-op-cont-regexp
    c-after-suffixed-type-decl-key
    c-after-suffixed-type-maybe-decl-key
    c-anchored-cpp-prefix
    c-assignment-op-regexp
    c-at-vsemi-p-fn
    c-backslash-column
    c-backslash-max-column
    ;;c-basic-offset
    c-before-font-lock-function
    c-block-comment-prefix
    c-block-comment-start-regexp
    c-block-prefix-charset
    c-block-stmt-1-key
    c-block-stmt-2-key
    c-brace-list-key
    c-cast-parens
    c-class-key
    c-cleanup-list
    c-colon-type-list-re
    c-comment-only-line-offset
    c-comment-prefix-regexp
    c-comment-start-regexp
    c-current-comment-prefix
    c-decl-block-key
    c-decl-hangon-key
    c-decl-prefix-or-start-re
    c-decl-prefix-re
    c-decl-start-re
    c-doc-comment-start-regexp
    c-doc-comment-style
    c-found-types
    c-get-state-before-change-function
    c-hanging-braces-alist
    c-hanging-colons-alist
    c-hanging-semi&comma-criteria
    c-identifier-key
    c-identifier-start
    c-identifier-syntax-modifications
    c-identifier-syntax-table
    ;;c-indent-comment-alist
    ;;c-indent-comments-syntactically-p
    ;;c-indentation-style
    c-keywords-obarray
    c-keywords-regexp
    c-known-type-key
    c-label-kwds-regexp
    c-label-minimum-indentation
    c-label-prefix-re
    c-line-comment-starter
    c-literal-start-regexp
    c-multiline-string-start-char
    c-nonlabel-token-key
    c-nonsymbol-chars
    c-nonsymbol-token-regexp
    c-not-decl-init-keywords
    ;;c-offsets-alist
    c-old-BOM
    c-old-EOM
    c-opt-<>-arglist-start
    c-opt-<>-arglist-start-in-paren
    c-opt-<>-sexp-key
    c-opt-asm-stmt-key
    c-opt-bitfield-key
    c-opt-block-decls-with-vars-key
    c-opt-block-stmt-key
    c-opt-cpp-macro-define-id
    c-opt-cpp-macro-define-start
    c-opt-cpp-prefix
    c-opt-cpp-start
    c-opt-extra-label-key
    c-opt-friend-key
    c-opt-identifier-concat-key
    c-opt-inexpr-brace-list-key
    c-opt-method-key
    c-opt-op-identifier-prefix
    c-opt-postfix-decl-spec-key
    c-opt-type-component-key
    c-opt-type-concat-key
    c-opt-type-modifier-key
    c-opt-type-suffix-key
    c-other-decl-block-key
    c-other-decl-block-key-in-symbols-alist
    c-overloadable-operators-regexp
    c-paragraph-separate
    c-paragraph-start
    c-paren-stmt-key
    c-prefix-spec-kwds-re
    c-primary-expr-regexp
    c-primitive-type-key
    c-recognize-<>-arglists
    c-recognize-colon-labels
    c-recognize-knr-p
    c-recognize-paren-inexpr-blocks
    c-recognize-paren-inits
    c-recognize-typeless-decls
    c-regular-keywords-regexp
    c-simple-stmt-key
    c-special-brace-lists
    c-special-indent-hook
    c-specifier-key
    c-stmt-delim-chars
    c-stmt-delim-chars-with-comma
    c-string-escaped-newlines
    c-symbol-key
    c-symbol-start
    c-syntactic-eol
    c-syntactic-ws-end
    c-syntactic-ws-start
    c-type-decl-end-used
    c-type-decl-prefix-key
    c-type-decl-suffix-key
    c-type-prefix-key
    c-vsemi-status-unknown-p-fn

    case-fold-search
    comment-end
    comment-end-skip
    comment-indent-function
    comment-line-break-function
    comment-multi-line
    comment-start
    comment-start-skip
    cursor-type

    default-directory
    defun-prompt-regexp
    delay-mode-hooks

    enable-multibyte-characters
    end-of-defun-function

    fill-paragraph-function
    font-lock-beginning-of-syntax-function
    font-lock-defaults
    font-lock-extend-after-change-region-function
    font-lock-extend-region-functions
    font-lock-fontified
    font-lock-fontify-buffer-function
    font-lock-fontify-region-function
    font-lock-keywords
    ;;font-lock-keywords-only
    font-lock-keywords-case-fold-search
    font-lock-mode
    font-lock-mode-hook
    font-lock-mode-major-mode
    font-lock-multiline
    font-lock-set-defaults
    font-lock-syntactic-keywords
    font-lock-syntactically-fontified
    font-lock-syntax-table
    font-lock-unfontify-buffer-function
    font-lock-unfontify-region-function
    fontification-functions
    forward-sexp-function

    indent-line-function
    indent-region-function
    imenu--index-alist
    imenu--last-menubar-index-alist
    imenu-create-index-function
    imenu-menubar-modified-tick
    isearch-mode

    jit-lock-after-change-extend-region-functions
    jit-lock-context-unfontify-pos
    jit-lock-contextually
    jit-lock-functions
    jit-lock-mode

    line-move-ignore-invisible
    local-abbrev-table

    major-mode
    mark-active
    ;;mark-ring
    mode-line-process
    mode-name

    normal-auto-fill-function
    ;;nxhtml-menu-mode-major-mode

    open-paren-in-column-0-is-defun-start
    outline-level
    outline-regexp

    paragraph-ignore-fill-prefix
    paragraph-separate
    paragraph-start
    parse-sexp-ignore-comments
    parse-sexp-lookup-properties
    php-mode-pear-hook
    point-before-scroll

    ;; More symbols from visual inspection
    ;;before-change-functions
    ;;delayed-mode-hooks
    ;;imenu-case-fold-search
    ;;imenu-generic-expression
    rngalt-completing-read-tag
    rngalt-completing-read-attribute-name
    rngalt-completing-read-attribute-value
    rngalt-complete-first-try
    rngalt-complete-last-try
    rngalt-complete-tag-hooks

    syntax-begin-function
    )
  "Buffer local variables that is not saved/set per chunk.
This is supposed to contain mostly buffer local variables
specific to major modes and that are not meant to be customized
by the user.
")

(when (< emacs-major-version 23)
  (defadvice c-after-change (around
                             mumamo-ad-c-after-change
                             activate
                             compile
                             )
    ;;(msgtrc "c-after-change: major-mode=%s c-nonsymbol-token-regexp=%s" major-mode c-nonsymbol-token-regexp)
    (when (or (not mumamo-multi-major-mode)
              (derived-mode-p 'c-mode))
      ad-do-it))
  )

(defun mumamo-save-per-major-local-vars (major)
  "Save some per major local variables for major mode MAJOR.
This should be called before switching to a new chunks major
mode."
  ;;(message "mumamo-save-per-major-local-vars %s %s" major (current-buffer))
  (let ((locals (buffer-local-variables)))
    (setq locals (mapcar (lambda (local)
                           (unless
                               (or (memq (car local) mumamo-buffer-locals-dont-set)
                                   (memq (car local) mumamo-per-buffer-local-vars)
                                   (memq (car local) mumamo-per-main-major-local-vars)
                                   (get (car local) 'permanent-local))
                             local))
                         locals))
    (setq locals (delq nil locals))
    (setq locals (sort locals (lambda (sym-a sym-b)
                                (string< (symbol-name (car sym-a))
                                         (symbol-name (car sym-b))))))
    (setq mumamo-buffer-locals-per-major
          (assq-delete-all major mumamo-buffer-locals-per-major))
    (setq mumamo-buffer-locals-per-major
          (cons (cons major-mode locals)
                mumamo-buffer-locals-per-major))))

;; (benchmark 1000 '(mumamo-save-per-major-local-vars major-mode))
;; (benchmark 1000 '(mumamo-restore-per-major-local-vars major-mode))
(defvar mumamo-restore-per-major-local-vars-in-hook-major nil)
(defun mumamo-restore-per-major-local-vars-in-hook ()
  "Restore some per major mode local variables.
Call `mumamo-restore-per-major-local-vars'.
Use `mumamo-restore-per-major-local-vars-in-hook-major' as the
major mode.

This should be called in the major mode setup hook."
  (mumamo-restore-per-major-local-vars
   mumamo-restore-per-major-local-vars-in-hook-major)
  (setq mumamo-restore-per-major-local-vars-in-hook-major nil))
(put 'mumamo-restore-per-major-local-vars-in-hook 'permanent-local-hook t)

(defun mumamo-restore-per-major-local-vars (major)
  "Restore some per major local variables for major mode MAJOR.
This should be called after switching to a new chunks major
mode."
  (let ((locals (cdr (assq major mumamo-buffer-locals-per-major)))
        var
        perm)
    (dolist (rec locals)
      (setq var (car rec))
      (setq perm (get var 'permanent-local))
      (unless (or perm
                  (memq var mumamo-buffer-locals-dont-set))
        (set (make-local-variable var) (cdr rec))))))

;; (defun mumamo-testing-new ()
;;   (let ((locals (buffer-local-variables))
;;         var
;;         perm
;;         )
;;     (dolist (rec locals)
;;       (setq var (car rec))
;;       (setq perm (get var 'permanent-local))
;;       (unless (or perm
;;                   (memq var mumamo-buffer-locals-dont-set))
;;         (setq var (cdr rec))))
;;     ))
;; ;;(benchmark 1000 '(mumamo-testing-new))

(defun mumamo-get-hook-value (hook remove)
  "Return hook HOOK value with entries in REMOVE removed.
Remove also t. The value returned is a list of both local and
default values."
  (let ((value (append (symbol-value hook) (default-value hook) nil)))
    (dolist (rem remove)
      (setq value (delq rem value)))
    (delq t value)))

;; FIX-ME: Clean up the different ways of surviving variables during
;; change of major mode.
(defvar mumamo-set-major-keymap-checked nil)
(make-variable-buffer-local 'mumamo-set-major-keymap-checked)

(defvar mumamo-org-startup-done nil)
(make-variable-buffer-local 'mumamo-org-startup-done)
(put 'mumamo-org-startup-done 'permanent-local t)


(defun mumamo-font-lock-fontify-chunk ()
  "Like `font-lock-default-fontify-buffer' but for a chunk.
Buffer must be narrowed to inner part of chunk when this function
is called."
  (let ((verbose (if (numberp font-lock-verbose)
                     (and (> font-lock-verbose 0)
                          (> (- (point-max) (point-min)) font-lock-verbose))
		   font-lock-verbose))
        font-lock-extend-region-functions ;; accept narrowing
        (font-lock-unfontify-region-function 'ignore))
    ;;(setq verbose t)
    (with-temp-message
	(when verbose
	  (format "Fontifying %s part %s-%s (%s)..." (buffer-name) (point-min) (point-max) font-lock-verbose))
      (condition-case err
          (save-excursion
            (save-match-data
              (font-lock-fontify-region (point-min) (point-max) verbose)
              (font-lock-after-fontify-buffer)
              (setq font-lock-fontified t)))
        (msgtrc "font-lock-fontify-chunk: %s" (error-message-string err))
        ;; We don't restore the old fontification, so it's best to unfontify.
        (quit (mumamo-font-lock-unfontify-chunk))))))


(defun mumamo-font-lock-unfontify-chunk ()
  "Like `font-lock-default-unfontify-buffer' for .
Buffer must be narrowed to chunk when this function is called."
  ;; Make sure we unfontify etc. in the whole buffer.
  (save-restriction
    ;;(widen)
    (font-lock-unfontify-region (point-min) (point-max))
    (font-lock-after-unfontify-buffer)
    (setq font-lock-fontified nil)))

(defun mumamo-set-major (major chunk)
  "Set major mode to MAJOR for mumamo."
  (mumamo-msgfntfy "mumamo-set-major %s, %s" major (current-buffer))
  (mumamo-cancel-idle-set-major-mode)
  (remove-hook 'pre-command-hook 'mumamo-set-major-pre-command t)
  ;;(mumamo-backtrace "mumamo-set-major")
  (remove-hook 'text-mode-hook 'viper-mode) ;; Fix-me: maybe add it back...
  (let ((start-time (get-internal-run-time))
        end-time
        used-time
        ;; Viper
        viper-vi-state-mode-list
        viper-emacs-state-mode-list
        viper-insert-state-mode-list
        ;; Org-Mode
        (org-inhibit-startup mumamo-org-startup-done)
        ;; Tell `mumamo-change-major-function':
        (mumamo-set-major-running major)
        ;; Fix-me: Take care of the new values added to these hooks!
        ;; That looks difficult. We may after this have changes to
        ;; both buffer local value and global value. The global
        ;; changes are in this variable, but the buffer local values
        ;; have been set once again.
        (change-major-mode-hook (mumamo-get-hook-value
                                 'change-major-mode-hook
                                 mumamo-change-major-mode-no-nos))
        (after-change-major-mode-hook (mumamo-get-hook-value
                                       'after-change-major-mode-hook
                                       mumamo-after-change-major-mode-no-nos))
        ;; Some major modes deactivates the mark, we do not want that:
        deactivate-mark
        ;; Font lock
        (font-lock-mode font-lock-mode)
        ;; We have to save and reset the cursor type, at least when
        ;; Viper is used
        (old-cursor-type cursor-type)
        ;; Protect last-command: fix-me: probably remove
        (last-command last-command)
        ;; Fix-me: remove this
        (old-rng-schema-file (when (boundp 'rng-current-schema-file-name) rng-current-schema-file-name))
        ;; Local vars, per buffer and per major mode
        per-buffer-local-vars-state
        per-main-major-local-vars-state
        )
    ;; We are not changing mode from font-lock's point of view, so do
    ;; not tell font-lock (let binding these hooks is probably not a
    ;; good choice since they may contain other stuff too):
    (setq mumamo-removed-from-hook nil)
    (mumamo-remove-from-hook 'change-major-mode-hook mumamo-change-major-mode-no-nos)

    ;;;;;;;;;;;;;;;;
    ;; Save per buffer local variables
    (dolist (sym (reverse mumamo-per-buffer-local-vars))
      (when (boundp sym)
        (when (and (get sym 'permanent-local)
                   (not (memq sym mumamo-per-buffer-local-vars-done-by-me))
                   (not (mumamo-hook-p sym)))
          (delq sym mumamo-per-buffer-local-vars)
          (lwarn 'mumamo-per-buffer-local-vars :warning
                 "Already 'permanent-local t: %s" sym))))
    (dolist (var mumamo-per-buffer-local-vars)
      (if (local-variable-p var)
          (push (cons var (symbol-value var))
                per-buffer-local-vars-state)))

    ;;;;;;;;;;;;;;;;
    ;; Save per main major local variables
    (when (mumamo-fun-eq major-mode (mumamo-main-major-mode))
      (dolist (var mumamo-per-main-major-local-vars)
        (if (local-variable-p var)
            (push (cons var (symbol-value var))
                  per-main-major-local-vars-state))))

    ;; For all hooks that probably can have buffer local values, go
    ;; through the buffer local values and look for a permanent-local
    ;; property on each function.  Remove those functions that does not
    ;; have it.  Then make the buffer local value of the hook survive
    ;; by putting a permanent-local property on it.
    (unless (> emacs-major-version 22)
      (dolist (hk mumamo-survive-hooks)
        (put hk 'permanent-local t)
        (when (local-variable-p hk)
          (let ((hkv (copy-sequence (symbol-value hk))))
            (dolist (v hkv)
              (unless (or (eq v t)
                          (get v 'permanent-local-hook))
                (remove-hook hk v t)
                ))))))

    (run-hooks 'mumamo-change-major-mode-hook)

    (setq mumamo-major-mode major)

    ;;;;;;;;;;;;;;;;
    ;; Save per major mode local variables before switching major
    (mumamo-save-per-major-local-vars major-mode)
    ;; Prepare to restore per major mode local variables after
    ;; switching back to major-mode, but do it in the greatest
    ;; ancestor's mode hook (see `run-mode-hooks'):
    (let (ancestor-hook-sym
          parent-hook-sym
          (parent major))
      ;; We want the greatest ancestor's mode hook:
      (setq parent-hook-sym (intern-soft (concat (symbol-name parent) "-hook")))
      (when parent-hook-sym (setq ancestor-hook-sym parent-hook-sym))
      (while (get parent 'derived-mode-parent)
        (setq parent (get parent 'derived-mode-parent))
        (setq parent-hook-sym (intern-soft (concat (symbol-name parent) "-hook")))
        (when parent-hook-sym (setq ancestor-hook-sym parent-hook-sym)))
      (when ancestor-hook-sym
        ;; Put first in local hook to run it first:
        (setq mumamo-restore-per-major-local-vars-in-hook-major major)
        (add-hook ancestor-hook-sym
                  'mumamo-restore-per-major-local-vars-in-hook
                  nil t))

      ;;(msgtrc "set-major A: buffer-invisibility-spec=%S" buffer-invisibility-spec)
      ;;(msgtrc "set-major A: word-wrap=%S, cb=%s" word-wrap (current-buffer))
      ;;(mumamo-backtrace "set-major")
      (let ((here (point)))
        (unwind-protect
            (save-restriction
              (let* ((minmax (mumamo-chunk-syntax-min-max chunk t))
                     (min (car minmax))
                     (max (cdr minmax))
                     (here (point))
                     ;; Fix-me: For some reason let binding did not help. Is this a bug or?
                     ;;
                     ;;(font-lock-fontify-buffer-function 'mumamo-font-lock-fontify-chunk)
                     (old-bf (buffer-local-value 'font-lock-fontify-buffer-function (current-buffer)))
                     (inhibit-redisplay t) ;; Fix-me: said to be for internal purposes only
                     )
                (narrow-to-region min max)
                (set (make-local-variable 'font-lock-fontify-buffer-function) 'mumamo-font-lock-fontify-chunk)
                ;;(message "funcall major=%s, %s" major font-lock-fontify-buffer-function)
                ;;(message "before funcall: function=%s" font-lock-fontify-buffer-function)
                (put 'font-lock-fontify-buffer-function 'permanent-local t)
                (funcall major) ;; <-----------------------------------------------
                (put 'font-lock-fontify-buffer-function 'permanent-local nil)
                (when old-bf
                  (set (make-local-variable 'font-lock-fontify-buffer-function) old-bf))
                ))
          (goto-char here)))
      ;;(msgtrc "set-major B: buffer-invisibility-spec=%S" buffer-invisibility-spec)
      ;;(msgtrc "set-major B: word-wrap=%S, cb=%s" word-wrap (current-buffer))

      (setq font-lock-mode-major-mode major) ;; Tell font-lock it is ok
      (set (make-local-variable 'font-lock-function) 'mumamo-font-lock-function)
      (if (not ancestor-hook-sym)
          (mumamo-restore-per-major-local-vars major)
        (remove-hook ancestor-hook-sym
                     'mumamo-restore-per-major-local-vars-in-hook
                     t)))
    ;;(msgtrc "set-major c: buffer-invisibility-spec=%S" buffer-invisibility-spec)

    (when (mumamo-fun-eq major 'org-mode) (setq mumamo-org-startup-done t))

    (setq mumamo-major-mode-indent-line-function (cons major-mode indent-line-function))
    (make-local-variable 'indent-line-function)

    (setq mode-name (concat (format-mode-line mode-name)
                            (save-match-data
                              (replace-regexp-in-string
                               "-mumamo-mode$" ""
                               (format "/%s" mumamo-multi-major-mode)))))

    (dolist (hk mumamo-survive-hooks) (put hk 'permanent-local nil))

    ;;     (when (and (featurep 'flymake)
    ;;                flymake-mode)
    ;;       (add-hook 'after-change-functions 'flymake-after-change-function nil t)
    ;;       (add-hook 'after-save-hook 'flymake-after-save-hook nil t)
    ;;       (add-hook 'kill-buffer-hook 'flymake-kill-buffer-hook nil t))

    ;;;;;;;;;;;;;;;;
    ;; Restore per buffer local variables

    ;; (dolist (sym mumamo-per-buffer-local-vars)
    ;;   (when (boundp sym)
    ;;     (put sym 'permanent-local nil)))
    ;;(msgtrc "per-buffer-local-vars-state=%S" per-buffer-local-vars-state)
    (dolist (saved per-buffer-local-vars-state)
      ;;(msgtrc "restore p buffer: %s, local=%s" (car saved) (local-variable-p (car saved)))
      (unless (local-variable-p (car saved))
        (set (make-local-variable (car saved)) (cdr saved))))

    ;;;;;;;;;;;;;;;;
    ;; Restore per main major local variables
    (unless (mumamo-fun-eq major-mode (mumamo-main-major-mode))
      (dolist (saved per-main-major-local-vars-state)
        (set (make-local-variable (car saved)) (cdr saved))))

    (mumamo-addback-to-hooks)

    (setq cursor-type old-cursor-type)
    (run-hooks 'mumamo-after-change-major-mode-hook)

    (when (derived-mode-p 'nxml-mode)
      (when (and old-rng-schema-file
                 (not (string= old-rng-schema-file rng-current-schema-file-name)))
        (let ((rng-schema-change-hook nil)) ;(list 'rng-alidate-clear)))
          (condition-case err
              (progn
                (rng-set-schema-file-1 old-rng-schema-file)
                (rng-what-schema))
            (nxml-file-parse-error
             (nxml-display-file-parse-error err)))
          (when rng-validate-mode
            ;; Fix-me: Change rng-validate variables so that this is
            ;; not necessary any more.
            (rng-validate-mode 0)
            (rng-validate-mode 1))
          )))
    ;; The nxml-parser should not die:
    (when (mumamo-derived-from-mode (mumamo-main-major-mode) 'nxml-mode)
      (add-hook 'after-change-functions 'rng-after-change-function nil t)
      (add-hook 'after-change-functions 'nxml-after-change nil t)
      ;; Added these for Emacs 22:
      (unless nxml-prolog-end (setq nxml-prolog-end 1))
      (unless nxml-scan-end (setq nxml-scan-end (copy-marker 1))))

;;;     (when (and global-font-lock-mode
;;;                font-lock-global-modes
;;;                font-lock-mode)
;;;     (when global-font-lock-mode
;;;       (add-hook 'change-major-mode-hook 'global-font-lock-mode-cmhh))
;;;     (add-hook 'change-major-mode-hook 'font-lock-change-mode nil t)

    (mumamo-set-fontification-functions)

    ;; If user has used M-x flyspell-mode then we need to correct it:
    ;; Fix-me: This is inflexible. Need flyspell to cooperate.
    (when (featurep 'flyspell)
      (setq flyspell-generic-check-word-predicate 'mumamo-flyspell-verify))

    (if mumamo-done-first-set-major
        (setq mumamo-just-changed-major t)
      (mumamo-msgfntfy "mumamo-set-major: ----- removing 'fontified")
      ;; Set up to fontify buffer
      (mumamo-save-buffer-state nil
        (remove-list-of-text-properties (point-min) (point-max) '(fontified)))
      (setq mumamo-done-first-set-major t))

    ;; Timing, on a 3ghz cpu:
    ;;
    ;;   used-time=(0 0 0), major-mode=css-mode
    ;;   used-time=(0 0 0), major-mode=ecmascript-mode
    ;;   used-time=(0 0 0), major-mode=html-mode
    ;;   used-time=(0 0 203000), major-mode=nxhtml-mode
    ;;
    ;; After some changes 2007-04-25:
    ;;
    ;;   used-time=(0 0 15000), major-mode=nxhtml-mode
    ;;
    ;; which is 15 ms.  That seems acceptable though I am not sure
    ;; everything is correct when switching to nxhtml-mode yet.  I
    ;; will have to wait for bug reports ;-)
    ;;
    ;; The delay is clearly noticeable and disturbing IMO unless you
    ;; change major mode in an idle timer.
    ;;
    ;;(setq end-time (get-internal-run-time))
    ;;(setq used-time (time-subtract end-time start-time))
    )
  (setq mumamo-set-major-keymap-checked nil)
  ;; Fix-me: Seems like setting/checking the keymap in a timer is
  ;; problematc. This is an Emacs bug.
  ;;(run-with-idle-timer 1 nil 'mumamo-set-major-check-keymap)
  ;;(force-mode-line-update) (message "force-mode-line-update called")
  )

(defun mumamo-set-major-check-keymap ()
  "Helper to work around an Emacs bug when setting local map in a timer."
  (or mumamo-set-major-keymap-checked
      (setq mumamo-set-major-keymap-checked
            (let ((map-sym (intern-soft (concat (symbol-name major-mode) "-map"))))
              (if (not map-sym)
                  t ;; Don't know what to do
                (equal (current-local-map)
                       (symbol-value map-sym)))))))

(defvar mumamo-original-fill-paragraph-function nil)
(make-variable-buffer-local 'mumamo-original-fill-paragraph-function)

(defun mumamo-setup-local-fontification-vars ()
  "Set up buffer local variables for mumamo style fontification."
  (make-local-variable 'font-lock-fontify-region-function)
  (setq font-lock-fontify-region-function 'mumamo-fontify-region)

  ;; Like font-lock-turn-on-thing-lock:
  (make-local-variable 'font-lock-fontify-buffer-function)
  (setq font-lock-fontify-buffer-function 'jit-lock-refontify)
  (setq font-lock-fontify-buffer-function 'mumamo-fontify-buffer)
  ;; Don't fontify eagerly (and don't abort if the buffer is large).
  (set (make-local-variable 'font-lock-fontified) t)

  (make-local-variable 'font-lock-unfontify-buffer-function)
  (setq font-lock-unfontify-buffer-function 'mumamo-unfontify-buffer)

  (set (make-local-variable 'indent-line-function) 'mumamo-indent-line-function)

  ;;(setq mumamo-original-fill-paragraph-function fill-paragraph-function)
  ;;(set (make-local-variable 'fill-paragraph-function) 'mumamo-fill-paragraph-function)
  ;;(set (make-local-variable 'fill-forward-paragraph-function 'forward-paragraph)

  (make-local-variable 'indent-region-function)
  (setq indent-region-function 'mumamo-indent-region-function)

  ;;(set (make-local-variable 'syntax-begin-function) 'mumamo-beginning-of-syntax)

  ;;(put 'font-lock-function 'permanent-local t)

  ;; FIX-ME: Not sure about this one, but it looks like it must be
  ;; set:
  (make-local-variable 'jit-lock-contextually)
  (setq jit-lock-contextually t)
  )

(defun mumamo-font-lock-function (mode)
  ;;(mumamo-backtrace "font-lock-function")
  (font-lock-default-function mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Turning on/off multi major modes

(defun mumamo-set-fontification-functions ()
  "Let mumamo take over fontification.
This is run after changing major mode so that jit-lock will get
the major mode specific values.  \(There are currently no such
values.)"
  ;; Give the jit machinery a starting point:
  (mumamo-jit-lock-register 'font-lock-fontify-region t)
  ;; Set the functions that font-lock should use:
  (mumamo-setup-local-fontification-vars)
  ;; Need some hook modifications to keep things together too:
  (add-hook 'change-major-mode-hook 'mumamo-change-major-function nil t)
  (add-hook 'post-command-hook 'mumamo-post-command nil t)
  (remove-hook 'change-major-mode-hook 'nxml-change-mode t)
  (remove-hook 'change-major-mode-hook 'nxhtml-change-mode t)
  )

(defun mumamo-initialize-state ()
  "Initialize some mumamo state variables."
  (setq mumamo-done-first-set-major nil)
  (setq mumamo-just-changed-major nil))

(defun mumamo-turn-on-actions (old-major-mode)
  "Do what is necessary to turn on mumamo.
Turn on minor mode function `font-lock-mode'.
Set up for mumamo style fontification.
Create a mumamo chunk at point.
Run `mumamo-turn-on-hook'.

OLD-MAJOR-MODE is used for the main major mode if the main major
mode in the chunk family is nil."
  ;;(unless font-lock-mode (font-lock-mode 1))
  (mumamo-msgfntfy "mumamo-turn-on-actions")
  (unless mumamo-current-chunk-family (error "Internal error: Chunk family is not set"))
  (if (not mumamo-current-chunk-family)
      (progn
        (lwarn '(mumamo) :warning
               "Could not turn on mumamo because chunk family was not set\n\tin buffer %s."
               (current-buffer))
        (with-current-buffer "*Warnings*"
          (insert "\tFor more information see `")
          (mumamo-insert-describe-button 'define-mumamo-multi-major-mode 'describe-function)
          (insert "'.\n")))
    ;; Load major mode:
    (setq mumamo-org-startup-done nil)
    (let ((main-major-mode (mumamo-major-mode-from-modespec (mumamo-main-major-mode))))
      (unless main-major-mode
        (setcar (cdr mumamo-current-chunk-family) old-major-mode)
        (setq main-major-mode (mumamo-main-major-mode)))
      ;;(with-temp-buffer (funcall main-major-mode))
      (setq mumamo-major-mode main-major-mode)
      (when (boundp 'nxml-syntax-highlight-flag)
        (when (mumamo-derived-from-mode main-major-mode 'nxml-mode)
          (set (make-local-variable 'nxml-syntax-highlight-flag) nil)))
      ;; Init fontification
      (mumamo-initialize-state)
      (mumamo-set-fontification-functions)
      (mumamo-save-buffer-state nil
        (remove-list-of-text-properties (point-min) (point-max)
                                        (list 'fontified)))
      ;; For validation header etc:
      (when (mumamo-derived-from-mode main-major-mode 'nxhtml-mode)
        (require 'rngalt nil t)
        (when (featurep 'rngalt)
          (setq rngalt-major-mode (mumamo-main-major-mode))
          (rngalt-update-validation-header-overlay))
        (when (featurep 'rng-valid)
          (setq rng-get-major-mode-chunk-function 'mumamo-find-chunks)
          (setq rng-valid-nxml-major-mode-chunk-function 'mumamo-valid-nxml-chunk)
          (setq rng-end-major-mode-chunk-function 'overlay-end))))
    ;;(mumamo-set-major-post-command)
    ;;(add-hook 'change-major-mode-hook 'mumamo-change-major-function nil t)
    (when (boundp 'flyspell-generic-check-word-predicate)
      (setq flyspell-generic-check-word-predicate 'mumamo-flyspell-verify))
    (run-hooks 'mumamo-turn-on-hook)
    ;;(mumamo-get-chunk-save-buffer-state (point))
    (let ((buffer-windows (get-buffer-window-list (current-buffer))))
      (if (not buffer-windows)
          (let* ((ovl (mumamo-find-chunks (point) "mumamo-turn-on-actions"))
                 (major (when ovl (mumamo-chunk-major-mode ovl))))
            (when major
              (mumamo-set-major major ovl)))
        (dolist (win (get-buffer-window-list (current-buffer) nil t))
          (let ((wp (or (window-end win)
                        (window-point win)
                        (window-start win))))
            (mumamo-get-chunk-save-buffer-state wp)
            (when (eq win (selected-window))
              (let* ((ovl (mumamo-find-chunks wp "mumamo-turn-on-actions"))
                     (major (when ovl (mumamo-chunk-major-mode ovl))))
                (when major
                  (mumamo-set-major major ovl))))))))
    ;;(msgtrc "mumamo-turn-on-action exit: font-lock-keywords-only =%s in buffer %s, def=%s" font-lock-keywords-only (current-buffer) (default-value 'font-lock-keywords-only))
    ;; This did not help for Emacs bug 3467:
    ;;(set-default 'font-lock-keywords-only nil)
    ;;(setq font-lock-keywords-only nil)
    )
  (set (make-local-variable 'font-lock-function) 'mumamo-font-lock-function)
  (mumamo-emacs-start-bug3467-timer-if-needed)
  )

;; (defun mumamo-on-font-lock-off ()
;;   "The reverse of `mumamo-turn-on-actions'."
;;   (let ((mumamo-main-major-mode (mumamo-main-major-mode)))
;;     (mumamo-turn-off-actions)
;;     ;; Turning off `font-lock-mode' also turns off `mumamo-mode'.  It is
;;     ;; quite tricky to not turn on `font-lock-mode' again in case we got
;;     ;; here because it was turned off.  We must first remove the cmhh
;;     ;; function and then also run the internal font lock turn off.
;;     (let* ((flm  font-lock-mode)
;;            (flgm global-font-lock-mode)
;;            (remove-cmhh (and (not flm) flgm)))
;;       ;; If remove-cmhh is non-nil then we got here because
;;       ;; `font-lock-mode' was beeing turned off in the buffer, but
;;       ;; `global-font-lock-mode' is still on.
;;       (when remove-cmhh
;;         (remove-hook 'change-major-mode-hook 'global-font-lock-mode-cmhh))

;;       (if mumamo-main-major-mode
;;           (funcall mumamo-main-major-mode)
;;         (fundamental-mode))

;;       (unless flm
;;         (setq font-lock-mode nil)
;;         (font-lock-mode-internal nil))
;;       (when remove-cmhh
;;         (add-hook 'change-major-mode-hook 'global-font-lock-mode-cmhh)))))

(defun mumamo-turn-off-actions ()
  "The reverse of `mumamo-turn-on-actions'."
  (mumamo-msgfntfy "mumamo-turn-off-actions")
  (when (fboundp 'nxhtml-validation-header-mode)
    (nxhtml-validation-header-mode -1))
  (when (mumamo-derived-from-mode
         (nth 1 mumamo-current-chunk-family) 'nxml-mode)
    (when (fboundp 'nxml-change-mode)
      (nxml-change-mode)))
  (when (and (boundp 'rng-validate-mode)
             rng-validate-mode)
    (rng-validate-mode 0))
  (when (featurep 'rng-valid)
    (setq rng-get-major-mode-chunk-function nil)
    (setq rng-valid-nxml-major-mode-chunk-function nil)
    (setq rng-end-major-mode-chunk-function nil)
    )
  ;; Remove nxml for Emacs 22
  (remove-hook 'after-change-functions 'rng-after-change-function t)
  (remove-hook 'after-change-functions 'nxml-after-change t)
  (when (boundp 'rngalt-major-mode)
    (setq rngalt-major-mode nil))
  (remove-hook 'change-major-mode-hook 'mumamo-change-major-function t)
  ;;(mumamo-unfontify-chunks)
  ;;(remove-hook 'after-change-functions 'mumamo-jit-lock-after-change t)
  (remove-hook 'after-change-functions 'mumamo-after-change t)
  (remove-hook 'post-command-hook 'mumamo-post-command t)
  ;;(remove-hook 'c-special-indent-hook 'mumamo-c-special-indent t)
  (mumamo-margin-info-mode -1)
  (when (fboundp 'mumamo-clear-all-regions) (mumamo-clear-all-regions))
  (save-restriction
    (widen)
    (mumamo-save-buffer-state nil
      (set-text-properties (point-min) (point-max) nil)))
  (setq mumamo-current-chunk-family nil)
  (setq mumamo-major-mode nil)
  (setq mumamo-multi-major-mode nil) ;; for minor-mode-map-alist
  (setq mumamo-multi-major-mode nil)
  (mumamo-remove-all-chunk-overlays)
  (when (fboundp 'rng-cancel-timers) (rng-cancel-timers))
  )

(defvar mumamo-turn-on-hook nil
  "Normal hook run after turning on `mumamo-mode'.")
(put 'mumamo-turn-on-hook 'permanent-local t)

(defvar mumamo-change-major-mode-hook nil
  "Normal hook run before internal change of major mode.")
(put 'mumamo-change-major-mode-hook 'permanent-local t)

(defvar mumamo-after-change-major-mode-hook nil
  "Normal hook run after internal change of major mode.")
(put 'mumamo-after-change-major-mode-hook 'permanent-local t)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Defining multi major modes

(defvar mumamo-defined-multi-major-modes nil
  "List of functions defined for turning on mumamo.
Those functions should be called instead of calling a major mode
function when you want to use multiple major modes in a buffer.
They may be added to for example `auto-mode-alist' to
automatically have the major mode support turned on when opening
a file.

Each of these functions defines how to mix certain major modes in
a buffer.

All functions defined by `define-mumamo-multi-major-mode' are
added to this list.  See this function for a general description
of how the functions work.

If you want to quickly define a new mix of major modes you can
use `mumamo-quick-static-chunk'.")

;;;###autoload
(defun mumamo-list-defined-multi-major-modes (show-doc show-chunks match)
  "List currently defined multi major modes.
If SHOW-DOC is non-nil show the doc strings added when defining
them. \(This is not the full doc string. To show the full doc
string you can click on the multi major mode in the list.)

If SHOW-CHUNKS is non-nil show the names of the chunk dividing
functions each multi major mode uses.

If MATCH then show only multi major modes whos names matches."
  (interactive (list (y-or-n-p "Include short doc string? ")
                     (y-or-n-p "Include chunk function names? ")
                     (read-string "List only multi major mode matching regexp (emtpy for all): ")))
  (with-output-to-temp-buffer (help-buffer)
    (help-setup-xref (list #'mumamo-list-defined-multi-major-modes) (interactive-p))
    (with-current-buffer (help-buffer)
      (insert "The currently defined multi major modes in your Emacs are:\n\n")
      (let ((mmms (reverse mumamo-defined-multi-major-modes))
            (here (point)))
        (setq mmms (sort mmms (lambda (a b)
                                (string< (symbol-name (cdr a))
                                         (symbol-name (cdr b))))))
        (when (string= match "") (setq match nil))
        (while mmms
          (let* ((mmm (car mmms))
                 (sym  (cdr mmm))
                 (desc (car mmm))
                 (auto (get sym 'autoload))
                 (auto-desc (when auto (nth 1 auto)))
                 (family (get sym 'mumamo-chunk-family))
                 (chunks (nth 2 family)))
            (when (or (not match)
                      (string-match-p match (symbol-name sym)))
              (insert "  `" (symbol-name sym) "'"
                      " (" desc ")\n"
                      (if (and show-doc auto-desc)
                          (concat "   " auto-desc "\n")
                        "")
                      (if show-chunks
                          (format "   Chunks:%s\n"
                                  (let ((str "")
                                        (nn 0))
                                    (mapc (lambda (c)
                                            (if (< nn 2)
                                                (setq str (concat str " "))
                                              (setq nn 0)
                                              (setq str (concat str "\n           ")))
                                            (setq nn (1+ nn))
                                            (setq str (concat str (format "%-30s" (format "`%s'" c))))
                                            )
                                          chunks)
                                    str))
                        "")
                      (if (or show-doc show-chunks) "\n\n" "")
                      ))
            (setq mmms (cdr mmms))))
        ))))

(defun mumamo-describe-chunks (chunks)
  "Return text describing CHUNKS."
  (let* ((desc
          (concat "* Main major mode: `" (symbol-name (nth 1 chunks)) "'\n"
                  "\n* Functions for dividing into submodes:\n")))
    (dolist (divider (nth 2 chunks))
      (setq desc
            (concat
             desc
             "\n`" (symbol-name divider)
             "'\n   "
             (let ((doc (if (functionp divider)
                            (documentation divider t)
                          "(Function not compiled when building doc)")))
               (if (not doc)
                   "(Not documented)"
                 (substring doc 0 (string-match "\n" doc)))))))
    (setq desc
          (concat
           desc
           "\n\n(Note that the functions for dividing into chunks returns\n"
           "a major mode specifier which may be translated into a major mode\n"
           "by `mumamo-main-major-mode'.)\n"))
    desc))

(defun mumamo-add-multi-keymap (toggle keymap)
  "Add TOGGLE and KEYMAP to `minor-mode-map-alist'.
This is used to add a keymap to multi major modes since the local
keymap is occupied by the major modes.

It is also used to add the `mumamo-map' keymap to every buffer
with a multi major mode."
  ;; Copied from add-minor-mode
  ;; Add the map to the minor-mode-map-alist.
  (when keymap
    (let ((existing (assq toggle minor-mode-map-alist))
          (after t))
      (if existing
	  (setcdr existing keymap)
	(let ((tail minor-mode-map-alist) found)
	  (while (and tail (not found))
	    (if (eq after (caar tail))
		(setq found tail)
	      (setq tail (cdr tail))))
	  (if found
	      (let ((rest (cdr found)))
		(setcdr found nil)
		(nconc found (list (cons toggle keymap)) rest))
	    (setq minor-mode-map-alist (cons (cons toggle keymap)
					     minor-mode-map-alist))))))))

(defvar mumamo-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control meta prior)] 'mumamo-backward-chunk)
    (define-key map [(control meta next)]  'mumamo-forward-chunk)
    ;; Use mumamo-indent-line-function:
    ;;(define-key map [tab] 'indent-for-tab-command)
    (define-key map [(meta ?q)] 'fill-paragraph)
    map)
  "Keymap that is active in all mumamo buffers.
It has the some priority as minor mode maps.")
;;(make-variable-buffer-local 'mumamo-map)
(put 'mumamo-map 'permanent-local t)

(mumamo-add-multi-keymap 'mumamo-multi-major-mode mumamo-map)

;;;###autoload
(defun mumamo-multi-major-modep (value)
  "Return t if VALUE is a multi major mode function."
  (and (fboundp value)
       (rassq value mumamo-defined-multi-major-modes)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Indenting, filling, moving etc

;; FIX-ME: Indentation in perl here doc indents the ending mark which
;; corrupts the perl here doc.

(defun mumamo-indent-line-function ()
  "Function to indent the current line.
This is the buffer local value of `indent-line-function' when
mumamo is used."
  (let ((here (point-marker))
        fontification-functions
        rng-nxml-auto-validate-flag
        (before-text (<= (current-column) (current-indentation))))
    (mumamo-indent-line-function-1 nil nil nil)
    ;; If the marker was in the indentation part strange things happen
    ;; if we try to go back to the marker, at least in php-mode parts.
    (if before-text
        (back-to-indentation)
      (goto-char here))))

(defun mumamo-indent-current-line-chunks (last-chunk-prev-line)
  "Return a list of chunks to consider when indenting current line.
This list consists of four chunks at these positions:
- Beginning of line - 1
- Beginning of line
- End of line
- End of line + 1"
  ;; Fix-me: must take markers into account too when a submode
  ;; includes the markers.
  (setq last-chunk-prev-line nil)
  ;;(msgtrc "indent-current-line-chunks: last-chunk-prev-line=%S" last-chunk-prev-line)
  (save-restriction
    (widen)
    (let* ((lb-pos (line-beginning-position))
           (le-pos (line-end-position))
           (pos0 (if (> lb-pos (point-min))
                     (1- lb-pos)
                   (point-min)))
           (pos1 lb-pos)
           (pos2 le-pos)
           (pos3 (if (< le-pos (point-max))
                     (+ 1 le-pos)
                   (point-max)))
           ;; Create all chunks on this line first, then grab them
           (ovl3 (mumamo-find-chunks pos3 "mumamo-indent-current-line-chunks"))
           (ovl2 (if (>= pos2 (overlay-start ovl3))
                     ovl3
                   (mumamo-get-existing-new-chunk-at pos2)))
           (ovl1 (if (>= pos1 (overlay-start ovl2))
                     ovl2
                   (mumamo-get-existing-new-chunk-at pos1)))
           (ovl0 (if (> pos0 (overlay-start ovl1))
                     ovl1
                   (mumamo-get-existing-new-chunk-at pos0 t))))
      (list ovl0 ovl1 ovl2 ovl3))))

;; Fix-me: need to back up past comments in for example <style> /* comment */
;; fix-me: clean up
(put 'mumamo-error-ind-0 'error-conditions '(error mumamo-error-ind-0))
(put 'mumamo-error-ind-0 'error-message "indentation 0 in sub chunk")



;;;;;;;;;;;;;;;;;;;;;;;;
;; Template indentation
;;; Contact Marc Bowes when I've finished this.

(defvar  mumamo-template-indent-buffer nil)
(make-variable-buffer-local 'mumamo-template-indent-buffer)
(put 'mumamo-template-indent-buffer 'permanent-local t)

(defvar  mumamo-template-indent-change-min nil)
(make-variable-buffer-local 'mumamo-template-indent-change-min)
(put 'mumamo-template-indent-hange-min 'permanent-local t)

(defun mumamo-template-indent-after-change (beg end len)
  (setq mumamo-template-indent-change-min
        (if mumamo-template-indent-change-min
            (min mumamo-template-indent-change-min beg)
          beg)))

;; (defun mumamo-get-indentor-create (indentor-chunk prev-indentor)
;;   (let ((indentor (overlay-get indentor-chunk 'mumamo-indentor))
;;         (indentor-buffer (when indentor (overlay-buffer indentor)))
;;         (chunk-str (with-current-buffer (overlay-buffer indentor-chunk)
;;                      (buffer-substring-no-properties (overlay-start indentor-chunk)
;;                                                      (overlay-end indentor-chunk))))
;;         )
;;     (unless (and indentor
;;                  (eq indentor-buffer mumamo-template-indent-buffer)
;;                  (string= chunk-str (overlay-get indentor 'indentor-chunk-string)))
;;       (when indentor
;;         (when (buffer-live-p 
;;     indentor
;;     ))
(defun mumamo-indentor-valid (indentor chunk chunk-string)
  (and indentor
       chunk
       (buffer-live-p (overlay-buffer chunk))
       (string= chunk-string (overlay-get indentor 'indentor-chunk-string))
       ))

(defun mumamo-template-indent-get-chunk-shift (indentor-chunk)
  "Return indentation shift for INDENTOR-CHUNK row and line after.
;; Fix-me: Handle changes better.

Indentation shift has two parts: shift for current line and for next line.
This function returns a cons with these two parts.
"
  (assert (overlayp indentor-chunk) t)
  (assert (buffer-live-p (overlay-buffer indentor-chunk)) t)
  (let ((indentor (overlay-get indentor-chunk 'mumamo-indentor))
        (prev-chunk (overlay-get indentor-chunk 'mumamo-prev-chunk))
        prev-indentor prev-indentor-chunk)
    (when indentor (assert (eq indentor-chunk (overlay-get indentor 'indentor-chunk)) t))
    (unless (and mumamo-template-indent-buffer
                 (buffer-live-p mumamo-template-indent-buffer))
      (setq indentor nil)
      (setq mumamo-template-indent-buffer
            (get-buffer-create (concat (buffer-name)
                                       "-template-indent-buffer")))
      (with-current-buffer mumamo-template-indent-buffer
        (setq buffer-undo-list t)
        (let ((major (car (overlay-get indentor-chunk 'mumamo-major-mode))))
          (funcall major))))
    (when indentor
      (unless (eq (overlay-buffer indentor) mumamo-template-indent-buffer)
        (setq indentor nil)))
      ;; We need the prev indentor to indent relative to.
    (while (and prev-chunk (not prev-indentor-chunk))
      (setq prev-chunk (overlay-get prev-chunk 'mumamo-prev-chunk))
      (when prev-chunk
        (when (eq (overlay-get prev-chunk 'mumamo-next-indent)
                  'mumamo-template-indentor)
          (setq prev-indentor-chunk (overlay-get prev-chunk 'mumamo-next-chunk)))))
    (when prev-indentor-chunk
      (setq prev-indentor (overlay-get prev-indentor-chunk 'mumamo-indentor)))
    (when prev-indentor
      (unless (buffer-live-p (overlay-buffer prev-indentor))
        (setq prev-indentor nil)))
    (when prev-indentor (assert (eq (overlay-buffer prev-indentor) mumamo-template-indent-buffer) t))
    (with-current-buffer mumamo-template-indent-buffer
      (save-restriction
        (widen)
        ;; Insert a blank line to be able to go to start of first
        ;; overlay -1.  Do it here in case the user erases the buffer.
        (when (= 0 (buffer-size)) (insert "\n"))
        (let ((i-str (when indentor
                       (buffer-substring-no-properties (overlay-start indentor) (overlay-end indentor))))
              (i-beg (when indentor (overlay-start indentor)))
              (c-str (with-current-buffer (overlay-buffer indentor-chunk)
                       (buffer-substring-no-properties (overlay-start indentor-chunk)
                                                       (overlay-end indentor-chunk))))
              (p-str (when prev-indentor-chunk
                       (with-current-buffer (overlay-buffer prev-indentor-chunk)
                         (buffer-substring-no-properties (overlay-start prev-indentor-chunk)
                                                         (overlay-end prev-indentor-chunk)))))
              (c-beg (overlay-start indentor-chunk))
              (p-beg (when prev-indentor-chunk (overlay-start prev-indentor-chunk))))
          ;; Check if `indentor' and `prev-indentor' are valid
          (when indentor
            ;;(unless (string= c-str (overlay-get indentor 'indentor-chunk-string))
            (unless (mumamo-indentor-valid indentor indentor-chunk c-str)
              (mumamo-remove-indentor indentor)))
          (when prev-indentor
            ;;(unless (string= p-str (overlay-get prev-indentor 'indentor-chunk-string))
            (unless (mumamo-indentor-valid prev-indentor prev-indentor-chunk p-str)
              (mumamo-remove-indentor prev-indentor)))
          (unless indentor
            (setq i-beg
                    (or i-beg
                        (when prev-indentor
                          ;; We just put `indentor' after this, but we
                          ;; must also remove old stuff.
                          (goto-char (overlay-end prev-indentor))
                          (forward-char 1)
                          (let* ((next-indentor (mumamo-indentor-at (point)))
                                 (next-indentor-chunk (when next-indentor
                                                        (overlay-get next-indentor 'indentor-chunk)))
                                 n-beg
                                 (new-i-beg (unless next-indentor-chunk (point))))
                            (while (not new-i-beg)
                              (setq n-beg (when (buffer-live-p (overlay-buffer next-indentor-chunk))
                                            (overlay-start next-indentor-chunk)))
                              (if (or (not n-beg) (< n-beg c-beg))
                                  (progn
                                    (mumamo-remove-indentor next-indentor)
                                    (goto-char (overlay-end prev-indentor))
                                    (forward-char 1)
                                    (setq next-indentor (mumamo-indentor-at (point)))
                                    (if next-indentor
                                        (setq next-indentor-chunk (overlay-get next-indentor 'indentor-chunk))
                                      (setq new-i-beg (point))))
                                (setq new-i-beg (point))))
                            new-i-beg))
                        ;; Fix-me: Find out where to insert indentor:
                        (let* ((ll 1)
                               (rr (point-max))
                               mm new-i-beg m-ovl m-ovl-old m-chunk m-beg)
                          (while (< ll rr)
                            (setq mm (+ ll (/ (- rr ll) 2)))
                            (setq m-ovl-old m-ovl)
                            (setq m-ovl (mumamo-indentor-at mm))
                            (if (or (not m-ovl) (eq m-ovl m-ovl-old))
                                (setq rr ll)
                              (setq m-chunk (overlay-get m-ovl 'indentor-chunk))
                              (setq m-beg (when (buffer-live-p (overlay-buffer m-chunk))
                                            (overlay-start m-chunk)))
                              (cond ((not m-beg)
                                     (mumamo-remove-indentor m-ovl)
                                     (setq rr (min rr (point-max))))
                                    ((> m-beg c-beg)
                                     (setq ll (1+ mm)))
                                    ((< m-beg c-beg)
                                     (setq rr (1- mm)))
                                    (t (error "Found old indentor at %s belonging to %S" mm m-chunk)))))
                          ;;(1+ (if m-ovl (overlay-end m-ovl) 0))
                          (if m-ovl (1+ (overlay-end m-ovl)) 2)
                          )))
            (goto-char i-beg)
            (setq indentor (mumamo-make-indentor indentor-chunk c-str)))
          (unless prev-indentor
            (when prev-indentor-chunk
              (goto-char (overlay-start indentor))
              (goto-char (point-at-bol))
              (setq prev-indentor (mumamo-make-indentor prev-indentor-chunk p-str))))
          (when prev-indentor (mumamo-indent-indentor prev-indentor))
          (mumamo-indent-indentor indentor)
          (let (prev-ind this-ind next-ind shift-in shift-out)
            (when prev-indentor
              (goto-char (overlay-end prev-indentor))
              (setq prev-ind (current-indentation)))
            (goto-char (overlay-start indentor))
            (setq this-ind (current-indentation))
            (goto-char (overlay-end indentor))
            (setq next-ind (current-indentation))
            (when prev-ind (setq shift-in (- this-ind prev-ind)))
            (setq shift-out (- next-ind this-ind))
            (msgtrc "template-indent-get-shunk-shift => (%s . %s)" shift-in shift-out)
            (cons shift-in shift-out)))))))


(defun mumamo-ruby-beginning-of-indent ()
  "TODO: document"
  ;; I don't understand this function.
  ;; It seems like it should move to the line where indentation should deepen,
  ;; but ruby-indent-beg-re only accounts for whitespace before class, module and def,
  ;; so this will only match other block beginners at the beginning of the line.
  (and
   (prog1
       (re-search-backward (concat "^\\(" ruby-indent-beg-re "\\)\\b") nil 'move)
     (skip-chars-forward " \t\n\r"))
    (beginning-of-line)))

(defadvice ruby-beginning-of-indent (around
                                     mumamo-ad-ruby-beginning-of-indent
                                     activate
                                     compile
                                     )
  (if t
      (mumamo-ruby-beginning-of-indent)
    ad-do-it)
  )

(defun mumamo-indentor-at (pos)
  "Return indentor overlay at POS."
  (let ((here (point))
        eol-pos)
    (goto-char pos)
    (setq eol-pos (line-end-position))
    (goto-char here)
    (catch 'ind
      (dolist (ovl (or (overlays-at eol-pos)
                       (when (> eol-pos 1)
                         (overlays-at (1- eol-pos)))))
        (when (overlay-get ovl 'indentor-chunk)
          (throw 'ind ovl))))))

(defun mumamo-remove-indentor (indentor)
  (let (beg end)
    (goto-char (overlay-start indentor))
    (setq beg (point-at-bol))
    (goto-char (overlay-end indentor))
    (setq end (1+ (point-at-eol)))
    (delete-region beg end)
    (delete-overlay indentor)
    (setq indentor nil)))

(defun mumamo-indent-indentor (indentor)
  (goto-char (overlay-start indentor))
  (if (= 2 (point-at-bol))
      (progn
        (back-to-indentation)
        (delete-region 2 (point))
        (insert "                    "))
    (indent-according-to-mode))
  (goto-char (overlay-end indentor))
  (indent-according-to-mode))

(defun mumamo-make-indentor (indentor-chunk chunk-string)
  (let* ((beg (point))
         (syntax-min-max (mumamo-chunk-syntax-min-max indentor-chunk t))
         (inner (with-current-buffer (overlay-buffer indentor-chunk)
                  (buffer-substring-no-properties (cdr syntax-min-max)
                                                  (car syntax-min-max))))
         indentor)
    (insert inner)
    (insert "\n\n")
    (setq indentor (make-overlay beg (1- (point)) nil t t))
    (overlay-put indentor 'indentor-chunk indentor-chunk)
    (overlay-put indentor 'face 'secondary-selection)
    (overlay-put indentor 'indentor-chunk-string chunk-string)
    (overlay-put indentor-chunk 'mumamo-indentor indentor)
    indentor))

;;(mumamo-fun-eq 'js-mode 'javascript-mode)
(defun mumamo-fun-eq (fun1 fun2)
  "Return non-nil if same functions or aliases."
  (or (eq fun1 fun2)
      (progn
        (while (and (fboundp fun1)
                    (symbolp (symbol-function fun1)))
          (setq fun1 (symbol-function fun1)))
        (while (and (fboundp fun2)
                    (symbolp (symbol-function fun2)))
          (setq fun2 (symbol-function fun2)))
        (eq fun1 fun2))))

(defun mumamo-indent-line-function-1 (prev-line-chunks
                                      last-parent-major-indent
                                      entering-submode-arg)
  ;; Fix-me: error indenting in xml-as-string at <?\n?>
  ;; Fix-me: clean up, use depth diff. go back to sibling not to main etc.
  ;; Fix-me: Add indentation hints to chunks, for example heredocs and rhtml.
  ;; Fix-me: maybe use special indentation functions for certain multi major modes? rhtml?
  "Indent current line.
When doing that care must be taken if this line's major modes at
the start and end are different from previous line major modes.
The latter may be known through the parameter PREV-LINE-CHUNKS.

Also the indentation of the last previous main major line may be
necessary to know.  This may be known through the parameter
LAST-PARENT-MAJOR-INDENT.

If the two parameters above are nil then this function will
search backwards in the buffer to try to determine their values.

The following rules are used when indenting:

- If the major modes are the same in this and the previous line
  then indentation is done using that major mode.

- Exception: If the chunks are not the same AND there is
  precisely one chunk between them which have the property value
  of 'mumamo-next-indent equal to 'mumamo-template-indentor then
  a special indent using the content of the middle chunk is
  done. An example of this is eRuby where a middle chunk could
  look like:

    <% 3.times do %>

  This example will increase indentation for the next line the
  same way as the chunk content would do in single major mode
  ruby-mode.

  FIXE-ME: IMPLEMENT THE ABOVE!

- Otherwise if going into a submode indentation is increased by
  `mumamo-submode-indent-offset' (if this is nil then indentation
  will instead be 0).

- However first non-empty line indentation in a chunk when going
  in is special if prev-prev chunk is on same mumamo-depth and
  have the same major mode. Then indent relative last non-empty
  line in prev-prev chunk.

- When going out of a submode indentation is reset to
  LAST-PARENT-MAJOR-INDENT.

- At the border the 'dividers' should be indented as the parent
  chunk. There are the following typical situations regarding
  inner/outer major modes:

  1) <style type='text/css'>
     Going in next line; first char outer; line end inner;

  2) </style>
     Going out this line; First char inner or outer; line end outer;

  3) <?php
     Going in next line; first char outer or inner; line end inner;

  4) ?>
     Going out this line; first char inner; line end outer;

  From this we deduce the following way to compute if we are
  going in or out:

  - Odd above (going in): Compare prev line end's mumamo-depth
    with current line end's dito. Set flag for first line in
    chunk.

  - Even above (going out): Same test as for going in, but going
    out happens on current line.
"
  ;;(msgtrc "indent-line-function-1 blp=%s" (line-beginning-position))
  (setq prev-line-chunks nil)
  ;;(setq last-parent-major-indent nil)
  ;;(setq entering-submode-arg nil)
  (unless prev-line-chunks
    (save-excursion
      (goto-char (line-beginning-position 1))
      (unless (= (point) 1)
        (skip-chars-backward "\n\t ")
        (goto-char (line-beginning-position 1))
        (setq prev-line-chunks (mumamo-indent-current-line-chunks nil))
        ;;(msgtrc "%d:prev-line-chunks=%S" (save-restriction (widen) (line-number-at-pos)) prev-line-chunks )
        )))
  (let* ((prev-line-chunk0 (nth 0 prev-line-chunks))
         (prev-line-chunk2 (nth 2 prev-line-chunks))
         (prev-line-chunk3 (nth 3 prev-line-chunks))
         (prev-line-major0 (mumamo-chunk-major-mode (nth 0 prev-line-chunks)))
         (prev-line-major1 (mumamo-chunk-major-mode (nth 1 prev-line-chunks)))
         (prev-line-major2 (mumamo-chunk-major-mode (nth 2 prev-line-chunks)))
         (prev-line-major3 (mumamo-chunk-major-mode (nth 3 prev-line-chunks)))
         (prev-depth2 (if prev-line-chunk2
                          (overlay-get prev-line-chunk2 'mumamo-depth)
                        0))
         (prev-depth3 (if prev-line-chunk3
                          (overlay-get prev-line-chunk3 'mumamo-depth)
                        0))

         (this-line-chunks (mumamo-indent-current-line-chunks (nth 3 prev-line-chunks)))
         ;;(dummy (msgtrc "%d:this-line-chunks=%S" (save-restriction (widen) (line-number-at-pos)) this-line-chunks))
         (this-line-chunk0 (nth 0 this-line-chunks))
         (this-line-chunk2 (nth 2 this-line-chunks))
         (this-line-chunk3 (nth 3 this-line-chunks))
         (this-line-major0 (mumamo-chunk-major-mode (nth 0 this-line-chunks)))
         (this-line-major1 (mumamo-chunk-major-mode (nth 1 this-line-chunks)))
         (this-line-major2 (mumamo-chunk-major-mode (nth 2 this-line-chunks)))
         (this-line-major3 (mumamo-chunk-major-mode (nth 3 this-line-chunks)))
         (this-depth2 (overlay-get this-line-chunk2 'mumamo-depth))
         (this-depth3 (overlay-get this-line-chunk3 'mumamo-depth))

         ;;(dummy (msgtrc "a\t this=%S" this-line-chunks))
         this-line-indent-major
         major-indent-line-function
         (main-major (mumamo-main-major-mode))
         (old-indent (current-indentation))
         (next-entering-submode (if (< prev-depth3 this-depth3) 'yes 'no))
         (entering-submode
          ;; Fix-me
          (progn
            (unless nil ;entering-submode-arg
              (let* ((prev-prev-line-chunks
                      (save-excursion
                        (goto-char (line-beginning-position 0))
                        (unless (bobp)
                          (skip-chars-backward "\n\t ")
                          (goto-char (line-beginning-position 1))
                          (let ((chunks (mumamo-indent-current-line-chunks nil)))
                            ;;(msgtrc "%d:prev-prev-line-chunks=%S" (save-restriction (widen) (line-number-at-pos)) chunks)
                            chunks))))
                     (prev-prev-line-chunk2 (nth 2 prev-prev-line-chunks))
                     (prev-prev-line-chunk3 (nth 3 prev-prev-line-chunks))
                     (prev-prev-depth2 (when prev-prev-line-chunk2
                                         (overlay-get prev-prev-line-chunk2 'mumamo-depth)))
                     (prev-prev-depth3 (when prev-prev-line-chunk3
                                         (overlay-get prev-prev-line-chunk3 'mumamo-depth))))
                ;;(msgtrc "depths 2=%s/%s/%s 3=%s/%s/%s" prev-prev-depth2 prev-depth2 this-depth2 prev-prev-depth3 prev-depth3 this-depth3)
                (setq entering-submode-arg
                      (if prev-prev-depth2
                          (if (and (eq prev-prev-line-chunk2
                                       (overlay-get prev-line-chunk2 'mumamo-prev-chunk))
                                   (< prev-prev-depth2 prev-depth2))
                              'yes
                            'no)
                        (if (> this-depth2 0) 'yes 'no)
                        ))
                ))
            (eq 'yes entering-submode-arg)
            )) ;; fix-me
         ;; Fix-me
         (leaving-submode (> prev-depth2 this-depth2))
         want-indent ;; The indentation we desire
         got-indent
         (here-on-line (point-marker))
         this-pending-undo-list
         (while-n1 0)
         (while-n2 0)
         (while-n3 0)
         ;; Is there a possible indentor chunk on this line?:
         (this-line-indentor-chunk (when (> (overlay-start this-line-chunk2)
                                            (point-at-bol))
                                     (overlay-get this-line-chunk2 'mumamo-prev-chunk)))
         ;;(dummy (msgtrc "this-line-indentor-chunk=%S" this-line-indentor-chunk))
         ;; Check if this really is an indentor chunk:
         ;; Fix-me: 'mumamo-indentor is not put on the chunk yet since
         ;; it is done in mumamo-template-indent-get-chunk-shift ... -
         ;; and now it is calle too often ...
         (this-line-indentor-prev (when this-line-indentor-chunk
                                    (overlay-get this-line-indentor-chunk 'mumamo-prev-chunk)))
         (this-line-is-indentor (and this-line-indentor-prev
                                     (eq (overlay-get this-line-indentor-prev 'mumamo-next-indent)
                                         'mumamo-template-indentor)
                                     (progn
                                       (goto-char (overlay-start this-line-indentor-chunk))
                                       (back-to-indentation)
                                       (= (point) (overlay-start this-line-indentor-chunk)))))
         ;; Fix-me: rewrite and reorder. We do not need both shift-in and shift-out
         (this-template-shift (when this-line-is-indentor
                                (mumamo-template-indent-get-chunk-shift this-line-indentor-chunk)))
         ;;(dummy (msgtrc "this-line-indentor=%s, %S" this-template-shift this-line-is-indentor))
         ;; Fix-me: skip over blank lines backward here:
         (prev-template-indentor (when prev-line-chunk0
                              (unless (eq this-line-chunk0 prev-line-chunk0)
                                (let* ((prev (overlay-get this-line-chunk0 'mumamo-prev-chunk))
                                       (prev-prev (overlay-get prev 'mumamo-prev-chunk)))
                                  (when (and (eq prev-prev prev-line-chunk0)
                                             (eq (overlay-get prev-prev 'mumamo-next-indent)
                                                 'mumamo-template-indentor))
                                    prev)))))
         (prev-template-shift-rec (when prev-template-indentor
                               (mumamo-template-indent-get-chunk-shift prev-template-indentor)
                               ))
         (template-shift (if (and (car this-template-shift) (/= 0 (car this-template-shift)))
                             (car this-template-shift)
                           (when prev-template-shift-rec
                             (cdr prev-template-shift-rec))))
         (template-indent-abs (when (and template-shift
                                         (/= 0 template-shift))
                                (+ template-shift
                                   (let ((here (point)))
                                     (if prev-template-indentor
                                         (goto-char (overlay-start prev-template-indentor))
                                       (goto-char (overlay-start this-line-indentor-chunk))
                                       (skip-chars-backward " \t\r\n\f"))
                                     (prog1
                                         (current-indentation)
                                       (goto-char here))))))
         )
    (when (and leaving-submode entering-submode)
      (message "Do not know how to indent here (both leaving and entering sub chunks)")
      )
    ;; Fix-me: indentation
    ;;(error "Leaving=%s, entering=%s this0,1,2,3=%s,%s,%s,%s" leaving-submode entering-submode this-line-major0 this-line-major1 this-line-major2 this-line-major3)
    (when (or leaving-submode entering-submode)
      (unless last-parent-major-indent
        (save-excursion
          ;;(while (and (> 500 (setq while-n1 (1+ while-n1)))
          (while (and (mumamo-while 500 'while-n1 "last-parent-major-indent")
                      (not last-parent-major-indent))
            (if (bobp)
                (setq last-parent-major-indent 0)
              (goto-char (line-beginning-position 0))
              (when (mumamo-fun-eq main-major
                                   (mumamo-chunk-major-mode
                                    (car
                                     (mumamo-indent-current-line-chunks nil)))
                                   )
                (skip-chars-forward " \t")
                (if (eolp)
                    (setq last-parent-major-indent 0)
                  (setq last-parent-major-indent (current-column)))))))))
    (mumamo-msgindent "  leaving-submode=%s, entering-submode=%s" leaving-submode entering-submode)
    ;;(msgtrc "  leaving-submode=%s, entering-submode=%s, template-indentor=%s" leaving-submode entering-submode template-indentor)

    ;; Fix-me: use this.
    ;; - clean up after chunk deletion
    ;; - next line after a template-indentor, what happens?
    ;;(setq template-indentor nil) ;; fix-me
    (cond
     ( template-indent-abs
       (setq want-indent (max 0 template-indent-abs)))
     ( leaving-submode
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;;;;; First line after submode
       (mumamo-msgindent "  leaving last-parent-major-indent=%s" last-parent-major-indent)
       (if (eq (overlay-get (overlay-get this-line-chunk0 'mumamo-prev-chunk)
                            'mumamo-next-indent)
               'heredoc)
           (setq want-indent 0)
         (setq want-indent last-parent-major-indent)))

     ( entering-submode
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;;;;; First line in submode
       ;;(setq this-line-indent-major this-line-major0)
       (setq this-line-indent-major (mumamo-indent-get-major-to-use this-line-major0 this-depth3))
       ;;(when (and prev-line-major0 (not (mumamo-fun-eq this-line-major0 prev-line-major0))) (setq this-line-indent-major prev-line-major0))
       (mumamo-msgindent "  this-line-indent-major=%s, major-mode=%s this0=%s" this-line-indent-major major-mode this-line-major0)
       (mumamo-msgindent "  mumamo-submode-indent-offset=%s" mumamo-submode-indent-offset)
       (unless (mumamo-fun-eq this-line-indent-major major-mode)
         (mumamo-set-major this-line-indent-major this-line-chunk0))
       (setq want-indent (+ last-parent-major-indent
                            (if (= 0 last-parent-major-indent)
                                (if mumamo-submode-indent-offset-0
                                    mumamo-submode-indent-offset-0
                                  -1000)
                              (if mumamo-submode-indent-offset
                                  mumamo-submode-indent-offset
                                -1000))))
       (unless (< 0 want-indent) (setq want-indent nil))
       (when (and want-indent (mumamo-indent-use-widen major-mode))
         ;; In this case only use want-indent if it is bigger than the
         ;; indentation calling indent-line-function would give.
         (condition-case nil
             (atomic-change-group
               (mumamo-call-indent-line (nth 0 this-line-chunks))
               (when (> want-indent (current-indentation))
                 (signal 'mumamo-error-ind-0 nil))
               (setq want-indent nil))
           (mumamo-error-ind-0)))
       (unless want-indent
         (mumamo-call-indent-line (nth 0 this-line-chunks)))
       (mumamo-msgindent "  enter sub.want-indent=%s, curr=%s, last-main=%s" want-indent (current-indentation)
                         last-parent-major-indent)
       ;;(unless (> want-indent (current-indentation)) (setq want-indent nil))
       )

     ( t
       ;; We have to change major mode, because we know nothing
       ;; about the requirements of the indent-line-function:
       ;; Fix-me: This may be cured by RMS suggestion to
       ;; temporarily set all variables back to global values?
       (setq this-line-indent-major (mumamo-indent-get-major-to-use this-line-major0 this-depth3))
       (mumamo-msgindent "  this-line-indent-major=%s" this-line-indent-major)
       (unless (mumamo-fun-eq this-line-indent-major major-mode) (mumamo-set-major this-line-indent-major this-line-chunk0))
       ;; Use the major mode at the beginning of since a sub chunk may
       ;; start at start of line.
       (if (mumamo-fun-eq this-line-major1 main-major)
           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           ;;;;; In main major mode
           ;;
           ;; Take care of the case when all the text is in a
           ;; sub chunk. In that case use the same indentation as if
           ;; the code all belongs to the surrounding major mode.
           (let ((here (point))
                 (use-widen (mumamo-indent-use-widen main-major)))
             ;; If we can't indent indent using the main major mode
             ;; because it is only blanks and we should not widen,
             ;; then use the indentation on the line where it starts.
             (mumamo-msgindent "  In main major mode")
             (forward-line 0)
             (skip-chars-backward " \t\n\r\f")
             (forward-line 0)
             (if (or use-widen (>= (point) (overlay-start this-line-chunk0)))
                 (progn
                   (goto-char here)
                   (mumamo-call-indent-line this-line-chunk0))
               (setq want-indent (current-indentation))
               (goto-char here))
             (mumamo-msgindent "  In main major mode B")
             (setq last-parent-major-indent (current-indentation)))
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;; In sub major mode
         ;;
         ;; Get the indentation the major mode alone would use:
         ;;(setq got-indent (mumamo-get-major-mode-indent-column))
         ;; Since this line has another major mode than the
         ;; previous line we instead want to indent relative to
         ;; that line in a way decided in mumamo:
         (mumamo-msgindent "  In sub major mode")
         (let ((chunk (mumamo-get-chunk-save-buffer-state (point)))
               (font-lock-dont-widen t)
               ind-zero
               (here (point))
               ind-on-first-sub-line)
           (save-restriction
             (mumamo-update-obscure chunk here)
             (let ((syn-min-max (mumamo-chunk-syntax-min-max chunk nil)))
               (narrow-to-region (car syn-min-max)
                                 (cdr syn-min-max)))
             (condition-case nil
                 (atomic-change-group
                   (mumamo-call-indent-line (nth 0 this-line-chunks))
                   (when (= 0 (current-indentation))
                     (setq ind-zero t)
                     ;; It is maybe ok if indentation on first sub
                     ;; line is 0 so check that:
                     (goto-char (point-min))
                     (widen)
                     (setq ind-on-first-sub-line (current-indentation))
                     (goto-char here)
                     (signal 'mumamo-error-ind-0 nil)))
               (mumamo-error-ind-0))
             ;; Unfortunately the indentation can sometimes get 0
             ;; here even though it is clear it should not be 0. This
             ;; happens when there are only comments or empty lines
             ;; above.
             ;;
             ;; See c:/test/erik-lilja-index.php for an example.
             (when ind-zero ;(and t (= 0 (current-indentation)))
               (save-excursion
                 (setq want-indent 0)
                 (unless (= 0 ind-on-first-sub-line)
                   ;;(while (and (> 500 (setq while-n2 (1+ while-n2)))
                   (while (and (mumamo-while 500 'while-n2 "want-indent")
                               (= 0 want-indent)
                               (/= (point) (point-min)))
                     (beginning-of-line 0)
                     (setq want-indent (current-indentation)))
                   ;; Now if want-indent is still 0 we need to look further above
                   (when (= 0 want-indent)
                     (widen)
                     ;;(while (and (> 500 (setq while-n3 (1+ while-n3)))
                     (while (and (mumamo-while 500 'while-n3 "want-indent 2")
                                 (= 0 want-indent)
                                 (/= (point) (point-min)))
                       (beginning-of-line 0)
                       (setq want-indent (current-indentation)))
                     ;; If we got to the main major mode we need to add
                     ;; the special submode offset:
                     (let* ((ovl (mumamo-get-chunk-save-buffer-state (point)))
                            (major (mumamo-chunk-major-mode ovl)))
                       (when (mumamo-fun-eq major main-major)
                         (setq want-indent (+ want-indent
                                              (if (= 0 want-indent)
                                                  mumamo-submode-indent-offset-0
                                                mumamo-submode-indent-offset)))))))))
             )))))
    (when want-indent
      ;;(msgtrc "indent-line-to %s at line-beginning=%s" want-indent (line-beginning-position))
      (indent-line-to want-indent))
    ;; (when (and template-shift (/= 0 template-shift))
    ;;   (let ((ind (+ (current-indentation) template-shift)))
    ;;     (indent-line-to ind)))
    ;; (when template-indent-abs
    ;;   (indent-line-to template-indent-abs))
    (goto-char here-on-line)
    ;;(msgtrc "exit: %s" (list this-line-chunks last-parent-major-indent))
    (list this-line-chunks last-parent-major-indent next-entering-submode)))

;; Fix-me: use this for first line in a submode
;; Fix-me: check more carefully for widen since it may lead to bad results.
(defun mumamo-indent-use-widen (major-mode)
  "Return non-nil if widen before indentation in MAJOR-MODE."
  (let* ((specials (cadr (assoc major-mode mumamo-indent-widen-per-major)))
         (use-widen (memq 'use-widen specials))
         (use-widen-maybe (assq 'use-widen specials)))
    (or use-widen
        (memq mumamo-multi-major-mode (cadr use-widen-maybe)))))
;;(mumamo-indent-use-widen 'php-mode)
;;(mumamo-indent-use-widen 'nxhtml-mode)
;;(mumamo-indent-use-widen 'html-mode)

;; Fix-me: remove
;; (defun mumamo-indent-special-or-default (default-indent)
;;   "Indent to DEFAULT-INDENT unless a special indent can be done."
;;   (mumamo-with-major-mode-indentation major-mode
;;     `(progn
;;        (if (mumamo-indent-use-widen major-mode)
;;            (save-restriction
;;              (widen)
;;              (mumamo-msgindent "=> special-or-default did widen, %s" major-mode)
;;              (funcall indent-line-function))
;;          (indent-to-column default-indent)))))

(defun mumamo-call-indent-line (chunk)
  "Call the relevant `indent-line-function'."
  ;;(msgtrc "call-indent-line %s, lbp=%s" chunk (line-beginning-position))
  (if nil
      (mumamo-with-major-mode-indentation major-mode
        `(save-restriction
           (when (mumamo-indent-use-widen major-mode)
             (mumamo-msgindent "=> indent-line did widen")
             (widen))
           (funcall indent-line-function)))
    (let ((maj (car mumamo-major-mode-indent-line-function))
          (fun (cdr mumamo-major-mode-indent-line-function)))
      (assert (mumamo-fun-eq maj major-mode))
      (save-restriction
        ;; (unless (mumamo-indent-use-widen major-mode)
        ;;   (let ((syn-min-max (mumamo-chunk-syntax-min-max chunk nil)))
        ;;     (narrow-to-region (car syn-min-max) (cdr syn-min-max))))
        (let ((mumamo-stop-widen (not (mumamo-indent-use-widen major-mode))))
          (if (not mumamo-stop-widen)
              (widen)
            (let ((syn-min-max (mumamo-chunk-syntax-min-max chunk nil)))
              (narrow-to-region (car syn-min-max) (cdr syn-min-max))))
          ;;(msgtrc "call-indent-line fun=%s" fun)
          ;;(funcall fun)
          ;; Fix-me: Use mumamo-funcall-evaled to avoid (widen):
          (mumamo-funcall-evaled fun)
          )))))

(defvar mumamo-stop-widen nil)
(when nil
  (let* ((fun 'describe-variable)
         (lib (symbol-file fun 'defun)))
    (find-function-search-for-symbol fun nil lib)))

(defun mumamo-funcall-evaled (fun &rest args)
  "Make sure FUN is evaled, then call it.
This make sure (currently) that defadvice for primitives are
called.  They are not called in byte compiled code.

See URL `http://debbugs.gnu.org/cgi/bugreport.cgi?bug=5863' since
this may change."
  (when mumamo-stop-widen
    (unless (get fun 'mumamo-evaled)
      (let* ((lib (symbol-file fun 'defun))
             (where (find-function-search-for-symbol fun nil lib))
             (buf (car where))
             (pos (cdr where)))
        (with-current-buffer buf
          (let ((close (and (not (buffer-modified-p))
                            (= 1 (point)))))
            ;;(goto-char pos) (eval-defun nil)
            (msgtrc "mumamo-funcall-evaled %s" (current-buffer))
            (eval-buffer)
            (when close (kill-buffer))))
        (put fun 'mumamo-evaled t))))
  (apply 'funcall fun args))

;;(require 'advice)
(defun mumamo-defadvice-widen ()
  (defadvice widen (around
                    mumamo-ad-widen
                    activate
                    compile
                    )
    (unless (and mumamo-multi-major-mode
                 mumamo-stop-widen)
      ad-do-it)))
(eval-after-load 'mumamo
  '(mumamo-defadvice-widen))

;; (defadvice font-lock-fontify-buffer (around
;;                                      mumam-ad-font-lock-fontify-buffer
;;                                      activate
;;                                      compile
;;                                      )
;;   (if mumamo-multi-major-mode
;;       (save-restriction
;;         (let* ((chunk (mumamo-find-chunks (point) "font-lock-fontify-buffer advice"))
;;                (syn-min-max (mumamo-chunk-syntax-min-max chunk nil))
;;                (syn-min (car syn-min-max))
;;                (syn-max (cdr syn-min-max))
;;                (mumamo-stop-widen t))
;;           (narrow-to-region syn-min syn-max)
;;           (font-lock-fontify-region syn-min syn-max)))
;;     ad-do-it))

(defun mumamo-indent-region-function (start end)
  "Indent the region between START and END."
  (save-excursion
    (setq end (copy-marker end))
    (goto-char start)
    (let ((old-point -1)
          prev-line-chunks
          last-parent-major-indent
          entering-submode-arg
          ;; Turn off validation during indentation
          (old-rng-validate-mode (when (boundp 'rng-validate-mode) rng-validate-mode))
          (rng-nxml-auto-validate-flag nil)
          (nxhtml-use-imenu nil)
          fontification-functions
          rng-nxml-auto-validate-flag
          (nxhtml-mode-hook (mumamo-get-hook-value
                             'nxhtml-mode-hook
                             '(html-imenu-setup)))
          ;;
          (while-n1 0))
      (when old-rng-validate-mode (rng-validate-mode -1))
      ;;(while (and (> 3000 (setq while-n1 (1+ while-n1)))
      (while (and (mumamo-while 3000 'while-n1 "indent-region")
                  (< (point) end)
                  (/= old-point (point)))
        ;;(message "mumamo-indent-region-function, point=%s" (point))
        (or (and (bolp) (eolp))
            (let ((ret (mumamo-indent-line-function-1
                        prev-line-chunks
                        last-parent-major-indent
                        entering-submode-arg)))
              (setq prev-line-chunks         (nth 0 ret))
              (setq last-parent-major-indent (nth 1 ret))
              (setq entering-submode-arg     (nth 2 ret))))
        (setq old-point (point))
        (forward-line 1))
      (when old-rng-validate-mode (rng-validate-mode 1)))
    (message "Ready indenting region")))


(defun mumamo-fill-forward-paragraph-function(&optional arg)
  "Function to move over paragraphs used by filling code.
This is the buffer local value of
`fill-forward-paragraph-function' when mumamo is used."
  ;; fix-me: Do this chunk by chunk
  ;; Fix-me: use this (but only in v 23)
  (let* ((ovl (mumamo-get-chunk-save-buffer-state (point)))
         (major (mumamo-chunk-major-mode ovl)))
    (mumamo-with-major-mode-fontification major
      fill-forward-paragraph-function)))

(defun mumamo-fill-chunk (&optional justify)
  "Fill each of the paragraphs in the current chunk.
Narrow to chunk region trimmed white space at the ends.  Then
call `fill-region'.

The argument JUSTIFY is the same as in `fill-region' and a prefix
behaves the same way as there."
  (interactive (progn
		 (barf-if-buffer-read-only)
		 (list (if current-prefix-arg 'full))))
  (let* ((ovl (mumamo-get-chunk-save-buffer-state (point)))
         (major (mumamo-chunk-major-mode ovl)))
    ;; Fix-me: There must be some bug that makes it necessary to
    ;; always change mode when fill-paragraph-function is
    ;; c-fill-paragraph.

    ;;(unless (mumamo-fun-eq major major-mode) (mumamo-set-major major ovl))
    (mumamo-set-major major ovl)

    (save-restriction
      (mumamo-update-obscure ovl (point))
      (let* ((syn-min-max (mumamo-chunk-syntax-min-max ovl nil))
             (syn-min (car syn-min-max))
             (syn-max (cdr syn-min-max))
             use-min
             (here (point-marker)))
        (goto-char syn-min)
        (skip-syntax-forward " ")
        ;; Move back over chars that have whitespace syntax but have the p flag.
        (backward-prefix-chars)
        (setq use-min (point))
        (goto-char syn-max)
        (skip-syntax-backward " ")
        (fill-region use-min (point) justify)))))

;; (defvar mumamo-dont-widen)
;; (defadvice widen  (around
;;                    mumamo-ad-widen
;;                    activate
;;                    disable
;;                    compile
;;                    )
;;   "Make `widen' do nothing.
;; This is for `mumamo-fill-paragraph-function' and is necessary
;; when `c-fill-paragraph' is the real function used."
;;   (unless (and (boundp 'mumamo-dont-widen)
;;                mumamo-dont-widen)
;;     ad-do-it))

(defadvice flymake-display-warning (around
                                    mumamo-ad-flymake-display-warning
                                    activate
                                    compile)
  "Display flymake warnings in the usual Emacs way."
  (let ((msg (ad-get-arg 0)))
    ;; Fix-me: Can't get backtrace here. Report it.
    ;;(setq msg (format (concat msg "\n%S" (with-output-to-string (backtrace)))))
    (lwarn '(flymake) :error msg)))
;;(lwarn '(flymake) :error "the warning")

(defun mumamo-forward-chunk ()
  "Move forward to next chunk."
  (interactive)
  (let* ((chunk (mumamo-get-chunk-save-buffer-state (point)))
         (end-pos (overlay-end chunk)))
    (goto-char (min end-pos
                    (point-max)))))

(defun mumamo-backward-chunk ()
  "Move backward to previous chunk."
  (interactive)
  (let* ((chunk (mumamo-get-chunk-save-buffer-state (point)))
         (start-pos (overlay-start chunk)))
    (goto-char (max (1- start-pos)
                    (point-min)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Spell checking

(defun mumamo-flyspell-verify ()
  "Function used for `flyspell-generic-check-word-predicate'."
  (let* ((chunk (when mumamo-multi-major-mode
                  (mumamo-find-chunks (point) "mumamo-lyspell-verify")))
         (chunk-major (when chunk (mumamo-chunk-major-mode chunk)))
         (mode-predicate (when chunk-major
                           (let ((predicate (get chunk-major
                                                 'flyspell-mode-predicate)))
                             (if predicate
                                 predicate
                               (if (mumamo-derived-from-mode chunk-major
                                                             'text-mode)
                                   nil
                                 'flyspell-generic-progmode-verify)))))
         )
    (if mode-predicate
        ;; Fix-me: (run-hooks 'flyspell-prog-mode-hook)
        (funcall mode-predicate)
      t)))

;; (featurep 'cc-engine)
(eval-after-load 'cc-engine
  (progn
    ;; From Alan's mail 2009-12-03: C Mode: acceleration in brace
    ;; deserts.
    ;; Fix-me: Should they be here, or...?
    (put 'c-state-cache 'permanent-local t)
    (put 'c-state-cache-good-pos 'permanent-local t)
    (put 'c-state-nonlit-pos-cache 'permanent-local t)
    (put 'c-state-nonlit-pos-cache-limit 'permanent-local t)
    (put 'c-state-brace-pair-desert 'permanent-local t)
    (put 'c-state-point-min 'permanent-local t)
    (put 'c-state-point-min-lit-type 'permanent-local t)
    (put 'c-state-point-min-lit-start 'permanent-local t)
    (put 'c-state-min-scan-pos 'permanent-local t)
    (put 'c-state-old-cpp-beg 'permanent-local t)
    (put 'c-state-old-cpp-end 'permanent-local t)

    ))

;; Fix-me: Seems perhaps like c-state-point-min-lit-start is reset in
;; c-state-mark-point-min-literal because c-state-literal-at returns
;; nil. (Or is (car lit) nil?)

(defvar mumamo-c-state-cache-init nil)
(make-variable-buffer-local 'mumamo-c-state-cache-init)
(put 'mumamo-c-state-cache-init 'permanent-local t)

(defun mumamo-c-state-cache-init ()
  (unless mumamo-c-state-cache-init
    ;;(msgtrc "c-state-cache-init running")
    (setq mumamo-c-state-cache-init t)
    (setq c-state-cache (or c-state-cache nil))
    (put 'c-state-cache 'permanent-local t)
    (setq c-state-cache-good-pos (or c-state-cache-good-pos 1))
    (put 'c-state-cache-good-pos 'permanent-local t)
    (setq c-state-nonlit-pos-cache (or c-state-nonlit-pos-cache nil))
    (put 'c-state-nonlit-pos-cache 'permanent-local t)
    (setq c-state-nonlit-pos-cache-limit (or c-state-nonlit-pos-cache-limit 1))
    (put 'c-state-nonlit-pos-cache-limit 'permanent-local t)
    (setq c-state-brace-pair-desert (or c-state-brace-pair-desert nil))
    (put 'c-state-brace-pair-desert 'permanent-local t)
    (setq c-state-point-min (or c-state-point-min 1))
    (put 'c-state-point-min 'permanent-local t)
    (setq c-state-point-min-lit-type (or c-state-point-min-lit-type nil))
    (put 'c-state-point-min-lit-type 'permanent-local t)
    (setq c-state-point-min-lit-start (or c-state-point-min-lit-start nil))
    (put 'c-state-point-min-lit-start 'permanent-local t)
    (setq c-state-min-scan-pos (or c-state-min-scan-pos 1))
    (put 'c-state-min-scan-pos 'permanent-local t)
    (setq c-state-old-cpp-beg (or c-state-old-cpp-beg nil))
    (put 'c-state-old-cpp-beg 'permanent-local t)
    (setq c-state-old-cpp-end (or c-state-old-cpp-end nil))
    (put 'c-state-old-cpp-end 'permanent-local t)
    (c-state-mark-point-min-literal)))

(defadvice c-state-cache-init (around
                               mumamo-ad-c-state-cache-init
                               activate
                               compile
                               )
  (if (not mumamo-multi-major-mode)
      ad-do-it
    (mumamo-c-state-cache-init)))

;; Fix-me: Have to add per chunk local majors for this one.
(defun mumamo-c-state-literal-at (here)
  ;; If position HERE is inside a literal, return (START . END), the
  ;; boundaries of the literal (which may be outside the accessible bit of the
  ;; buffer).  Otherwise, return nil.
  ;;
  ;; This function is almost the same as `c-literal-limits'.  It differs in
  ;; that it is a lower level function, and that it rigourously follows the
  ;; syntax from BOB, whereas `c-literal-limits' uses a "local" safe position.
  (let* ((is-here (point))
         (s (syntax-ppss here))
         (ret (when (or (nth 3 s) (nth 4 s))	; in a string or comment
                (parse-partial-sexp (point) (point-max)
                                    nil			 ; TARGETDEPTH
                                    nil			 ; STOPBEFORE
                                    s			 ; OLDSTATE
                                    'syntax-table)	 ; stop at end of literal
                (cons (nth 8 s) (point)))))
    (goto-char is-here)
    ret))

;; (save-restriction
;;   (widen)
;;   (let* ((chunk (mumamo-find-chunks (point) "mumamo-c-state-literal-at"))
;;          (syntax-min-max (mumamo-chunk-syntax-min-max chunk t)))
;;     (narrow-to-region (car syntax-min-max) (cdr syntax-min-max)))
;;   (save-excursion
;;     (let ((c c-state-nonlit-pos-cache)
;;           pos npos lit)
;;       ;; Trim the cache to take account of buffer changes.
;;       (while (and c (> (car c) c-state-nonlit-pos-cache-limit))
;;         (setq c (cdr c)))
;;       (setq c-state-nonlit-pos-cache c)

;;       (while (and c (> (car c) here))
;;         (setq c (cdr c)))
;;       (setq pos (or (car c) (point-min)))

;;       (while (<= (setq npos (+ pos c-state-nonlit-pos-interval))
;;       	   here)
;;         (setq lit (c-state-pp-to-literal pos npos))
;;         (setq pos (or (cdr lit) npos)) ; end of literal containing npos.
;;         (setq c-state-nonlit-pos-cache (cons pos c-state-nonlit-pos-cache)))

;;       (if (> pos c-state-nonlit-pos-cache-limit)
;;           (setq c-state-nonlit-pos-cache-limit pos))
;;       (if (< pos here)
;;           (setq lit (c-state-pp-to-literal pos here)))
;;       lit))))


(defadvice c-state-literal-at (around
                               mumamo-ad-c-state-state-literal-at
                               activate
                               compile
                               )
  (if (not mumamo-multi-major-mode)
      ad-do-it
    (mumamo-c-state-literal-at (ad-get-arg 0))))


(defun mumamo-c-state-get-min-scan-pos ()
  ;; Return the lowest valid scanning pos.  This will be the end of the
  ;; literal enclosing point-min, or point-min itself.
  (save-restriction
    (save-excursion
      (widen)
      (mumamo-narrow-to-chunk-inner)
      (or (and c-state-min-scan-pos
               (>= c-state-min-scan-pos (point-min))
               c-state-min-scan-pos)
          (if (not c-state-point-min-lit-start)
              (goto-char (point-min))
            (goto-char c-state-point-min-lit-start)
            (if (eq c-state-point-min-lit-type 'string)
                (forward-sexp)
              (forward-comment 1)))
          (setq c-state-min-scan-pos (point))))))

(defadvice c-state-get-min-scan-pos (around
                                     mumamo-ad-c-state-get-min-scan-pos-at
                                     activate
                                     compile
                                     )
  (if (not mumamo-multi-major-mode)
      ad-do-it
    (setq ad-return-value (mumamo-c-state-get-min-scan-pos))))

(eval-after-load 'rng-match
;;;   (defun rng-match-init-buffer ()
;;;     (make-local-variable 'rng-compile-table)
;;;     (make-local-variable 'rng-ipattern-table)
;;;     (make-local-variable 'rng-last-ipattern-index))
  (progn
    (put 'rng-compile-table 'permanent-local t)
    (put 'rng-ipattern-table 'permanent-local t)
    (put 'rng-last-ipattern-index 'permanent-local t)
    ))

(eval-after-load 'flyspell
  (progn
    (put 'flyspell-mode 'permanent-local t)

    (put 'flyspell-generic-check-word-predicate 'permanent-local t)

    (put 'flyspell-casechars-cache 'permanent-local t)
    (put 'flyspell-ispell-casechars-cache 'permanent-local t)

    (put 'flyspell-not-casechars-cache 'permanent-local t)
    (put 'flyspell-ispell-not-casechars-cache 'permanent-local t)

    (put 'flyspell-auto-correct-pos 'permanent-local t)
    (put 'flyspell-auto-correct-region 'permanent-local t)
    (put 'flyspell-auto-correct-ring 'permanent-local t)
    (put 'flyspell-auto-correct-word 'permanent-local t)

    (put 'flyspell-consider-dash-as-word-delimiter-flag 'permanent-local t)

    (put 'flyspell-dash-dictionary 'permanent-local t)

    (put 'flyspell-dash-local-dictionary 'permanent-local t)

    (put 'flyspell-word-cache-start 'permanent-local t)
    (put 'flyspell-word-cache-end 'permanent-local t)
    (put 'flyspell-word-cache-word 'permanent-local t)
    (put 'flyspell-word-cache-result 'permanent-local t)

    (put 'flyspell-word-cache-start 'permanent-local t)


    (put 'flyspell-kill-ispell-hook 'permanent-local-hook t)
    (put 'flyspell-post-command-hook 'permanent-local-hook t)
    (put 'flyspell-pre-command-hook 'permanent-local-hook t)
    (put 'flyspell-after-change-function 'permanent-local-hook t)
    (put 'flyspell-hack-local-variables-hook 'permanent-local-hook t)
    (put 'flyspell-auto-correct-previous-hook 'permanent-local-hook t)

    (when mumamo-multi-major-mode
      (when (featurep 'flyspell)
        (setq flyspell-generic-check-word-predicate 'mumamo-flyspell-verify)))
    ))

(defun flyspell-mumamo-mode ()
  "Turn on function `flyspell-mode' for multi major modes."
  (interactive)
  (require 'flyspell)
  (setq flyspell-generic-check-word-predicate 'mumamo-flyspell-verify)
  (flyspell-mode 1)
  ;;(run-hooks 'flyspell-prog-mode-hook)
  )

(eval-after-load 'sgml-mode
  (progn
    (put 'sgml-tag-face-alist 'permanent-local t)
    (put 'sgml-display-text   'permanent-local t)
    (put 'sgml-tag-alist 'permanent-local t)
    (put 'sgml-face-tag-alist 'permanent-local t)
    (put 'sgml-tag-help 'permanent-local t)
    ))

(eval-after-load 'hl-line
  (progn
    (put 'hl-line-overlay 'permanent-local t)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; New versions of syntax-ppss functions, temporary written as defadvice.

(defadvice syntax-ppss-flush-cache (around
                                    mumamo-ad-syntax-ppss-flush-cache
                                    activate
                                    compile
                                    )
  "Support for mumamo.
See the defadvice for `syntax-ppss' for an explanation."
  (if (not mumamo-multi-major-mode)
      ad-do-it
    (let ((pos (ad-get-arg 0)))
      (let* ((chunk-at-pos (when (and (boundp 'mumamo-multi-major-mode)
                                      mumamo-multi-major-mode)
                             (mumamo-find-chunks-1 pos "syntax-ppss-flush-cache"))))
        (if chunk-at-pos
            (let* ((syntax-ppss-last  (overlay-get chunk-at-pos 'syntax-ppss-last))
                   (syntax-ppss-cache (overlay-get chunk-at-pos 'syntax-ppss-cache)))
              ;;(setq ad-return-value ad-do-it)
              ad-do-it
              (overlay-put chunk-at-pos 'syntax-ppss-last syntax-ppss-last)
              (overlay-put chunk-at-pos 'syntax-ppss-cache syntax-ppss-cache))
          ;;(setq ad-return-value ad-do-it)
          ad-do-it
          )))))

(defvar mumamo-syntax-chunk-at-pos nil
  "Internal use.")
(make-variable-buffer-local 'mumamo-syntax-chunk-at-pos)

;; Fix-me: Is this really needed?
;; See http://lists.gnu.org/archive/html/emacs-devel/2008-04/msg00374.html
(defadvice syntax-ppss-stats (around
                              mumamo-ad-syntax-ppss-stats
                              activate
                              compile
                              )
  "Support for mumamo.
See the defadvice for `syntax-ppss' for an explanation."
  (if mumamo-syntax-chunk-at-pos
      (let* ((syntax-ppss-stats
              (overlay-get mumamo-syntax-chunk-at-pos 'syntax-ppss-stats)))
        ad-do-it
        (overlay-put mumamo-syntax-chunk-at-pos 'syntax-ppss-stats syntax-ppss-stats))
    ad-do-it))

(defvar mumamo-syntax-ppss-major nil)

;; FIX-ME: There is a problem with " in xhtml files, especially after
;; syntax="...".  Looks like it is the " entry in
;; `sgml-font-lock-syntactic-keywords' that is jumping in!  Dumping
;; things in `font-lock-apply-syntactic-highlight' seems to show that.
;;
;; (I have put in some dump code in my patched version of
;; Emacs+EmacsW32 there for that.  This is commented out by default
;; and it will only work for the file nxhtml-changes.html which is big
;; enough for the problem to occur.  It happens at point 1109.)
;;
;; It is this piece of code where the problem arise:
;;
;;   (if (prog1
;;           (zerop (car (syntax-ppss (match-beginning 0))))
;;         (goto-char (match-end 0)))
;;       .)
;;
;;
;; It comes from `sgml-font-lock-syntactic-keywords' in sgml-mode.el
;; and is supposed to protect from " that is not inside a tag.
;; However in this case for the second " in syntax="..." `syntax-ppss'
;; returns 0 as the first element in its return value.  That happen
;; even though `major-mode' is correctly `html-mode'.  It leads to
;; that the property 'syntax with the value (1) is added to the "
;; after the css-mode chunk in syntax="...".  The problem persists
;; even if the chunk has `fundamental-mode' instead of `css-mode'.
;;
;; Bypassing the cache for `syntax-pss' by calling
;; `parse-partial-sexp' directly instead of doing ad-do-it (see
;; by-pass-chache in the code below) solves the problem for now.  It
;; does not feel like the right solution however.
;;
;; One way of temporary solving the problem is perhaps to modify
;; `mumamo-chunk-attr=' to make "" borders, but I am not sure that it
;; works and it is the wrong solution.
(defadvice syntax-ppss (around
                        mumamo-ad-syntax-ppss
                        activate
                        compile
                        )
  "Support for mumamo chunks.
For each chunk store as properties of the chunk the parse state
that is normally hold in `syntax-ppss-last' and
`syntax-ppss-cache'.

Compute the beginning parse state for a chunk this way:

- If the chunk major mode is the same as the main major mode for
  the multi major mode then parse from the beginning of the file
  to the beginning of the chunk using the main major mode.  While
  doing that jump over chunks that do not belong to the main
  major mode and cache the state at the end and beginning of the
  the main major mode chunks.

FIX-ME: implement above.  Solution?:
 (parse-partial-sexp syntax-min (1+ syntax-max) nil nil state-at-syntax-min)
Put this at next chunk's beginning.

- Otherwise set the state at the beginning of the chunk to nil.

Do here also other necessary adjustments for this."
  (if (not mumamo-multi-major-mode)
      ad-do-it
    (let ((pos (ad-get-arg 0)))
      (unless pos (setq pos (point)))
      (let* ((chunk-at-pos (when (and (boundp 'mumamo-multi-major-mode) mumamo-multi-major-mode)
                             (mumamo-find-chunks-1 pos "syntax-ppss")))
             (dump2 (and (boundp 'dump-quote-hunt)
                         dump-quote-hunt
                         (boundp 'start)
                         ;;(= 1109 start)
                         )))
        ;;(setq dump2 t)
        (setq mumamo-syntax-chunk-at-pos chunk-at-pos)
        (when dump2 (msgtrc "\npos=%s point-min=%s mumamo-syntax-ppss.chunk-at-pos=%s" pos (point-min) chunk-at-pos))
        (if chunk-at-pos
            (let* ((chunk-syntax-min-max (mumamo-chunk-syntax-min-max chunk-at-pos t))
                   (chunk-syntax-min (car chunk-syntax-min-max))
                   (chunk-major (mumamo-chunk-major-mode chunk-at-pos))
                   (syntax-ppss-last  (overlay-get chunk-at-pos 'syntax-ppss-last))
                   (syntax-ppss-cache (overlay-get chunk-at-pos 'syntax-ppss-cache))
                   (syntax-ppss-last-min  (overlay-get chunk-at-pos 'syntax-ppss-last-min))
                   (syntax-ppss-cache-min (list syntax-ppss-last-min))
                   ;; This must be fetch the same way as in syntax-ppss:
                   (syntax-begin-function (overlay-get chunk-at-pos 'syntax-begin-function))
                   (syntax-ppss-max-span (if chunk-syntax-min
                                             (/ (- pos chunk-syntax-min -2) 2)
                                           syntax-ppss-max-span))
                   (syntax-ppss-stats (let ((stats (overlay-get chunk-at-pos 'syntax-ppss-stats)))
                                        (if stats
                                            stats
                                          (default-value 'syntax-ppss-stats))))
                   (last-min-pos (or (car syntax-ppss-last-min)
                                     1))
                   )
              ;; If chunk has moved the cached values are invalid.
              (unless (= chunk-syntax-min last-min-pos)
                (setq syntax-ppss-last nil)
                (setq syntax-ppss-last-min nil)
                (setq syntax-ppss-cache nil)
                (setq syntax-ppss-cache-min nil)
                (setq syntax-ppss-stats (default-value 'syntax-ppss-stats)))
              (when dump2
                (msgtrc " get syntax-ppss-last-min=%s len=%s chunk=%s" syntax-ppss-last-min (length syntax-ppss-last-min) chunk-at-pos)
                (msgtrc " prop syntax-ppss-last-min=%s" (overlay-properties chunk-at-pos))
                (msgtrc " chunk-major=%s, %s, syntax-min=%s\n last-min=%s" chunk-major major-mode chunk-syntax-min syntax-ppss-last-min))
              ;;(setq dump2 nil)
              (when syntax-ppss-last-min
                (unless (car syntax-ppss-last-min)
                  ;;(msgtrc "fix-me: emacs bug workaround, setting car of syntax-ppss-last-min")
                  ;;(setcar syntax-ppss-last-min (1- chunk-syntax-min))
                  ;;(msgtrc "fix-me: emacs bug workaround, need new syntax-ppss-last-min because car is nil")
                  (setq syntax-ppss-last-min nil)
                  ))
              (unless syntax-ppss-last-min
                (setq syntax-ppss-last nil)
                (save-restriction
                  (widen)
                  (let* ((min-pos chunk-syntax-min)
                         (chunk-sub-major (mumamo-chunk-major-mode chunk-at-pos))
                         (main-major (mumamo-main-major-mode))
                         (is-main-mode-chunk (mumamo-fun-eq chunk-sub-major main-major)))
                    (when dump2 (msgtrc " min-pos=%s, is-main-mode-chunk=%s" min-pos is-main-mode-chunk))
                    ;; Looks like assert can not be used here for some reason???
                    ;;(assert (and min-pos) t)
                    (unless (and min-pos) (error "defadvice syntax-ppss: (and min-pos=%s)" min-pos))
                    (setq syntax-ppss-last-min
                          (cons min-pos ;;(1- min-pos)
                                (if nil ;is-main-mode-chunk
                                    ;; Fix-me: previous chunks as a
                                    ;; cache? The problem is updating
                                    ;; this. Perhaps it is possible to
                                    ;; prune how far back to go by
                                    ;; going to the first chunk
                                    ;; backwards where
                                    ;; (pars-partial-sexp min max) is
                                    ;; "nil"?
                                    (mumamo-with-major-mode-fontification main-major
                                      `(parse-partial-sexp 1 ,min-pos nil nil nil nil))
                                  (parse-partial-sexp 1 1))))
                    (setq syntax-ppss-cache-min (list syntax-ppss-last-min))
                    (when dump2 (msgtrc " put syntax-ppss-last-min=%s len=%s chunk=%s" syntax-ppss-last-min (length syntax-ppss-last-min) chunk-at-pos))
                    (when dump2 (msgtrc " prop syntax-ppss-last-min=%s" (overlay-properties chunk-at-pos)))
                    (overlay-put chunk-at-pos 'syntax-ppss-last-min syntax-ppss-last-min)
                    (let ((test-syntax-ppss-last-min
                           (overlay-get chunk-at-pos 'syntax-ppss-last-min)))
                      (when dump2 (msgtrc " test syntax-ppss-last-min=%s len=%s" test-syntax-ppss-last-min (length test-syntax-ppss-last-min)))
                      (when dump2 (msgtrc " propt syntax-ppss-last-min=%s" (overlay-properties chunk-at-pos)))
                      ))))
              (when dump2 (msgtrc " here 0, syntax-ppss-last=%s" syntax-ppss-last))
              (unless syntax-ppss-last
                (setq syntax-ppss-last syntax-ppss-last-min)
                (setq syntax-ppss-cache syntax-ppss-cache-min))
              ;;(syntax-ppss pos)
              (when dump2 (msgtrc " at 1, syntax-ppss-last=%s" syntax-ppss-last))
              (when dump2 (msgtrc " at 1, syntax-ppss-cache=%s" syntax-ppss-cache))
              (let (ret-val
                    (by-pass-cache t)
                    (dump2 dump2))
                (if (not by-pass-cache)
                    (progn
                      (when dump2
                        (let ((old-ppss (cdr syntax-ppss-last))
                              (old-pos (car syntax-ppss-last)))
                          ;;(assert (and old-pos pos) t)
                          (unless (and old-pos pos) (error "defadvice syntax-ppss: (and old-pos=%s pos=%s)" old-pos pos))
                          (msgtrc "parse-partial-sexp=>%s" (parse-partial-sexp old-pos pos nil nil old-ppss))))
                      (let (dump2)
                        (setq ret-val ad-do-it)))
                  (let ((old-ppss (cdr syntax-ppss-last))
                        (old-pos (car syntax-ppss-last)))
                    (when dump2
                      (msgtrc "Xparse-partial-sexp %s %s nil nil %s" old-pos pos old-ppss)
                      (let (dump2)
                        (msgtrc "ad-do-it=>%s" ad-do-it)))
                    (save-restriction
                      (widen)
                      ;;(assert (and old-pos pos) t)
                      (unless (and old-pos pos) (error "defadvice syntax-ppss 2 (and old-pos=%s pos=%s)" old-pos pos))
                      (when dump2
                        (msgtrc "parse-partial-sexp %s %s nil nil %s" old-pos pos old-ppss))
                      (setq ret-val (parse-partial-sexp old-pos pos nil nil old-ppss)))))
                (when dump2 (msgtrc " ==>ret-val=%s" ret-val))
                ;;(mumamo-backtrace "syntax-ppss")
                (setq ad-return-value ret-val))
              (overlay-put chunk-at-pos 'syntax-ppss-last syntax-ppss-last)
              (overlay-put chunk-at-pos 'syntax-ppss-cache syntax-ppss-cache)
              (overlay-put chunk-at-pos 'syntax-ppss-stats syntax-ppss-stats)
              )
          ad-do-it)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rng-valid.el support

;; Fix-me: The solution in this defadvice is temporary. The defadvice
;; for rng-do-some-validation should be fixed instead.
;; (ad-disable-advice 'rng-mark-error 'around 'mumamo-ad-rng-mark-error)
;; (ad-ensable-advice 'rng-mark-error 'around 'mumamo-ad-rng-mark-error)
(defadvice rng-mark-error (around
                           mumamo-ad-rng-mark-error
                           activate
                           compile)
  "Adjust range for error to chunks."
  (if (not mumamo-multi-major-mode)
      ad-do-it
    (let* ((beg (ad-get-arg 1))
           (end (ad-get-arg 2))
           (xml-parts nil)
           (chunk (mumamo-find-chunks beg "rng-mark-error")))
      (if (not chunk)
          ad-do-it
        (when (and (not (overlay-get chunk 'mumamo-region))
                   (mumamo-valid-nxml-chunk chunk))
          ;; rng-error
          (let ((part-beg (max (overlay-start chunk)
                               beg))
                (part-end (min (overlay-end chunk)
                               end)))
            (when (< part-beg part-end)
              (ad-set-arg 1 part-beg)
              (ad-set-arg 2 part-end)
              ad-do-it)))))))

(defadvice rng-do-some-validation-1 (around
                                     mumamo-ad-rng-do-some-validation-1
                                     activate
                                     compile)
  "Adjust validation to chunks."
  (if (not mumamo-multi-major-mode)
      ad-do-it
    (let (major-mode-chunk
          (point-max (1+ (buffer-size))) ;(save-restriction (widen) (point-max)))
          end-major-mode-chunk
          (limit (+ rng-validate-up-to-date-end
                    rng-validate-chunk-size))
          (remove-start rng-validate-up-to-date-end)
          (next-cache-point (+ (point) rng-state-cache-distance))
          (continue t)
          (xmltok-dtd rng-dtd)
          have-remaining-chars
          xmltok-type
          xmltok-start
          xmltok-name-colon
          xmltok-name-end
          xmltok-replacement
          xmltok-attributes
          xmltok-namespace-attributes
          xmltok-dependent-regions
          xmltok-errors
          (while-n1 0)
          (while-n2 0)
          (old-point -1)
          )
      ;;(msgtrc "> > > > > enter rng-do-some-validation-1, continue-p-function=%s" continue-p-function)
      (setq have-remaining-chars (< (point) point-max))
      (when (and continue (= (point) 1))
        (let ((regions (xmltok-forward-prolog)))
          (rng-clear-overlays 1 (point))
          (while regions
            (when (eq (aref (car regions) 0) 'encoding-name)
              (rng-process-encoding-name (aref (car regions) 1)
                                         (aref (car regions) 2)))
            (setq regions (cdr regions))))
        (unless (equal rng-dtd xmltok-dtd)
          (rng-clear-conditional-region))
        (setq rng-dtd xmltok-dtd))
      (setq while-n1 0)
      (while (and (mumamo-while 2000 'while-n1 "continue")
                  (/= old-point (point))
                  continue)
        (setq old-point (point))
        ;; If mumamo (or something similar) is used then jump over parts
        ;; that can not be parsed by nxml-mode.
        (when (and rng-get-major-mode-chunk-function
                   rng-valid-nxml-major-mode-chunk-function
                   rng-end-major-mode-chunk-function)
          (let ((here (point))
                next-non-space-pos)
            (skip-chars-forward " \t\r\n")
            (setq next-non-space-pos (point))
            (goto-char here)
            (unless (and end-major-mode-chunk
                         ;; Remaining chars in this chunk?
                         (< next-non-space-pos end-major-mode-chunk))
              (setq end-major-mode-chunk nil)
              (setq major-mode-chunk (funcall rng-get-major-mode-chunk-function next-non-space-pos "rng-do-some-validation-1 A"))
              (setq while-n2 0)
              (while (and (mumamo-while 500 'while-n2 "major-mode-chunk")
                          major-mode-chunk
                          (not (funcall rng-valid-nxml-major-mode-chunk-function major-mode-chunk))
                          (< next-non-space-pos (point-max)))
                ;;(msgtrc "next-non-space-pos=%s, cb=%s" next-non-space-pos (current-buffer))
                (let ((end-pos (funcall rng-end-major-mode-chunk-function major-mode-chunk)))
                  ;; fix-me: The problem here is that
                  ;; mumamo-find-chunks can return a 0-length chunk.
                  ;;(goto-char (+ end-pos 0))
                  (goto-char (+ end-pos (if (= end-pos (point)) 1 0)))
                  (setq major-mode-chunk (funcall rng-get-major-mode-chunk-function (point) "rng-do-some-validation-1 B"))
                  ;;(message "---> here 3, point=%s, ep=%s, mm-chunk=%s" (point) end-pos major-mode-chunk)
                  )
                (setq next-non-space-pos (point))))
            ;; Stop parsing if we do not have a chunk here yet.
            ;;(message "major-mode-chunk=%s" major-mode-chunk)
            ;;(message "rng-valid-nxml-major-mode-chunk-function=%s" rng-valid-nxml-major-mode-chunk-function)
            (setq continue (and major-mode-chunk
                                (funcall rng-valid-nxml-major-mode-chunk-function major-mode-chunk)))
            ;;(unless continue (message "continue=nil, no major-mode-chunk"))
            (when continue
              ;;(message "  continue=t")
              (setq end-major-mode-chunk (funcall rng-end-major-mode-chunk-function major-mode-chunk)))))

        (when continue
          ;; Narrow since rng-forward will continue into next chunk
          ;; even if limit is at chunk end.
          (if t
              (progn
                ;;(message "before rng-forward, point=%s" (point))
                (setq have-remaining-chars (rng-forward end-major-mode-chunk))
                ;;(message "after  rng-forward, point=%s" (point))
                )
            ;; Fix-me: Validation does not work when narrowing because
            ;; some state variables values seems to be lost. Probably
            ;; looking at `rng-validate-prepare' will tell what to do.
            (save-restriction
              (when (and end-major-mode-chunk
                         (< (point-min) end-major-mode-chunk))
                (narrow-to-region (point-min) end-major-mode-chunk))
              (setq have-remaining-chars (rng-forward end-major-mode-chunk)))
            (unless (> end-major-mode-chunk (point))
              ;;(setq have-remaining-chars t)
              (goto-char end-major-mode-chunk))
            )
          ;;(message "end-major-mode-chunk=%s, rng-validate-up-to-date-end=%s" end-major-mode-chunk rng-validate-up-to-date-end)
          (setq have-remaining-chars (< (point) point-max))
          ;;(unless have-remaining-chars (message "*** here have-remaining-chars=%s, p=%s/%s" have-remaining-chars (point) point-max))
          (let ((pos (point)))
            (when end-major-mode-chunk
              ;; Fix-me: Seems like we need a new initialization (or why
              ;; do we otherwise hang without this?)
              (and (> limit end-major-mode-chunk) (setq limit end-major-mode-chunk)))
            (setq continue
                  (and have-remaining-chars
                       continue
                       (or (< pos limit)
                           (and continue-p-function
                                (funcall continue-p-function)
                                (setq limit (+ limit rng-validate-chunk-size))
                                t))))
            ;;(unless continue (message "continue=nil, why?: %s<%s, %s" pos limit (when continue-p-function (funcall continue-p-function))))
            (cond ((and rng-conditional-up-to-date-start
                        ;; > because we are getting the state from (1- pos)
                        (> pos rng-conditional-up-to-date-start)
                        (< pos rng-conditional-up-to-date-end)
                        (rng-state-matches-current (get-text-property (1- pos)
                                                                      'rng-state)))
                   (when (< remove-start (1- pos))
                     (rng-clear-cached-state remove-start (1- pos)))
                   ;; sync up with cached validation state
                   (setq continue nil)
                   ;; do this before settting rng-validate-up-to-date-end
                   ;; in case we get a quit
                   (rng-mark-xmltok-errors)
                   (rng-mark-xmltok-dependent-regions)
                   (setq rng-validate-up-to-date-end
                         (marker-position rng-conditional-up-to-date-end))
                   (rng-clear-conditional-region)
                   (setq have-remaining-chars
                         (< rng-validate-up-to-date-end point-max))
                   ;;(unless have-remaining-chars (message "have-remaining-chars=%s rng-validate-up-to-date-end=%s, point-max=%s" have-remaining-chars rng-validate-up-to-date-end point-max))
                   )
                  ((or (>= pos next-cache-point)
                       (not continue))
                   (setq next-cache-point (+ pos rng-state-cache-distance))
                   (rng-clear-cached-state remove-start pos)
                   (when have-remaining-chars
                     ;;(message "rng-cach-state (1- %s)" pos)
                     (rng-cache-state (1- pos)))
                   (setq remove-start pos)
                   (unless continue
                     ;; if we have just blank chars skip to the end
                     (when have-remaining-chars
                       (skip-chars-forward " \t\r\n")
                       (when (= (point) point-max)
                         (rng-clear-overlays pos (point))
                         (rng-clear-cached-state pos (point))
                         (setq have-remaining-chars nil)
                         ;;(message "have-remaining-chars => nil, cause (point) = point-max")
                         (setq pos (point))))
                     (when (not have-remaining-chars)
                       (rng-process-end-document))
                     (rng-mark-xmltok-errors)
                     (rng-mark-xmltok-dependent-regions)
                     (setq rng-validate-up-to-date-end pos)
                     (when rng-conditional-up-to-date-end
                       (cond ((<= rng-conditional-up-to-date-end pos)
                              (rng-clear-conditional-region))
                             ((< rng-conditional-up-to-date-start pos)
                              (set-marker rng-conditional-up-to-date-start
                                          pos))))))))))
      ;;(message "--- exit rng-do-some-validation-1, have-remaining-chars=%s" have-remaining-chars)
      (setq have-remaining-chars (< (point) point-max))
      (setq ad-return-value have-remaining-chars))))

(defadvice rng-after-change-function (around
                                      mumamo-ad-rng-after-change-function
                                      activate
                                      compile)
  (when rng-validate-up-to-date-end
    ad-do-it))

(defadvice rng-validate-while-idle (around
                                    mumamo-ad-rng-validate-while-idle
                                    activate
                                    compile)
  (if (not (buffer-live-p buffer))
      (rng-kill-timers)
    ad-do-it))

(defadvice rng-validate-quick-while-idle (around
                                          mumamo-ad-rng-validate-quick-while-idle
                                          activate
                                          compile)
  (if (not (buffer-live-p buffer))
      (rng-kill-timers)
    ad-do-it))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; xmltok.el

;; (ad-disable-advice 'xmltok-add-error 'around 'mumamo-ad-xmltok-add-error)
;; (ad-ensable-advice 'xmltok-add-error 'around 'mumamo-ad-xmltok-add-error)
(defadvice xmltok-add-error (around
                             mumamo-ad-xmltok-add-error
                             activate
                             compile
                             )
  "Prevent rng validation errors in non-xml chunks.
This advice only prevents adding nxml/rng-valid errors in non-xml
chunks.  Doing more seems like a very big job - unless Emacs gets
a narrow-to-multiple-regions function!"
  (if (not mumamo-multi-major-mode)
      ad-do-it
    ;;(error "xmltok-add-error: %S" (with-output-to-string (backtrace)))
    (when (let* ((start (or start xmltok-start))
                 (end (or end (point)))
                 (chunk (mumamo-find-chunks (if start start end) "xmltok-add-error"))
                 )
            (or (not chunk)
                (and (not (overlay-get chunk 'mumamo-region))
                     (mumamo-valid-nxml-chunk chunk))))
      (setq xmltok-errors
            (cons (xmltok-make-error message
                                     (or start xmltok-start)
                                     (or end (point)))
                  xmltok-errors)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Maybe activate advices

;; Fix-me: This assumes there are no other advices on these functions.
(if t
    (progn
      ;; (ad-activate 'syntax-ppss)
      ;; (ad-activate 'syntax-ppss-flush-cache)
      ;; (ad-activate 'syntax-ppss-stats)
      ;; (ad-activate 'rng-do-some-validation-1)
      ;; (ad-activate 'rng-mark-error)
      ;; (ad-activate 'xmltok-add-error)
      (ad-enable-advice 'syntax-ppss 'around 'mumamo-ad-syntax-ppss)
      (ad-enable-advice 'syntax-ppss-flush-cache 'around 'mumamo-ad-syntax-ppss-flush-cache)
      (ad-enable-advice 'syntax-ppss-stats 'around 'mumamo-ad-syntax-ppss-stats)
      (ad-enable-advice 'rng-do-some-validation-1 'around 'mumamo-ad-rng-do-some-validation-1)
      (ad-enable-advice 'rng-mark-error 'around 'mumamo-ad-rng-mark-error)
      (ad-enable-advice 'rng-after-change-function 'around 'mumamo-ad-rng-after-change-function)
      (ad-enable-advice 'rng-validate-while-idle 'around 'mumamo-ad-rng-validate-while-idle)
      (ad-enable-advice 'rng-validate-quick-while-idle 'around 'mumamo-ad-rng-validate-quick-while-idle)
      (ad-enable-advice 'xmltok-add-error 'around 'mumamo-ad-xmltok-add-error)
      )
  ;; (ad-deactivate 'syntax-ppss)
  ;; (ad-deactivate 'syntax-ppss-flush-cache)
  ;; (ad-deactivate 'syntax-ppss-stats)
  ;; (ad-deactivate 'rng-do-some-validation-1)
  ;; (ad-deactivate 'rng-mark-error)
  ;; (ad-deactivate 'xmltok-add-error)
  (ad-disable-advice 'syntax-ppss 'around 'mumamo-ad-syntax-ppss)
  (ad-disable-advice 'syntax-ppss-flush-cache 'around 'mumamo-ad-syntax-ppss-flush-cache)
  (ad-disable-advice 'syntax-ppss-stats 'around 'mumamo-ad-syntax-ppss-stats)
  (ad-disable-advice 'rng-do-some-validation-1 'around 'mumamo-ad-rng-do-some-validation-1)
  (ad-disable-advice 'rng-mark-error 'around 'mumamo-ad-rng-mark-error)
  (ad-disable-advice 'rng-after-change-function 'around 'mumamo-ad-rng-after-change-function)
  (ad-disable-advice 'rng-validate-while-idle 'around 'mumamo-ad-rng-validate-while-idle)
  (ad-disable-advice 'rng-validate-quick-while-idle 'around 'mumamo-ad-rng-validate-quick-while-idle)
  (ad-disable-advice 'xmltok-add-error 'around 'mumamo-ad-xmltok-add-error)
  )

(font-lock-add-keywords
 'emacs-lisp-mode
 '(("\\<define-mumamo-multi-major-mode\\>" . font-lock-keyword-face)))



;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simple defadvice to move into Emacs later

(defun mumamo-ad-desktop-buffer-info (buffer)
  (set-buffer buffer)
  (list
   ;; base name of the buffer; replaces the buffer name if managed by uniquify
   (and (fboundp 'uniquify-buffer-base-name) (uniquify-buffer-base-name))
   ;; basic information
   (desktop-file-name (buffer-file-name) desktop-dirname)
   (buffer-name)
   (if mumamo-multi-major-mode mumamo-multi-major-mode major-mode)
   ;; minor modes
   (let (ret)
     (mapc
      #'(lambda (minor-mode)
	  (and (boundp minor-mode)
	       (symbol-value minor-mode)
	       (let* ((special (assq minor-mode desktop-minor-mode-table))
		      (value (cond (special (cadr special))
				   ((functionp minor-mode) minor-mode))))
		 (when value (add-to-list 'ret value)))))
      (mapcar #'car minor-mode-alist))
     ret)
   ;; point and mark, and read-only status
   (point)
   (list (mark t) mark-active)
   buffer-read-only
   ;; auxiliary information
   (when (functionp desktop-save-buffer)
     (funcall desktop-save-buffer desktop-dirname))
   ;; local variables
   (let ((locals desktop-locals-to-save)
	 (loclist (buffer-local-variables))
	 (ll))
     (while locals
       (let ((here (assq (car locals) loclist)))
	 (if here
	     (setq ll (cons here ll))
	   (when (member (car locals) loclist)
	     (setq ll (cons (car locals) ll)))))
       (setq locals (cdr locals)))
     ll)))

(defadvice desktop-buffer-info (around
                                mumamo-ad-desktop-buffer-info
                                activate
                                compile)
  (setq ad-return-value (mumamo-ad-desktop-buffer-info (ad-get-arg 0))))

(defun mumamo-ad-set-auto-mode-0 (mode &optional keep-mode-if-same)
  "Apply MODE and return it.
If optional arg KEEP-MODE-IF-SAME is non-nil, MODE is chased of
any aliases and compared to current major mode.  If they are the
same, do nothing and return nil."
  (unless (and keep-mode-if-same
	       (eq (indirect-function mode)
                   (if mumamo-multi-major-mode
                       (indirect-function mumamo-multi-major-mode)
                     (indirect-function major-mode))))
    (when mode
      (funcall mode)
      mode)))

(defadvice set-auto-mode-0 (around
                            mumamo-ad-set-auto-mode-0
                            activate
                            compile)
  (setq ad-return-value (mumamo-ad-set-auto-mode-0 (ad-get-arg 0)
                                                   (ad-get-arg 1)
                                                   )))



(defvar mumamo-sgml-get-context-last-close nil
  "Last close tag start.
Only used for outermost level.")

(defun mumamo-sgml-get-context (&optional until)
  "Determine the context of the current position.
By default, parse until we find a start-tag as the first thing on a line.
If UNTIL is `empty', return even if the context is empty (i.e.
we just skipped over some element and got to a beginning of line).

The context is a list of tag-info structures.  The last one is the tag
immediately enclosing the current position.

Point is assumed to be outside of any tag.  If we discover that it's
not the case, the first tag returned is the one inside which we are."
  (let ((here (point))
	(stack nil)
	(ignore nil)
	(context nil)
	tag-info
        last-close)
    ;; CONTEXT keeps track of the tag-stack
    ;; STACK keeps track of the end tags we've seen (and thus the start-tags
    ;;   we'll have to ignore) when skipping over matching open..close pairs.
    ;; IGNORE is a list of tags that can be ignored because they have been
    ;;   closed implicitly.
    ;; LAST-CLOSE is last close tag that can be useful for indentation
    ;;   when on outermost level.
    (skip-chars-backward " \t\n")      ; Make sure we're not at indentation.
    (while
	(and (not (eq until 'now))
	     (or stack
		 (not (if until (eq until 'empty) context))
		 (not (sgml-at-indentation-p))
		 (and context
		      (/= (point) (sgml-tag-start (car context)))
		      (sgml-unclosed-tag-p (sgml-tag-name (car context)))))
	     (setq tag-info (ignore-errors (sgml-parse-tag-backward))))

      ;; This tag may enclose things we thought were tags.  If so,
      ;; discard them.
      (while (and context
                  (> (sgml-tag-end tag-info)
                     (sgml-tag-end (car context))))
        (setq context (cdr context)))

      (cond
       ((> (sgml-tag-end tag-info) here)
	;; Oops!!  Looks like we were not outside of any tag, after all.
	(push tag-info context)
	(setq until 'now))

       ;; start-tag
       ((eq (sgml-tag-type tag-info) 'open)
        (when (and (null stack)
                   last-close)
          (setq last-close 'no-use))
	(cond
	 ((null stack)
	  (if (assoc-string (sgml-tag-name tag-info) ignore t)
	      ;; There was an implicit end-tag.
	      nil
	    (push tag-info context)
	    ;; We're changing context so the tags implicitly closed inside
	    ;; the previous context aren't implicitly closed here any more.
	    ;; [ Well, actually it depends, but we don't have the info about
	    ;; when it doesn't and when it does.   --Stef ]
	    (setq ignore nil)))
	 ((eq t (compare-strings (sgml-tag-name tag-info) nil nil
				 (car stack) nil nil t))
	  (setq stack (cdr stack)))
	 (t
	  ;; The open and close tags don't match.
	  (if (not sgml-xml-mode)
	      (unless (sgml-unclosed-tag-p (sgml-tag-name tag-info))
		(message "Unclosed tag <%s>" (sgml-tag-name tag-info))
		(let ((tmp stack))
		  ;; We could just assume that the tag is simply not closed
		  ;; but it's a bad assumption when tags *are* closed but
		  ;; not properly nested.
		  (while (and (cdr tmp)
			      (not (eq t (compare-strings
					  (sgml-tag-name tag-info) nil nil
					  (cadr tmp) nil nil t))))
		    (setq tmp (cdr tmp)))
		  (if (cdr tmp) (setcdr tmp (cddr tmp)))))
	    (message "Unmatched tags <%s> and </%s>"
		     (sgml-tag-name tag-info) (pop stack)))))

	(if (and (null stack) (sgml-unclosed-tag-p (sgml-tag-name tag-info)))
	    ;; This is a top-level open of an implicitly closed tag, so any
	    ;; occurrence of such an open tag at the same level can be ignored
	    ;; because it's been implicitly closed.
	    (push (sgml-tag-name tag-info) ignore)))

       ;; end-tag
       ((eq (sgml-tag-type tag-info) 'close)
	(if (sgml-empty-tag-p (sgml-tag-name tag-info))
	    (message "Spurious </%s>: empty tag" (sgml-tag-name tag-info))
          ;; Keep track of last close if context will return nil
          (when (and (not last-close)
                     (null stack)
                     (> here (point-at-eol))
                     (let ((here (point)))
                       (goto-char (sgml-tag-start tag-info))
                       (skip-chars-backward " \t")
                       (prog1
                           (bolp)
                         (goto-char here))))
            (setq last-close tag-info))

	  (push (sgml-tag-name tag-info) stack)))
       ))

    ;; return context
    (setq mumamo-sgml-get-context-last-close
          (when (and last-close
                     (not (eq last-close 'no-use)))
            (sgml-tag-start last-close)))
    context))

(defadvice sgml-get-context (around
                             mumamo-ad-sgml-get-context
                             activate
                             compile)
  (setq ad-return-value (mumamo-sgml-get-context (ad-get-arg 0))))

(defun mumamo-sgml-calculate-indent (&optional lcon)
  "Calculate the column to which this line should be indented.
LCON is the lexical context, if any."
  (unless lcon (setq lcon (sgml-lexical-context)))

  ;; Indent comment-start markers inside <!-- just like comment-end markers.
  (if (and (eq (car lcon) 'tag)
	   (looking-at "--")
	   (save-excursion (goto-char (cdr lcon)) (looking-at "<!--")))
      (setq lcon (cons 'comment (+ (cdr lcon) 2))))

  (case (car lcon)

    (string
     ;; Go back to previous non-empty line.
     (while (and (> (point) (cdr lcon))
		 (zerop (forward-line -1))
		 (looking-at "[ \t]*$")))
     (if (> (point) (cdr lcon))
	 ;; Previous line is inside the string.
	 (current-indentation)
       (goto-char (cdr lcon))
       (1+ (current-column))))

    (comment
     (let ((mark (looking-at "--")))
       ;; Go back to previous non-empty line.
       (while (and (> (point) (cdr lcon))
		   (zerop (forward-line -1))
		   (or (looking-at "[ \t]*$")
		       (if mark (not (looking-at "[ \t]*--"))))))
       (if (> (point) (cdr lcon))
	   ;; Previous line is inside the comment.
	   (skip-chars-forward " \t")
	 (goto-char (cdr lcon))
	 ;; Skip `<!' to get to the `--' with which we want to align.
	 (search-forward "--")
	 (goto-char (match-beginning 0)))
       (when (and (not mark) (looking-at "--"))
	 (forward-char 2) (skip-chars-forward " \t"))
       (current-column)))

    ;; We don't know how to indent it.  Let's be honest about it.
    (cdata nil)
    ;; We don't know how to indent it.  Let's be honest about it.
    (pi nil)

    (tag
     (goto-char (1+ (cdr lcon)))
     (skip-chars-forward "^ \t\n")	;Skip tag name.
     (skip-chars-forward " \t")
     (if (not (eolp))
	 (current-column)
       ;; This is the first attribute: indent.
       (goto-char (1+ (cdr lcon)))
       (+ (current-column) sgml-basic-offset)))

    (text
     (while (looking-at "</")
       (forward-sexp 1)
       (skip-chars-forward " \t"))
     (let* ((here (point))
	    (unclosed (and ;; (not sgml-xml-mode)
		       (looking-at sgml-tag-name-re)
		       (assoc-string (match-string 1)
				     sgml-unclosed-tags 'ignore-case)
		       (match-string 1)))
	    (context
	     ;; If possible, align on the previous non-empty text line.
	     ;; Otherwise, do a more serious parsing to find the
	     ;; tag(s) relative to which we should be indenting.
	     (if (and (not unclosed) (skip-chars-backward " \t")
		      (< (skip-chars-backward " \t\n") 0)
		      (back-to-indentation)
		      (> (point) (cdr lcon)))
		 nil
	       (goto-char here)
	       (nreverse (sgml-get-context (if unclosed nil 'empty)))))
	    (there (point)))
       ;; Ignore previous unclosed start-tag in context.
       (while (and context unclosed
		   (eq t (compare-strings
			  (sgml-tag-name (car context)) nil nil
			  unclosed nil nil t)))
	 (setq context (cdr context)))
       ;; Indent to reflect nesting.
       (cond
	;; If we were not in a text context after all, let's try again.
	((and context (> (sgml-tag-end (car context)) here))
	 (goto-char here)
	 (sgml-calculate-indent
	  (cons (if (memq (sgml-tag-type (car context)) '(comment cdata))
		    (sgml-tag-type (car context)) 'tag)
		(sgml-tag-start (car context)))))
	;; Align on the first element after the nearest open-tag, if any.
	((and context
	      (goto-char (sgml-tag-end (car context)))
	      (skip-chars-forward " \t\n")
	      (< (point) here) (sgml-at-indentation-p))
	 (current-column))
	(t
	 (goto-char (or (and (null context)
                             mumamo-sgml-get-context-last-close)
                        there))
	 (+ (current-column)
	    (* sgml-basic-offset (length context)))))))

    (otherwise
     (error "Unrecognized context %s" (car lcon)))

    ))

(defadvice sgml-calculate-indent (around
                                  mumamo-ad-sgml-calculate-indent
                                  activate
                                  compile)
  (setq ad-return-value (mumamo-sgml-calculate-indent (ad-get-arg 0))))

(defadvice python-eldoc-function (around
                                  mumamo-ad-python-eldoc-function
                                  activate
                                  compile)
  (if (not mumamo-multi-major-mode)
      ad-do-it
    (let ((here (point)))
      (unwind-protect
          (save-restriction
            (mumamo-narrow-to-chunk-inner)
            ad-do-it)
        (goto-char here)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The END
;;(when buffer-file-name (message "Finished evaluating %s" buffer-file-name))
;;(when load-file-name (message "Finished loading %s" load-file-name))

(provide 'mumamo)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mumamo.el ends bere
