;; Copyright (C) 2015-2017 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>

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
;; tracks shell output

(require 'shell)

(require 'load-relative)
(require-relative-list
 '("core"   "helper" "track" "loc" "lochist" "file"
   "fringe" "window" "regexp" "menu" "backtrace-mode"
   "send"   "shortkey" "utils") "realgud-")

(require-relative-list  '("buffer/command") "realgud-buffer-")

;; FIXME figure out if I can put this in something like a header file.
(declare-function realgud-fringe-erase-history-arrows 'realgud-buffer-command)
(declare-function realgud:track-set-debugger          'realgud-track)
(declare-function realgud-populate-debugger-menu      'realgud-menu)
(declare-function realgud-cmdbuf-info-divert-output?=
		  'realgud-buffer-command)
(declare-function realgud-cmdbuf-info-prior-prompt-regexp=
		  'realgud-buffer-command)
(declare-function realgud-cmdbuf-info-set?
		  'realgud-buffer-command)
(declare-function realgud:canonic-major-mode
		  'realgud-utils)
(declare-function shell-mode 'shell)

(defvar realgud-track-mode-map
  (let ((map  (copy-keymap shell-mode-map)))
    (realgud-populate-debugger-menu map)
    (define-key map "\r"	'realgud:send-input)
    (define-key map [M-right]	'realgud-track-hist-newest)
    (define-key map [M-down]	'realgud-track-hist-newer)
    (define-key map [M-up]	'realgud-track-hist-older)
    (define-key map [M-print]	'realgud-track-hist-older)
    (define-key map [M-S-down]	'realgud-track-hist-newest)
    (define-key map [M-S-up]	'realgud-track-hist-oldest)
    (define-key map "\C-cS" 'realgud-window-src-undisturb-cmd)
    (define-key map (kbd "C-c !d") 'realgud:goto-debugger-loc-line)
    map)
  "Keymap used in `realgud-track-minor-mode'.

\\{realgud-track-mode-map}")


(defvar realgud:tool-bar-map
  (let ((map (make-sparse-keymap)))
    (dolist (x '((realgud:cmd-break . "gud/break")
		 ;; (realgud:cmd-remove . "gud/remove")
		 ;; (realgud:cmd-print . "gud/print")
		 ;; (realgud:cmd-pstar . "gud/pstar")
		 ;; (realgud:cmd-pp . "gud/pp")
		 ;; (realgud:cmd-watch . "gud/watch")
		 (realgud:cmd-restart . "gud/run")
		 ;; (realgud:cmd-go . "gud/go")
		 ;; (realgud:cmd-stop-subjob . "gud/stop")
		 (realgud:cmd-continue . "gud/cont")
		 (realgud:cmd-until . "gud/until")
		 (realgud:cmd-next . "gud/next")
		 (realgud:cmd-step . "gud/step")
		 (realgud:cmd-finish . "gud/finish")
		 ;; (realgud:cmd-nexti . "gud/nexti")
		 ;; (realgud:cmd-stepi . "gud/stepi")
		 (realgud:cmd-older-frame . "gud/up")
		 (realgud:cmd-newer-frame . "gud/down")
		 (realgud:cmdbuf-info-describe . "info"))
	       map)
      (tool-bar-local-item-from-menu
       (car x) (cdr x) map realgud-track-mode-map)))
  "toolbar use when `realgud' interface is active"
  )

(define-minor-mode realgud-track-mode
  "Minor mode for tracking debugging inside a process shell."
  :init-value nil
  :global nil
  :group 'realgud

  :lighter
  (:eval (progn
	   (concat " "
		   (if (realgud-cmdbuf-info-set?)
		       (realgud-sget 'cmdbuf-info 'debugger-name)
		     "dbgr??"))))

  :keymap realgud-track-mode-map
  ;; Setup/teardown
  (realgud-track-mode-setup realgud-track-mode)
  )

;; FIXME: this should have been picked up by require'ing track.
(defvar realgud-track-divert-string)

(defun realgud-track-mode-setup (mode-on?)
  "Called when entering or leaving `realgud-track-mode'. Variable
MODE-ON is a boolean which specifies if we are going into or out
of this mode."
  (if mode-on?
      (let ((process (get-buffer-process (current-buffer))))
	(unless process
	  (setq realgud-track-mode nil)
	  (error "Can't find a process for buffer %s" (current-buffer)))

	(setq realgud-track-divert-string "")
	(setq realgud-track-mode 't)

	;; FIXME: save and chain process-sentinel via
	;; (process-sentinel (get-buffer-process (current-buffer)))
	(set-process-sentinel process 'realgud-term-sentinel)
	(unless (and (realgud-cmdbuf-info-set?)
		     (realgud-sget 'cmdbuf-info 'debugger-name))
	  (call-interactively 'realgud:track-set-debugger))
	(if (boundp 'comint-last-output-start)
	    (progn
	      (realgud-cmdbuf-info-prior-prompt-regexp= comint-prompt-regexp)
	      (realgud-cmdbuf-info-divert-output?= nil)
	      (let* ((regexp-hash
		      (and (realgud-cmdbuf-info? realgud-cmdbuf-info)
			   (realgud-sget 'cmdbuf-info 'regexp-hash)))
		     (prompt-pat (and regexp-hash
				      (gethash "prompt" regexp-hash))))
		(if prompt-pat
		    (setq comint-prompt-regexp
			    (realgud-loc-pat-regexp prompt-pat)))))
	  (set-marker comint-last-output-start (point)))

	(set (make-local-variable 'tool-bar-map) realgud:tool-bar-map)
	(let ((mode (realgud:canonic-major-mode)))
	  (cond ((eq mode 'eshell)
		 (add-hook 'eshell-output-filter-functions
			   'realgud-track-eshell-output-filter-hook))
		((eq mode 'comint)
		 (add-hook 'comint-output-filter-functions
			   'realgud-track-comint-output-filter-hook))
		))
	(run-mode-hooks 'realgud-track-mode-hook))
  ;; else
    (progn
      (if (and (boundp 'comint-last-output-start) realgud-cmdbuf-info)
	(setq comint-prompt-regexp
	   (realgud-sget 'cmdbuf-info 'prior-prompt-regexp))
	)
      (kill-local-variable 'realgud:tool-bar-map)
      (realgud-fringe-erase-history-arrows)
      (let ((mode (realgud:canonic-major-mode)))
	(cond ((eq mode 'eshell)
	       (remove-hook 'eshell-output-filter-functions
		    'realgud-track-eshell-output-filter-hook))
	      ((eq mode 'comint)
	       (remove-hook 'comint-output-filter-functions
			    'realgud-track-comint-output-filter-hook))
	      ))
      (let* ((cmd-process (get-buffer-process (current-buffer)))
	     (status (if cmd-process
			 (list (propertize (format ":%s"
						   (process-status cmd-process))
			    'face 'debugger-running))
		       ""))
	     )
	(setq mode-line-process status)
	;; Force mode line redisplay soon.
	(force-mode-line-update)
	;; FIXME: This is a workaround. Without this, we comint doesn't
	;; process commands
	(unless (member 'comint-mode minor-mode-list) (comint-mode))
	)

      ;; FIXME: restore/unchain old process sentinels.
      )
    )
  )

;; For name == "trepan", produces:
;;   (defvar trepan-track-mode nil
;;     "Non-nil if using trepan track-mode ... "
;;   (defvar trepan-track-mode-map (make-sparse-keymap))
;;   (defvar trepan-short-key-mode-map (make-sparse-keymap))
;;   (set-keymap-parent trepan-short-key-mode-map realgud-short-key-mode-map)
(defmacro realgud-track-mode-vars (name)
  `(progn
     (defvar ,(intern (concat name "-track-mode")) nil
	,(format "Non-nil if using %s-track-mode as a minor mode of some other mode.
Use the command `%s-track-mode' to toggle or set this variable." name name))
     (defvar ,(intern (concat name "-track-mode-map")) (make-sparse-keymap)
       ,(format "Keymap used in `%s-track-mode'." name))
     (defvar ,(intern (concat name "-short-key-mode-map")) (make-sparse-keymap))
    ))

;; FIXME: The below could be a macro? I have a hard time getting
;; macros right.
(defun realgud-track-mode-body(name)
  "Used in by custom debuggers: pydbgr, trepan, gdb, etc. NAME is
the name of the debugger which is used to preface variables."
  (realgud:track-set-debugger name)
  (funcall (intern (concat "realgud-define-" name "-commands")))
  (if (intern (concat name "-track-mode"))
      (progn
	(setq realgud-track-mode 't)
	(run-mode-hooks (intern (concat name "-track-mode-hook"))))
    (progn
      (setq realgud-track-mode nil)
      )))

(defun realgud:track-mode-disable()
  "Disable the debugger track-mode hook"
  (interactive "")
  (if realgud-track-mode
      (progn
	(setq realgud-track-mode nil)
	;; FIXME: for some reason, disabling trak mode also
	;; disables shell mode. Reinitialize it?
	(if (equal mode-name "Shell")
	    (shell-mode))
	)
    (message "Debugger is not in track mode")))

(defun realgud:track-mode-enable()
  "Enable the debugger track-mode hook"
  (interactive "")
  (if realgud-track-mode
      (message "Debugger track mode is already enabled.")
    (setq realgud-track-mode t))
  )

(provide-me "realgud-")
