;;; hmouse-sh.el --- System-dependent Smart Mouse Key bindings.
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:     3-Sep-91 at 21:40:58
;;
;; Copyright (C) 1991-2016  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;   See description in "hmouse-key.el".

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'hvar)

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;; Other mouse event location prefixes to possibly handle:
;;   header-line
;;   vertical-scroll-bar
;;   horizontal-scroll-bar
;;   right-divider
;;   bottom-divider

(defun hmouse-get-bindings (hmouse-middle-flag)
  "Returns the list of current bindings of mouse keys used by Hyperbole.
If HMOUSE-MIDDLE-FLAG is non-nil, includes the middle mouse key binding as well.
These may be the bindings prior to initializing Hyperbole or the Hyperbole bindings."
  ;; Do nothing when running in batch mode.
  (unless noninteractive
    (nconc
     (if hmouse-middle-flag (hmouse-get-unshifted-bindings))
     (eval
      (cdr (assoc
	    ;; Get mouse bindings under Emacs or XEmacs, even if not under a
	    ;; window system since they can have frames on ttys and windowed
	    ;; displays at the same time.
	    (or (and (featurep 'xemacs) "xemacs")
		(and hyperb:emacs-p "emacs")
		(hyperb:window-system))
	    '(("emacs" .
	       (mapcar (lambda (key) (cons key (global-key-binding key)))
		       (if (eq window-system 'dps)
			   ;; NEXTSTEP offers only 2 shift-mouse buttons which we use
			   ;; as the Smart Keys.
			   '([S-down-mouse-1] [S-drag-mouse-1] [S-mouse-1]
			     [S-down-mouse-2] [S-drag-mouse-2] [S-mouse-2]
			     [S-double-mouse-1] [S-triple-mouse-1]
			     [S-double-mouse-2] [S-triple-mouse-2]
			     [header-line S-down-mouse-1] [header-line S-drag-mouse-1]
			     [header-line S-mouse-1]
			     [header-line S-down-mouse-2] [header-line S-drag-mouse-2]
			     [header-line S-mouse-2]
			     [left-fringe S-down-mouse-1] [left-fringe S-drag-mouse-1]
			     [left-fringe S-mouse-1]
			     [left-fringe S-down-mouse-2] [left-fringe S-drag-mouse-2]
			     [left-fringe S-mouse-2]
			     [right-fringe S-down-mouse-1] [right-fringe S-drag-mouse-1]
			     [right-fringe S-mouse-1]
			     [right-fringe S-down-mouse-2] [right-fringe S-drag-mouse-2]
			     [right-fringe S-mouse-2]
			     [vertical-line S-down-mouse-1] [vertical-line S-drag-mouse-1]
			     [vertical-line S-mouse-1]
			     [vertical-line S-down-mouse-2] [vertical-line S-drag-mouse-2]
			     [vertical-line S-mouse-2]
			     [mode-line S-down-mouse-1] [mode-line S-drag-mouse-1]
			     [mode-line S-mouse-1]
			     [mode-line S-down-mouse-2] [mode-line S-drag-mouse-2]
			     [mode-line S-mouse-2]
			     )
			 ;; X, OS X or MS Windows
			 '([S-down-mouse-2] [S-drag-mouse-2] [S-mouse-2]
			   [S-down-mouse-3] [S-drag-mouse-3] [S-mouse-3]
			   [S-double-mouse-2] [S-triple-mouse-2]
			   [S-double-mouse-3] [S-triple-mouse-3]
			   [header-line S-down-mouse-2] [header-line S-drag-mouse-2]
			   [header-line S-mouse-2]
			   [header-line S-down-mouse-3] [header-line S-drag-mouse-3]
			   [header-line S-mouse-3]
			   [left-fringe S-down-mouse-2] [left-fringe S-drag-mouse-2]
			   [left-fringe S-mouse-2]
			   [left-fringe S-down-mouse-3] [left-fringe S-drag-mouse-3]
			   [left-fringe S-mouse-3]
			   [right-fringe S-down-mouse-2] [right-fringe S-drag-mouse-2]
			   [right-fringe S-mouse-2]
			   [right-fringe S-down-mouse-3] [right-fringe S-drag-mouse-3]
			   [right-fringe S-mouse-3]
			   [vertical-line S-down-mouse-2] [vertical-line S-drag-mouse-2]
			   [vertical-line S-mouse-2]
			   [vertical-line S-down-mouse-3] [vertical-line S-drag-mouse-3]
			   [vertical-line S-mouse-3]
			   [mode-line S-down-mouse-2] [mode-line S-drag-mouse-2]
			   [mode-line S-mouse-2]
			   [mode-line S-down-mouse-3] [mode-line S-drag-mouse-3]
			   [mode-line S-mouse-3]
			   ))))
	      ("xemacs" .
	       (nconc
		(mapcar (lambda (key)
			  (cons key (global-key-binding key)))
			'([(shift button2)] [(shift button2up)]
			  [(shift button3)] [(shift button3up)]))
		(if (boundp 'mode-line-map)
		    (mapcar (lambda (key)
			      (cons key (lookup-key mode-line-map key)))
			    '([(shift button3)] [(shift button3up)])))))
	      ("xterm" .
	       (mapcar (lambda (key) (cons key (lookup-key mouse-map key)))
		       (list x-button-s-middle x-button-s-middle-up
			     x-button-s-right  x-button-s-right-up)))
	      ("next" .
	       (mapcar (lambda (key)
			 (cons key (mousemap-get
				    (mouse-list-to-mouse-code key)
				    current-global-mousemap)))
		       (apply 'nconc
			      (mapcar (lambda (region)
					(mapcar (function
						 (lambda (key)
						   (cons region key)))
						'((shift left)  (shift up left)
						  (shift right) (shift up right)
						  )))
				      '(text scrollbar modeline minibuffer)))
		       )))))))))

(defun hmouse-get-unshifted-bindings ()
  "Returns the list of middle mouse key bindings prior to their use as Smart Keys."
  ;; Do nothing when running in batch mode.
  (eval
   (cdr (assoc
	 ;; Get mouse bindings under Emacs or XEmacs, even if not under a
	 ;; window system since they can have frames on ttys and windowed
	 ;; displays at the same time.
	 (or (and (featurep 'xemacs) "xemacs")
	     (and hyperb:emacs-p "emacs")
	     (hyperb:window-system))
	 '(("emacs" .
	    (mapcar (lambda (key) (cons key (global-key-binding key)))
		    (if (not (eq window-system 'dps))
			;; X, OS X or MS Windows
			'([down-mouse-2] [drag-mouse-2] [mouse-2]
			  ;; [down-mouse-3] [drag-mouse-3] [mouse-3]
			  [double-mouse-2] [triple-mouse-2]
			  ;; [double-mouse-3] [triple-mouse-3]
			  [header-line down-mouse-2] [header-line drag-mouse-2]
			  [header-line mouse-2]
			  [left-fringe down-mouse-2] [left-fringe drag-mouse-2]
			  [left-fringe mouse-2]
			  [right-fringe down-mouse-2] [right-fringe drag-mouse-2]
			  [right-fringe mouse-2]
			  [vertical-line down-mouse-2] [vertical-line drag-mouse-2]
			  [vertical-line mouse-2]
			  ;; [left-fringe down-mouse-3] [left-fringe drag-mouse-3]
			  ;; [left-fringe mouse-3]
			  ;; [right-fringe down-mouse-3] [right-fringe drag-mouse-3]
			  ;; [right-fringe mouse-3]
			  ;; [vertical-line down-mouse-3] [vertical-line drag-mouse-3]
			  ;; [vertical-line mouse-3]
			  [mode-line down-mouse-2] [mode-line drag-mouse-2]
			  [mode-line mouse-2]
			  ;; [mode-line down-mouse-3] [mode-line drag-mouse-3]
			  ;; [mode-line mouse-3]
			  ))))
	   ("xemacs" .
	    (nconc
	     (mapcar (lambda (key)
		       (cons key (global-key-binding key)))
		     '([button2] [button2up]
		       ;; [button3] [button3up]
		       ))
	     ;; (if (boundp 'mode-line-map)
	     ;;   (mapcar (function
	     ;;	    (lambda (key)
	     ;;	      (cons key (lookup-key mode-line-map key))))
	     ;;	   '([button3] [button3up])))
	     ))
	   ("xterm" .
	    (mapcar (lambda (key) (cons key (lookup-key mouse-map key)))
		    (list x-button-middle x-button-middle-up
			  ;; x-button-right  x-button-right-up
			  )))
	   )))))

;; Based on functions from Emacs mouse.el.
(defun hmouse-move-point-emacs (event &optional promote-to-region)
  "Move point to the position clicked on with the mouse.
This should be bound to a mouse click event type.
If PROMOTE-TO-REGION is non-nil and event is a multiple-click,
select the corresponding element around point."
  (interactive "e\np")
  (let ((start-w-or-f (posn-window (event-start event)))
	(end-w-or-f   (posn-window (event-end event))))
    (if (framep start-w-or-f)
	(with-selected-frame start-w-or-f (setq start-w-or-f (selected-window))))
    (if (framep end-w-or-f)
	(with-selected-frame end-w-or-f (setq end-w-or-f (selected-window))))
    (if (and (window-minibuffer-p start-w-or-f)
	     (not (minibuffer-window-active-p start-w-or-f)))
	;; Select the ending frame only, not the window pressed within.
	(select-frame (window-frame end-w-or-f))
      ;; Give temporary modes such as isearch a chance to turn off.
      (run-hooks 'mouse-leave-buffer-hook)
      (if (and promote-to-region (> (event-click-count event) 1))
	  (mouse-set-region event)
	;; Use event-end in case called from mouse-drag-region.
	;; If EVENT is a click, event-end and event-start give same value.
	(if (and (window-minibuffer-p end-w-or-f)
		 (not (minibuffer-window-active-p end-w-or-f)))
	    ;; Select the ending frame only, not the window pressed within.
	    (select-frame (window-frame end-w-or-f))
	  (condition-case ()
	      (posn-set-point (event-end event))
	    (error (select-frame (window-frame end-w-or-f)))))))))

(defun hmouse-move-point-eterm (arg-list)
  (apply 'mouse-move-point arg-list))

(defun hmouse-move-point-xemacs ()
  (condition-case ()
      (mouse-set-point current-mouse-event)
    ;; Catch "not in a window" errors, e.g. on modeline
    (error nil)))

(defun hmouse-set-key-list (binding key-list)
  (mapc (lambda (key) (hkey-global-set-key key binding)) key-list)
  nil)

(defun hmouse-shifted-setup (hmouse-middle-flag)
  "Call `hmouse-install' instead of this and see its documentation."
  (interactive)
  ;; Do nothing when running in batch mode.
  (unless noninteractive
    (or hmouse-bindings-flag hmouse-previous-bindings
	(setq hmouse-previous-bindings (hmouse-get-bindings hmouse-middle-flag)))
    (if hmouse-middle-flag (hmouse-unshifted-setup hmouse-middle-flag))
    ;; Ensure Gillespie's Info mouse support is off since
    ;; Hyperbole handles that.
    (if (boundp 'Info-mouse-support) (setq Info-mouse-support nil))
    ;;
    ;; This event setting from the "kmacro.el" library can
    ;; trigger an autoload that binds [S-mouse-3] to 'kmacro-end-call-mouse,
    ;; interfering with the same key used for the Assist Key, so disable
    ;; this.
    (setq kmacro-call-mouse-event nil)
    ;;
    (cond
     ;; GNU Emacs
     (hyperb:emacs-p
      (setq hmouse-set-point-command 'hmouse-move-point-emacs)
      (if (eq window-system 'dps)
	  ;; NEXTSTEP offers only 2 shift-mouse buttons which we use
	  ;; as the Smart Keys.
	  (progn
	    (hmouse-set-key-list #'action-key-depress-emacs
				 '([S-down-mouse-1] [header-line S-down-mouse-1]
				   [left-fringe S-down-mouse-1]
				   [right-fringe S-down-mouse-1]
				   [vertical-line S-down-mouse-1]
				   [mode-line S-down-mouse-1]))
	    (hmouse-set-key-list #'action-mouse-key-emacs
				 '([S-drag-mouse-1] [S-mouse-1]
				   [S-double-mouse-1] [S-triple-mouse-1]
				   [header-line S-drag-mouse-1]
				   [header-line S-mouse-1]
				   [left-fringe S-drag-mouse-1]
				   [left-fringe S-mouse-1]
				   [right-fringe S-drag-mouse-1]
				   [right-fringe S-mouse-1]
				   [vertical-line S-drag-mouse-1]
				   [vertical-line S-mouse-1]
				   [mode-line S-drag-mouse-1]
				   [mode-line S-mouse-1]))

	    (hmouse-set-key-list #'assist-key-depress-emacs
				 '([S-down-mouse-2] [header-line S-down-mouse-2]
				   [left-fringe S-down-mouse-2]
				   [right-fringe S-down-mouse-2]
				   [vertical-line S-down-mouse-2]
				   [mode-line S-down-mouse-2]))
	    (hmouse-set-key-list #'assist-mouse-key-emacs
				 '([S-drag-mouse-2] [S-mouse-2]
				   [S-double-mouse-2] [S-triple-mouse-2]
				   [header-line S-drag-mouse-2]
				   [header-line S-mouse-2]
				   [left-fringe S-drag-mouse-2]
				   [left-fringe S-mouse-2]
				   [right-fringe S-drag-mouse-2]
				   [right-fringe S-mouse-2]
				   [vertical-line S-drag-mouse-2]
				   [vertical-line S-mouse-2]
				   [mode-line S-drag-mouse-2]
				   [mode-line S-mouse-2])))
	;; X, OS X or MS Windows
	(hmouse-set-key-list #'action-key-depress-emacs
			     '([S-down-mouse-2] [header-line S-down-mouse-2]
			       [left-fringe S-down-mouse-2]
			       [right-fringe S-down-mouse-2]
			       [vertical-line S-down-mouse-2]
			       [mode-line S-down-mouse-2]))
	(hmouse-set-key-list #'action-mouse-key-emacs
			     '([S-drag-mouse-2] [S-mouse-2]
			       [S-double-mouse-2] [S-triple-mouse-2]
			       [header-line S-drag-mouse-2]
			       [header-line S-mouse-2]
			       [left-fringe S-drag-mouse-2]
			       [left-fringe S-mouse-2]
			       [right-fringe S-drag-mouse-2]
			       [right-fringe S-mouse-2]
			       [vertical-line S-drag-mouse-2]
			       [vertical-line S-mouse-2]
			       [mode-line S-drag-mouse-2]
			       [mode-line S-mouse-2]))

	(hmouse-set-key-list #'assist-key-depress-emacs
			     '([S-down-mouse-3] [header-line S-down-mouse-3]
			       [left-fringe S-down-mouse-3]
			       [right-fringe S-down-mouse-3]
			       [vertical-line S-down-mouse-3]
			       [mode-line S-down-mouse-3]))
	(hmouse-set-key-list #'assist-mouse-key-emacs
			     '([S-drag-mouse-3] [S-mouse-3]
			       [S-double-mouse-3] [S-triple-mouse-3]
			       [header-line S-drag-mouse-3]
			       [left-fringe S-drag-mouse-3]
			       [left-fringe S-mouse-3]
			       [right-fringe S-drag-mouse-3]
			       [right-fringe S-mouse-3]
			       [header-line S-mouse-3]
			       [vertical-line S-drag-mouse-3]
			       [vertical-line S-mouse-3]
			       [mode-line S-drag-mouse-3]
			       [mode-line S-mouse-3]))))
     ;;
     ;; XEmacs
     ((featurep 'xemacs)
      ;; Set mouse bindings under XEmacs, even if not under a window
      ;; system since it can have frames on ttys and windowed displays at
      ;; the same time.
      (setq hmouse-set-point-command 'hmouse-move-point-xemacs)
      (global-set-key '(shift button2)     'action-key-depress)
      (global-set-key '(shift button2up)   'action-mouse-key)
      (if (fboundp 'infodock-set-mouse-bindings)
	  (infodock-set-mouse-bindings)
	(if (boundp 'mode-line-map)
	    (progn (define-key mode-line-map '(shift button3)
		     'assist-key-depress)
		   (define-key mode-line-map '(shift button3up)
		     'assist-mouse-key)
		   ))
	(global-set-key '(shift button3)     'assist-key-depress)
	(global-set-key '(shift button3up)   'assist-mouse-key)))
     ;;
     ;; X
     ((equal (hyperb:window-system) "xterm")
      (setq hmouse-set-point-command 'x-mouse-set-point)
      (define-key mouse-map x-button-s-middle 'action-key-depress)
      (define-key mouse-map x-button-s-middle-up 'action-mouse-key)
      (define-key mouse-map x-button-s-right 'assist-key-depress)
      (define-key mouse-map x-button-s-right-up 'assist-mouse-key)
      ;; Use these instead of the above for a true META-BUTTON binding.
      ;; (define-key mouse-map x-button-m-middle 'assist-key-depress)
      ;; (define-key mouse-map x-button-m-middle-up 'assist-mouse-key)
      )
     ;;
     ;; NeXT
     ((equal (hyperb:window-system) "next")
      (setq hmouse-set-point-command 'hmouse-move-point-eterm)
      ;; Use left button to set point.
      ;; Use shift-left button instead of non-existent middle as Action Key.
      (mapc
       (lambda (region)
	 (global-set-mouse (cons region '(shift left))     'action-key-depress)
	 (global-set-mouse (cons region '(shift up left))  'action-mouse-key)
	 (global-set-mouse (cons region '(shift right))    'assist-key-depress)
	 (global-set-mouse (cons region '(shift up right)) 'assist-mouse-key)
	 ;; Use these instead of the above for a true META-BUTTON binding.
	 ;; (global-set-mouse (cons region '(meta    right))  'assist-key-depress)
	 ;; (global-set-mouse (cons region '(meta up right))  'assist-mouse-key)
	 )
       '(text scrollbar modeline minibuffer))
      ))
    (setq hmouse-bindings (hmouse-get-bindings hmouse-middle-flag)
	  hmouse-bindings-flag t)))

(defun hmouse-unshifted-setup (&optional _hmouse-middle-flag)
  "Binds the middle mouse key to the Action Key."
  (interactive)
  (cond	;; GNU Emacs
   (hyperb:emacs-p
    ;; Get rid of Info-mode [mouse-2] binding since Hyperbole performs
    ;; a superset of what it does.
    (var:add-and-run-hook 'Info-mode-hook
			  (lambda () (define-key Info-mode-map [mouse-2] nil)))
    ;;
    (if (not (eq window-system 'dps))
	;; X, OS X or MS Windows
	(progn (hmouse-set-key-list #'action-key-depress-emacs
				    '([down-mouse-2] [header-line down-mouse-2]
				      [left-fringe down-mouse-2]
				      [right-fringe down-mouse-2]
				      [vertical-line down-mouse-2]
				      [mode-line down-mouse-2]))
	       (hmouse-set-key-list #'action-mouse-key-emacs
				    '([drag-mouse-2] [mouse-2]
				      [double-mouse-2] [triple-mouse-2]
				      [header-line drag-mouse-2]
				      [header-line mouse-2]
				      [left-fringe drag-mouse-2]
				      [left-fringe mouse-2]
				      [right-fringe drag-mouse-2]
				      [right-fringe mouse-2]
				      [vertical-line drag-mouse-2]
				      [vertical-line mouse-2]
				      [mode-line drag-mouse-2]
				      [mode-line mouse-2]))

	       ;; (hmouse-set-key-list #'assist-key-depress-emacs
	       ;; 		     '([down-mouse-3] [header-line down-mouse-3]
	       ;;		       [left-fringe down-mouse-3]
	       ;;		       [right-fringe down-mouse-3]
	       ;;                      [vertical-line down-mouse-3]
	       ;; 		       [mode-line down-mouse-3]))
	       ;; (hmouse-set-key-list #'assist-mouse-key-emacs
	       ;; 		     '([drag-mouse-3] [mouse-3]
	       ;; 		       [double-mouse-3] [triple-mouse-3]
	       ;; 		       [header-line drag-mouse-3]
	       ;; 		       [header-line mouse-3]
	       ;;		       [left-fringe drag-mouse-3]
	       ;;		       [left-fringe mouse-3]
	       ;;		       [right-fringe drag-mouse-3]
	       ;;		       [right-fringe mouse-3]
	       ;; 		       [vertical-line drag-mouse-3]
	       ;; 		       [vertical-line mouse-3]
	       ;; 		       [mode-line drag-mouse-3]
	       ;; 		       [mode-line mouse-3]))
	       )))
   ;;
   ;; XEmacs
   ((featurep 'xemacs)
    ;; Set mouse bindings under XEmacs, even if not under a window
    ;; system since it can have frames on ttys and windowed displays at
    ;; the same time.
    ;;
    ;; Get rid of Info-mode button 2 bindings since Hyperbole
    ;; handles things in Info.
    (var:add-and-run-hook 'Info-mode-hook
			  (lambda () (define-key Info-mode-map 'button2 nil)))
    ;;
    (global-set-key 'button2     'action-key-depress)
    (global-set-key 'button2up   'action-mouse-key)
    ;; (if (and (boundp 'Info-mode-map) (keymapp Info-mode-map))
    ;;    (funcall (lambda () (define-key Info-mode-map 'button3 nil)))
    ;;  (add-hook 'Info-mode-hook unbind-but3))
    ;; (if (boundp 'mode-line-map)
    ;;    (progn (define-key mode-line-map 'button3   'assist-key-depress)
    ;;	   (define-key mode-line-map 'button3up 'assist-mouse-key)))
    ;; (global-set-key 'button3     'assist-key-depress)
    ;; (global-set-key 'button3up   'assist-mouse-key)
    )
   ;;
   ;; X
   ((equal (hyperb:window-system) "xterm")
    (define-key mouse-map x-button-middle 'action-key-depress)
    (define-key mouse-map x-button-middle-up 'action-mouse-key)
    ;; (define-key mouse-map x-button-right 'assist-key-depress)
    ;; (define-key mouse-map x-button-right-up 'assist-mouse-key)
    ;; Use these instead of the above for a true META-BUTTON binding.
    ;; (define-key mouse-map x-button-m-middle 'assist-key-depress)
    ;; (define-key mouse-map x-button-m-middle-up 'assist-mouse-key)
    )))

(provide 'hmouse-sh)

;;; hmouse-sh.el ends here
