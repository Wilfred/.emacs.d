;;; isearch+.el --- Extensions to `isearch.el'.
;;
;; Filename: isearch+.el
;; Description: Extensions to `isearch.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2013, Drew Adams, all rights reserved.
;; Created: Fri Dec 15 10:44:14 1995
;; Version: 20130410.2040
;; X-Original-Version: 21.0
;; Last-Updated: Wed Apr 10 13:37:09 2013 (-0700)
;;           By: dradams
;;     Update #: 2083
;; URL: http://www.emacswiki.org/isearch+.el
;; Doc URL: http://www.emacswiki.org/IsearchPlus
;; Keywords: help, matching, internal, local
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x
;;
;; Features that might be required by this library:
;;
;;   `avoid', `frame-fns', `misc-cmds', `misc-fns', `strings',
;;   `thingatpt', `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `isearch.el'.
;;
;;  More description below.
;;
;;
;;  Index
;;  -----
;;
;;  If you have library `linkd.el' and Emacs 22 or later, load
;;  `linkd.el' and turn on `linkd-mode' now.  It lets you easily
;;  navigate around the sections of this doc.  Linkd mode will
;;  highlight this Index, as well as the cross-references and section
;;  headings throughout this file.  You can get `linkd.el' here:
;;  http://dto.freeshell.org/notebook/Linkd.html.
;;
;;  (@> "Overview of Features")
;;  (@> "Change log")
;;  (@> "Faces and Variables")
;;  (@> "Keys and Hooks")
;;  (@> "Commands")
;;  (@> "Non-Interactive Functions")
;;  (@> "Character-Property Search")
;;
;;
;;  Macros defined here:
;;
;;    `with-isearch-suspended' (Emacs 22-24.3).
;;
;;  Commands defined here:
;;
;;    `isearch-insert-char-by-name' (Emacs 23-24.2),
;;    `isearchp-char-prop-backward',
;;    `isearchp-char-prop-backward-regexp',
;;    `isearchp-char-prop-forward',
;;    `isearchp-char-prop-forward-regexp',
;;    `isearchp-cycle-mismatch-removal',
;;    `isearchp-fontify-buffer-now', `isearchp-init-edit',
;;    `isearchp-open-recursive-edit' (Emacs 22+),
;;    `isearchp-put-prop-on-region',
;;    `isearchp-retrieve-last-quit-search',
;;    `isearchp-set-region-around-search-target',
;;    `isearchp-toggle-invisible',
;;    `isearchp-toggle-regexp-quote-yank',
;;    `isearchp-toggle-set-region', `isearchp-yank-char' (Emacs 22+),
;;    `isearchp-yank-line' (Emacs 22+),
;;    `isearchp-yank-sexp-symbol-or-char' (Emacs 22+),
;;    `isearchp-yank-sexp-symbol-or-char-1' (Emacs 22+),
;;    `isearchp-yank-symbol-or-char' (Emacs 22+),
;;    `isearchp-yank-symbol-or-char-1' (Emacs 22+),
;;    `isearchp-yank-word-or-char' (Emacs 22+).
;;
;;  User options defined here:
;;
;;    `isearchp-drop-mismatch', `isearchp-initiate-edit-commands'
;;    (Emacs 22+), `isearchp-mouse-2-flag',
;;    `isearchp-regexp-quote-yank-flag', `isearchp-set-region-flag'.
;;
;;  Faces defined here:
;;
;;    `isearch-fail'.
;;
;;  Non-interactive functions defined here:
;;
;;    `isearchp-char-prop-1', `isearchp-char-prop-default-match-fn',
;;    `isearchp-char-prop-end', `isearchp-char-properties-in-buffer',
;;    `isearchp-char-prop-filter-pred',
;;    `isearchp-char-prop-matches-p', `isearchp-fail-pos',
;;    `isearchp-highlight-lighter', `isearchp-message-prefix',
;;    `isearchp-message-suffix', `isearchp-read-face-names',
;;    `isearchp-read-face-names--read', `isearchp-read-sexps',
;;    `isearchp-remove-duplicates', `isearchp-remove-mismatch',
;;    `isearchp-repeat-command', `isearchp-set-region',
;;    `isearchp-set-sel-and-yank', `isearchp-some',
;;    `isearchp-update-edit-init-commands' (Emacs 22+).
;;
;;  Internal variables defined here:
;;
;;    `isearchp-char-prop-prop', `isearchp-char-prop-type',
;;    `isearchp-char-prop-values', `isearchp-filter-predicate-orig',
;;    `isearchp-last-non-nil-invisible',
;;    `isearchp-last-quit-regexp-search', `isearchp-last-quit-search',
;;    `isearchp-win-pt-line'.
;;
;;
;;  ***** NOTE: The following functions defined in `isearch.el' have
;;              been REDEFINED OR ADVISED HERE:
;;
;;  `isearch-abort'       - Save search string when `C-g'.
;;  `isearch-cancel'      - Restore cursor position relative to window.
;;  `isearch-edit-string' - Put point at mismatch position.
;;  `isearch-mode'        - Save cursor position relative to window.
;;  `isearch-mode-help'   - End isearch.  List bindings.
;;  `isearch-message'     - Highlight failed part of search string in
;;                          echo area, in face `isearch-fail'.
;;  `isearch-message-prefix' - Highlight prompt keywords:
;;                             wrapped, regexp, word, multi
;;  `isearch-mouse-2'     - Respect `isearchp-mouse-2-flag'(Emacs 21+)
;;  `isearch-printing-char' - Respect option `isearchp-drop-mismatch'
;;  `isearch-toggle-case-fold' - Show case sensitivity in mode-line.
;;                               Message.
;;  `isearch-toggle-word' - Message, and turn off regexp search.
;;  `isearch-update' (Emacs 20-23) - Run `isearch-update-post-hook'.
;;  `isearch-yank-string' - Respect `isearchp-regexp-quote-yank-flag'.
;;
;;
;;  The following bindings are made here for incremental search mode
;;  (`C-s' prefix):
;;
;;    `C-`'        `isearchp-toggle-regexp-quote-yank'
;;    `C-+'        `isearchp-toggle-invisible'
;;    `C-_'        `isearchp-yank-symbol-or-char' (Emacs 22+)
;;    `C-('        `isearchp-yank-sexp-symbol-or-char' (Emacs 22+)
;;    `C-SPC'      `isearchp-toggle-set-region'
;;    `C-end'      `goto-longest-line' (requires `misc-cmds.el')
;;    `C-h'        `isearch-mode-help'
;;    `C-t'        `isearchp-char-prop-forward' (Emacs 23+)
;;    `C-x o'      `isearchp-open-recursive-edit' (Emacs 22+)
;;    `C-x 8 RET'  `isearch-insert-char-by-name' (Emacs 23-24.2)
;;    `C-y C-_'    `isearchp-yank-symbol-or-char' (Emacs 22+)
;;    `C-y C-('    `isearchp-yank-sexp-symbol-or-char' (Emacs 22+)
;;    `C-y C-2'    `isearch-yank-secondary'
;;    `C-y C-c'    `isearchp-yank-char' (Emacs 22+)
;;    `C-y C-e'    `isearchp-yank-line'
;;    `C-y C-w'    `isearchp-yank-word-or-char' (Emacs 22+)
;;    `C-y C-y'    `isearch-yank-kill'
;;    `C-y M-y'    `isearch-yank-pop' (Emacs 24+)
;;    `M-c'        `isearch-toggle-case-fold'
;;    `M-e'        `isearch-edit-string'
;;    `M-g'        `isearchp-retrieve-last-quit-search'
;;    `M-k'        `isearchp-cycle-mismatch-removal'
;;    `M-r'        `isearch-toggle-regexp'
;;    `M-w'        `isearchp-kill-ring-save'
;;    `C-M-t'      `isearchp-char-prop-forward-regexp' (Emacs 23+)
;;    `C-M-y'      `isearch-yank-secondary'
;;    `C-M-tab'    `isearch-complete' (on MS Windows)
;;    `next'       `isearch-repeat-forward'
;;    `prior'      `isearch-repeat-backward'
;;
;;
;;  User option `isearchp-initiate-edit-commands' causes certain keys
;;  not to exit Isearch but rather to edit the search string.
;;  Customize it to `nil' if you do not want this behavior at all.
;;
;;
;;  The following bindings are made here for incremental search edit
;;  mode:
;;
;;    `C-x 8 RET'  `insert-char' (Emacs 23+)
;;    `C-M-tab'    `isearch-complete-edit' (MS Windows only)
;;
;;
;;  This file should be loaded *AFTER* loading the standard GNU file
;;  `isearch.el'.  So, in your `~/.emacs' file, do this:
;;
;;  (eval-after-load "isearch" '(require 'isearch+))
;;
 
;;(@* "Overview of Features")
;;
;;; Overview of Features ---------------------------------------------
;;
;;  * Case-sensitivity is indicated in the mode line minor-mode
;;    lighter: `ISEARCH' for case-insensitive; `Isearch' for
;;    case-sensitive.
;;
;;  * Highlighting of the mode-line minor-mode lighter when search has
;;    wrapped around (Emacs 24+ only).
;;
;;  * Highlighting of parts of the prompt, to indicate the type of
;;    search: regexp, word, multiple-buffer, and whether searching has
;;    wrapped around the buffer (Emacs 22+ only).
;;
;;  * Ability to search within character-property zones.  Example:
;;    search within zones having a `face' text property with a value
;;    of `font-lock-comment-face' or `font-lock-string-face'.  Search
;;    overlays or text properties.  From within Isearch: `C-t' (or
;;    `C-M-t' for regexp search).  First time, or with a prefix
;;    argument, you are prompted for the property and its values.  See
;;    the doc string of command `isearchp-char-prop-forward'.
;;
;;  * Besides relying on other code to set `face' and other text
;;    properties for use with `C-t', you can use command
;;    `isearchp-put-prop-on-region' (outside of Isearch) to add a text
;;    property to a zone of text.  By default, it applies the last
;;    property and value whose zones you searched using `C-t', but a
;;    prefix arg lets you specify the property and value to apply.
;;    This gives you an interactive way to set up zones for
;;    text-property search (`C-t').  For property `face', empty input
;;    removes all faces from the region.
;;
;;  * Option and commands to let you select the last target occurrence
;;    (set the region around it):
;;
;;    - Option `isearchp-set-region-flag' - Non-`nil' means
;;      automatically set the region around the last search target.
;;    - Command `isearchp-toggle-set-region', bound to `C-SPC' during
;;      isearch - toggle `isearchp-set-region-flag'.
;;    - Command `set-region-around-search-target' - manually set the
;;      region around the last search target.
;;
;;  * Option (`isearchp-regexp-quote-yank-flag') and command
;;    (`isearchp-toggle-regexp-quote-yank', bound to `C-`') to toggle
;;    quoting (escaping) of regexp special characters.  With escaping
;;    turned off, you can yank text such as `^\*.*' without it being
;;    transformed to `\^\\\*\.\*'.
;;
;;  * `M-g' (`isearchp-retrieve-last-quit-search') yanks the last
;;    successful search string (regexp or plain) from when you last
;;    hit `C-g' in Isearch.  Sometimes you search for something but
;;    abandon the search - you just want to check the locations of
;;    something, without staying at any of them.  Afterward, if you
;;    want to find them again, use `M-g'.  This yanks that search
;;    string, so you can append it to whatever you are already
;;    searching for.
;;
;;  * `C-M-y' (`isearch-yank-secondary') yanks the secondary selection
;;    into the search string, if you also use library `second-sel.el'.
;;
;;  * `C-c' (`isearchp-yank-char') yanks successive characters onto
;;    the search string.
;;
;;  * `C-_' (`isearchp-yank-symbol-or-char') yanks successive symbols
;;    (or words or subwords or chars) into the search string.
;;
;;  * `C-(' (`isearchp-yank-symbol-or-char') yanks successive sexps
;;    (or symbols or words or subwords or chars) into the search string.
;;
;;  * `M-w' (`isearchp-kill-ring-save') copies the current search
;;    string to the kill ring.  You can then, for example, use `C-s
;;    M-y' to search for the same thing in another Emacs session.
;;    (Use `M-s w' for `isearch-toggle-word'.)
;;
;;  * All commands that yank text onto the search string are bound to
;;    keys with prefix `C-y' (in addition to any other Isearch
;;    bindings):
;;
;;      `C-y C-_'   isearchp-yank-symbol-or-char
;;      `C-y C-('   isearchp-yank-sexp-symbol-or-char
;;      `C-y C-2'   isearch-yank-secondary
;;      `C-y C-c'   isearchp-yank-char
;;      `C-y C-e'   isearchp-yank-line
;;      `C-y C-w'   isearchp-yank-word-or-char
;;      `C-y C-y'   isearch-yank-kill
;;      `C-y M-y'   isearch-yank-pop
;;
;;    You can repeat any of these for which it makes sense (i.e., all
;;    except `isearch-yank-secondary', `isearch-yank-kill', and
;;    `isearch-yank-pop') by just repeating the last key.  For
;;    example: `C-y C-e C-e C-e' adds the text up to the end of three
;;    lines.
;;
;;    Note: Because of Emacs bug #14095 (a regression), this does not
;;    yet work in Emacs 24 for `isearchp-yank-line'.  (It "works" for
;;    the others in Emacs 24 only because the same key without the
;;    `C-y' prefix is bound to essentially the same command.  So the
;;    "repetition" is really a separate command.)
;;
;;  * `C-x 8 RET' (`isearchp-read-unicode-char') reads the name of a
;;    Unicode character with completion and appends it to the search
;;    string.  Same thing when editing the search string (i.e., after
;;    `M-e').
;;
;;  * `C-x o' (`isearchp-open-recursive-edit') opens a recursive
;;    editing session, where you can do anything you like (including
;;    search for something different).  Using `C-M-c' closes the
;;    recursive editing session and resumes the search (from the
;;    current position when you hit `C-M-c').
;;
;;  * Highlighting of the mismatched portion of your search string in
;;    the minibuffer.  This is the portion that is removed if you do
;;    `C-g', or removed/replaced automatically if you use `M-k' (see
;;    next).  I added this feature to vanilla Emacs in release 23.1.
;;
;;  * `C-g' after successfully finding matches restores not only the
;;    original position but also its relative position in the window.
;;    IOW, you get back to what you saw before searching.  Fixes Emacs
;;    bug #12253 for Isearch.
;;
;;  * `M-k' (`isearchp-cycle-mismatch-removal') cycles automatic
;;    removal or replacement of the input portion that does not match,
;;    bound to .  The behavior is controlled by the value of option
;;    `isearchp-drop-mismatch':
;;
;;    `replace-last' - Your current input replaces the last mismatched
;;                     text.  You can always see your last input, even
;;                     if it is a mismatch.  And it is available for
;;                     editing using `M-e'.
;;    nil            - Your current input is appended, even if the
;;                     previous input has a mismatched portion.
;;    anything else  - Your current input is ignored (removed) if it
;;                     causes a mismatch.  The search string always
;;                     has successful matches.
;;
;;  * `C-+' (`isearchp-toggle-invisible') toggles invisible-text
;;    sensitivity while searching.
;;
;;  * Other bindings during Isearch:
;;
;;    - `next', `prior' repeat the last Isearch forward and backward
;;      (easier than using the chords `C-s', `C-r').
;;    - `C-end' - go to the longest line.  Repeat to go to the longest
;;      line following that one in the buffer.  As usual, `C-g' puts
;;      you back where you started.  This binding is made only if you
;;      also use `misc-cmds.el'.
;;    - `C-h' provides help on Isearch while searsching.  This library
;;      also redefines `isearch-mode-help' so that it lists all
;;      Isearch bindings and ends Isearch properly.
;;
;;  * `M-e' (`isearch-edit-string') automatically puts the cursor at
;;    the first mismatch position in the search string, for easy
;;    editing.  Whereas `C-g' (see also `M-k') removes all of the
;;    mismatch, this feature lets you change or insert a character or
;;    two, without losing the rest of the search string.
;;
;;  * A user option, `isearchp-initiate-edit-commands', that specifies
;;    commands whose keys will not exit Isearch but will instead
;;    initiate editing of the search string.  For example, if
;;    `backward-char' is included in the list then `C-b' and `left'
;;    will just move the cursor backward over the search string so you
;;    can change, delete, or insert chars in the middle somewhere.
;;    This makes the search string more minibuffer-like.
;;
;;  * You can, by default, select text with the mouse, then hit `C-s'
;;    etc. to search for it.  This is controlled by user option
;;    `isearchp-mouse-2-flag'.
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;(@* "Change log")
;;
;; 2013/04/10 dadams
;;     Define with-isearch-suspended for Emacs 24.3 too.  Apparently it did not make it into the release.
;; 2013/03/30 dadams
;;     Added: isearchp-yank-char, isearchp-yank-word-or-char, isearchp-yank-line, isearchp-repeat-command.
;;     Renamed isearchp-yank(-sexp)-symbol-or-char to isearchp-yank(-sexp)-symbol-or-char-1.
;;     isearchp-yank(-sexp)-symbol-or-char (new): Redefined as repeatable, using *-1 helper.
;;     Bind isearch-toggle-case-fold to M-c, not C-c.  Bind isearchp-yank-char to C-c.
;;     Bind isearchp-yank-(char|line|(word|symbol|sexp-symbol)-or-char) to C-y + control char.
;;     Do not bind vanilla isearch-yank commands to C-y prefix.
;;
;; 2013/03/29 dadams
;;     Added: isearchp-kill-ring-save.  Bind it to M-w, instead of isearch-toggle-word (which is now M-s w).
;;     Bind isearch-toggle-word to M-s w for Emacs < 23.
;;     Renamed: isearchp-with-search-suspended to with-isearch-suspended,
;;              isearchp-insert-char-by-name to isearch-insert-char-by-name.
;;     For Emacs 24.3+, do not define with-isearch-suspended or isearch-insert-char-by-name
;;       (vanilla has same definitions).  Do not duplicate key binding for isearch-insert-char-by-name.
;;     Make C-y a prefix key, and put all yank commands on it:
;;       C-y C-_   isearchp-yank-symbol-or-char
;;       C-y C-(   isearchp-yank-sexp-symbol-or-char
;;       C-y C-2   isearch-yank-secondary
;;       C-y C-c   isearchp-yank-char
;;       C-y C-e   isearchp-yank-line
;;       C-y C-w   isearchp-yank-word-or-char
;;       C-y C-y   isearch-yank-kill
;;       C-y M-y   isearch-yank-pop
;;     Moved key bindings and hooks to the end of the file.
;; 2013/01/28 dadams
;;     Advise isearch-forward to add Isearch+ doc.
;; 2013/01/16 dadams
;;     New feature: C-g restores window position of start.  Fixes Emacs bug #12253.
;;       Added redefinitions of isearch-cancel, isearch-mode.  Added: isearchp-win-pt-line.
;; 2012/12/15 dadams
;;     Added redefinition of isearch-printing-char.
;;     Renamed/replaced: isearchp-toggle-mismatch-removal with isearchp-cycle-mismatch-removal,
;;                       isearchp-mismatch-removal-flag   with isearchp-drop-mismatch.
;;     isearchp-cycle-mismatch-removal, isearchp-drop-mismatch: Handle replace-last case.
;; 2012/12/13 dadams
;;     Advise: isearch-update (Emacs 20-23).
;;     Added: isearchp-toggle-mismatch-removal, isearchp-mismatch-removal-flag,
;;            isearchp-remove-mismatch, isearchp-open-recursive-edit.
;;     Bind isearchp-toggle-mismatch-removal to M-k, isearchp-open-recursive-edit to C-x o.
;;     Put isearchp-highlight-lighter on isearch-update-post-hook for Emacs 20-23 also.
;;     Updated to fit Juri's vanilla Emacs version of macro isearchp-use-new-search-string:
;;       Renamed: isearchp-use-new-search-string: to isearchp-with-search-suspended,
;;                isearchp-read-unicode-char to isearchp-insert-char-by-name.
;;       Updated isearch-edit-string, isearchp-insert-char-by-name to use new macro definition.
;; 2012/12/12 dadams
;;     Bind C-x and C-x 8 to nil in isearch-mode-map, for Emacs 23.
;; 2012/12/09 dadams
;;     Added: Macro isearchp-use-new-search-string (Emacs 22+) - factored from isearch-edit-string.
;;            isearchp-read-unicode-char (Emacs 23+).
;;     Bind C-x 8 RET in isearch-mode-map and minibuffer-local-isearch-map.
;;     isearch-edit-string:
;;       Define using isearchp-use-new-search-string.
;;       Update per Emacs 24: Save/restore *-case-fold-search. Bind history-add-new-input to nil.
;; 2012/10/09 dadams
;;     isearchp-read-face-names: Bind icicle-multi-completing-p to t.
;; 2012/09/30 dadams
;;     Added: isearchp-last-quit(-regexp)-search, isearchp-retrieve-last-quit-search,
;;            redefinition of isearch-abort.
;;     Bound isearchp-retrieve-last-quit-search to M-g.
;; 2012/08/27 dadams
;;     isearch(p)-message-(prefix|suffix): Emacs 24.2 turned out to use the same code as 24.1.
;; 2012/08/12 dadams
;;     isearch-edit-string: isearchp-fail-pos -> (or (isearch-fail-pos) (length isearch-string)).
;; 2012/08/08 dadams
;;     Added: isearchp-message-prefix, isearchp-message-suffix.  Use everywhere in place of vanilla.
;;     isearch-message, isearch-fail-pos:
;;       Redefine only for Emacs 22 & 23.  Use vanilla defs (Emacs 24, but with Emacs 22/23 vars).
;;       Removed isearchp-fail-pos - replaced it with isearch-fail-pos.
;;     isearch-message-prefix: Added redefinition for Emacs 24.2+ (changed arglist).
;; 2012/02/08 dadams
;;     isearchp-remove-duplicates: Redefined to use a hash table.
;; 2012/01/11 dadams
;;     Added isearch-message-prefix (redefinition).
;;     Added faces: isearchp-(wrapped|regexp|word|multi).
;;     isearchp-highlight-lighter: Propertize lighter if wrapped.
;; 2011/12/01 dadams
;;     isearchp-toggle-(invisible|regexp-quote-yank|set-region|case-fold):
;;       Added sit-for after message.
;;     isearchp-toggle-(regexp-quote-yank|set-region): Added isearch-update after message + sit-for.
;;     isearch-toggle-word: Redefine even for Emacs versions that have it, to get message etc.
;;                          Added message and sit-for.
;; 2011/11/14 dadams
;;     Bind switch-frame event to ignore in Isearch.
;;     Added and commented out: isearchp-switch-frame-or-exit.
;; 2011/11/13 dadams
;;     Added: isearchp-set-sel-and-yank.
;;     isearch-mouse-2: Use isearchp-set-sel-and-yank, even for nil case.
;;                      Don't require transient-mark-mode.
;; 2011/11/11 dadams
;;     Added defgroup for isearch-plus.  Added: isearchp-mouse-2-flag.
;;     Added redefinition of isearch-mouse-2 that respects isearchp-mouse-2-flag.
;;       And it works for Windows too and all Emacs versions.
;;     Bind mouse-2 to isearch-mouse-2 and down-mouse-2 to ignore.
;;     isearchp-highlight-lighter: Delete (isearch-mode isearch-mode) also from alist.
;;     isearch-yank-string: Updated wrt Emacs 24 code.
;; 2011/09/25 dadams
;;     Added: isearchp-put-prop-on-region.
;;     isearchp-read-face-names: Added optional args empty-means-none-p, only-one-p.
;;     isearchp-read-sexps: Added optional arg only-one-p.
;; 2011/09/23 dadams
;;     Added (renamed from icicle- versions): isearchp-char-prop-default-match-fn,
;;                                            isearchp-char-prop-matches-p, isearchp-some.
;;     isearchp-char-prop-1: Use isearchp-read-sexps, not icicle-sexp-list.
;;     isearchp-char-prop-filter-pred: Use isearchp-char-prop-matches-p,
;;                                     isearchp-char-prop-default-match-fn, not icicle-*.
;; 2011/09/22 dadams
;;     Added: isearchp-char-prop-(backward|forward)(-regexp), isearchp-fontify-buffer-now,
;;            isearchp-char-prop-(1|end|filter-pred), isearchp-char-properties-in-buffer,
;;            isearchp-read-face-names, isearchp-read-face-names--read, isearchp-read-sexps,
;;            isearchp-remove-duplicates, isearchp-char-prop-prop, isearchp-char-prop-type,
;;            isearchp-char-prop-values, isearchp-filter-predicate-orig.
;;     Renamed: set-region-around-search-target to isearchp-set-region-around-search-target.
;;     Bound isearchp-char-prop-forward(-regexp) to C-t, C-M-t.
;;     Define keys here, instead of on isearch-mode-hook.  So we rely on eval-after-load.
;;     Changed key for isearch-toggle-regexp to same as vanilla Emacs: M-r.
;; 2011/09/12 dadams
;;     isearchp-fail-pos: Replaced isearch-message* with isearch-string*.  Thx to Juri Linkov.
;; 2011/09/08 dadams
;;     Added isearchp-init-edit (from anonymous fn), so can see it in keymap help.
;; 2011/07/07 dadams
;;     Added: isearchp-highlight-lighter, isearch-toggle-case-fold (redefinition).
;;     Put isearchp-highlight-lighter on isearch-update-post-hook.
;; 2011/06/03 dadams
;;     isearchp-initiate-edit-commands: Added left-word.
;; 2011/05/27 dadams
;;     Added: isearchp-initiate-edit-commands, isearchp-update-edit-init-commands.
;; 2011/05/16 dadams
;;     Added: isearchp-fail-pos, redefinition of isearch-edit-string.
;;     Removed: isearchp-goto-success-end (not needed - go there by default now).
;; 2011/01/04 dadams
;;     Added autoload cookies for defcustom and commands.
;; 2010/12/05 dadams
;;     Added: isearchp-toggle-invisible, isearchp-last-non-nil-invisible.
;; 2010/10/18 dadams
;;     isearch-mode-hook: Protect isearchp-goto-success-end with fboundp.
;; 2010/06/23 dadams
;;     Added: isearchp-yank(-sexp)-symbol-or-char.  Bound to C-_, C-(.
;; 2010/04/22 dadams
;;     Added: isearchp-toggle-regexp-quote-yank, isearchp-regexp-quote-yank-flag,
;;            isearch-yank-string (redefinition).
;; 2009/06/09 dadams
;;     Bind isearch-repeat-(forward|backward) to (next|prior) in isearch-mode-map.
;; 2008/11/10 dadams
;;     Added: isearchp-goto-success-end.
;; 2008/05/25 dadams
;;     Don't add C-M-tab to isearch-mode-map if already defined.
;; 2008/05/24 dadams
;;     Don't bind C-j to isearch-edit-string.  Bind M-e to isearch-edit-string (for Emacs 20).
;; 2008/02/28 dadams
;;     isearch-message: Protect from Emacs 21 also.
;; 2008/02/24 dadams
;;     isearch-message:
;;       Juri's fix for M-r (was losing failed text) and C-M-s [a-z] (highlighted only ]).
;; 2008/02/23 dadams
;;     isearch-message:
;;       isearch-fail face: Provide better defaults.
;;       Juri's fix for M-p: Use isearch-message for succ-msg, if diff from first msg of
;;         isearch-cmds (isearch-edit-string sets it).
;; 2007/09/10 dadams
;;     Bound goto-longest-line to C-s C-end.  Added soft require of misc-cmds.el.
;; 2007/09/07 dadams
;;     isearch-message:
;;       regexp-quote succ-msg. put-text-property, not propertize, for trailing whitespace.
;; 2007/07/10 dadams
;;     isearchp-set-region: Do nothing unless transient-mark-mode.
;; 2007/02/02 dadams
;;     isearch-message: Fixed when succ-msg matches whole isearch-message (no highlight).
;; 2007/01/23 dadams
;;     isearch-message: For Emacs 22+ only.
;; 2006/12/12 dadams
;;     Added isearch-toggle-word (from Juri Linkov), and bound it.
;; 2006/10/28 dadams
;;     Added: isearch-fail, isearch-message (redefinition).
;; 2006/07/30 dadams
;;     Added: set-region-around-search-target.
;; 2006/07/29 dadams
;;     Added: isearchp-toggle-set-region,isearchp-set-region(-flag). Thx to Andreas Roehler
;; 2006/01/24 dadams
;;     On MS Windows, bind isearch-complete* to C-tab.
;; 1999/03/17 dadams
;;     Updated to corrspond to Emacs 34.1 version.
;; 1996/04/24 dadams
;;     Added redefinition of isearch-search.  Require cl.el.
;; 1995/12/28 dadams
;;     Changed isearch-edit-string binding.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

 ;; Cannot do (require 'isearch), because `isearch.el' does no `provide'.
 ;; Don't want to do a (load-library "isearch") either, because it wouldn't
 ;; allow doing (eval-after-load "isearch" '(progn (require 'isearch+)))

(require 'misc-cmds nil t) ;; goto-longest-line


;; Quiet the byte compiler.
(defvar bidi-display-reordering)        ; Emacs 24+, built-in.
(defvar isearch-error)                  ; In `isearch.el'.
(defvar isearch-filter-predicate)       ; In `isearch.el'.
(defvar isearch-invalid-regexp)         ; In `isearch.el' (Emacs 20-21).
(defvar isearch-last-case-fold-search)  ; In `isearch.el'.
(defvar isearch-message-prefix-add)     ; In `isearch.el'.
(defvar isearch-new-message)            ; In `with-isearch-suspended' (here).
(defvar isearch-new-string)             ; In `with-isearch-suspended' (here).
(defvar isearch-original-minibuffer-message-timeout) ; In `isearch.el'.
(defvar isearch-push-state-function)    ; In `isearch.el'.
(defvar isearch-start-hscroll)          ; In `isearch.el'.
(defvar isearch-within-brackets)        ; In `isearch.el'.
(defvar isearch-wrap-function)          ; In `isearch.el'.
(defvar last-repeatable-command)        ; In `repeat.el'.
(defvar minibuffer-message-timeout)     ; In Emacs C code.
(defvar multi-isearch-next-buffer-current-function) ; In `isearch.el'.
(defvar subword-mode)

(defvar isearchp-initiate-edit-commands) ; Below.

;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;(@* "Faces and Variables")

;;; Faces and Variables ----------------------------------------------
(defgroup isearch-plus nil
  "Isearch enhancements."
  :prefix "isearchp-" :group 'isearch
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
Isearch+ bug: \
&body=Describe bug here, starting with `emacs -Q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Download" "http://www.emacswiki.org/isearch+.el")
  :link '(url-link :tag "Description" "http://www.emacswiki.org/IsearchPlus")
  :link '(emacs-commentary-link :tag "Commentary" "isearch+"))

(when (> emacs-major-version 21)        ; Emacs 22+
  (defface isearch-fail
      '((((class color) (min-colors 88) (background dark))
         (:foreground "white" :background "#22225F5F2222")) ; a dark green
        (((class color) (min-colors 88) (background light))
         (:foreground "Black" :background "Plum"))
        (((class color) (min-colors 8)) (:background "red"))
        (((type tty) (class mono)) :inverse-video t)
        (t :background "gray"))
    "*Face for highlighting failed part in Isearch echo-area message."
    :group 'isearch-plus)
  (defface isearchp-wrapped
      '((((class color) (min-colors 88)) (:foreground "DeepPink"))
        (t :underline t))
    "*Face for highlighting wrapped-search indicator in Isearch echo-area message."
    :group 'isearch-plus)
  (defface isearchp-regexp
      '((((class color) (min-colors 8)) (:foreground "Firebrick"))
        (t :underline t))
    "*Face for highlighting regexp-search indicator in Isearch echo-area message."
    :group 'isearch-plus)
  (defface isearchp-word
      '((((class color) (min-colors 8)) (:foreground "DarkGreen"))
        (t :underline t))
    "*Face for highlighting word-search indicator in Isearch echo-area message."
    :group 'isearch-plus)
  (defface isearchp-multi
      '((((class color) (min-colors 8)) (:foreground "DarkViolet"))
        (t :underline t))
    "*Face for highlighting multi-buffer indicator in Isearch echo-area message."
    :group 'isearch-plus))

(when (fboundp 'isearch-unread-key-sequence) ; Emacs 22+

  (defun isearchp-init-edit (&rest ignored)
    "Invoke current key sequence, but after calling `isearch-edit-string'."
    (interactive)
    (isearch-unread-key-sequence
     (listify-key-sequence (this-command-keys)))
    (isearch-edit-string))

  (defun isearchp-update-edit-init-commands ()
    "Make `isearchp-initiate-edit-commands' edit the search string."
    (dolist (cmd  isearchp-initiate-edit-commands)
      (substitute-key-definition cmd 'isearchp-init-edit isearch-mode-map (current-global-map))))

  ;; No autoload cookie - need function `isearchp-update-edit-init-commands'.
  (defcustom isearchp-initiate-edit-commands
    '(backward-char                     ; `C-b'
      left-char                         ; `left' (Emacs 24+)
      ;; backward-delete-char                ; `DEL'
      ;; backward-delete-char-untabify       ; `DEL'
      ;; backward-kill-paragraph             ; `C-backspace'
      ;; backward-kill-sentence              ; `C-x DEL'
      ;; backward-kill-sexp                  ; `C-M-backspace'
      ;; backward-kill-word                  ; `M-DEL'
      ;; backward-list                       ; `C-M-p'
      ;; backward-page                       ; `C-x ['
      ;; backward-paragraph                  ; `C-up', `M-{'
      ;; backward-sentence                   ; `M-a'
      backward-sexp                     ; `C-M-b', `C-M-left'
      ;; backward-to-indentation             ; Not bound by default
      ;; backward-up-list                    ; `C-M-u', `C-M-up'
      backward-word                     ; `M-b', `M-left'
      left-word                         ; `C-left'
      ;; delete-backward-char
      ;; kill-backward-up-list               ; Not bound by default
      ;; beginning-of-buffer                 ; `M-<', `C-home'
      ;; beginning-of-defun                  ; `C-M-a', `C-M-home',
      ;; beginning-of-line                   ; `C-a', `home'
      ;; beginning-of-line+                  ; `C-a', `home'
      ;; beginning-of-line-text              ; Not bound by default
      ;; beginning-of-visual-line            ; `C-a', `home'
      )
    "*Commands whose key bindings initiate Isearch edit.
When invoked by a key sequence, Isearch edits the search string,
applying the command to it immediately.

Commands you might want to include here are typically commands that
move point to the left, possibly deleting text along the way.

Set this to `nil' if you always want all such commands to exit Isearch
and act on the buffer text."
    :set #'(lambda (sym defs)
             (custom-set-default sym defs)
             (isearchp-update-edit-init-commands))
    :type '(repeat (restricted-sexp :tag "Command"
                    ;; Use `symbolp' instead of `functionp' or `fboundp', in
                    ;; case the library defining the function is not loaded.
                    :match-alternatives (symbolp) :value ignore))
    :group 'isearch-plus))

;;;###autoload
(defcustom isearchp-drop-mismatch nil
  "*Non-nil means remove or replace a search-string mismatch.
There are three possible values:

`replace-last' - Replace the last mismatch in the search string with
                 the latest input (e.g., replace the last typed char
                 or last yanked text).
nil            - Never remove mismatched text from the search string.
anything else  - Always remove mismatched text from the search string.

* Vanilla Isearch has the behavior of a nil value.

* Non-nil, non-`replace-last' means the search string never contains
  mismatched characters.

* `replace-last' means you see only the latest mismatched input, and
  it is available for editing, using \\<isearch-mode-map>`\\[isearch-edit-string]'.

You can cycle among the three possible values using \
`\\[isearchp-cycle-mismatch-removal]'."
  :type '(choice
          (const :tag "Replace last mismatch"  'replace-last)
          (const :tag "Never remove mismatch"  nil)
          (other :tag "Always remove mismatch" t))
  :group 'isearch-plus)

;;;###autoload
(defcustom isearchp-mouse-2-flag t
  "*Non-nil means clicking `mouse-2' during Isearch yanks the selection.
In that case, you can select text with the mouse, then hit `C-s' to
search for it.

If the value is nil, yank only if the `mouse-2' click is in the echo
area.  If not in the echo area, invoke whatever `mouse-2' is bound to
outside of Isearch."
  :type 'boolean :group 'isearch-plus)

;;;###autoload
(defcustom isearchp-regexp-quote-yank-flag t
  "*Non-nil means escape special chars in text yanked for a regexp isearch.
You can toggle this with `isearchp-toggle-regexp-quote-yank', bound to
`C-`' during isearch."
  :type 'boolean :group 'isearch-plus)

;;;###autoload
(defcustom isearchp-set-region-flag nil
  "*Non-nil means set region around search target.
This is used only for Transient Mark mode.
You can toggle this with `isearchp-toggle-set-region', bound to
`C-SPC' during isearch."
  :type 'boolean :group 'isearch-plus)


(defvar isearchp-last-non-nil-invisible (or search-invisible  'open)
  "Last non-nil value of `search-invisible'.")

(defvar isearchp-last-quit-search nil
  "Last successful search string when you hit `C-g' to quit Isearch.")

(defvar isearchp-last-quit-regexp-search nil
  "Last successful search regexp when you hit `C-g' to quit regexp Isearch.")

(defvar isearchp-win-pt-line nil
  "Line number of point before searching, relative to `window-start'.")
 
;;(@* "Commands")

;;; Commands ---------------------------------------------------------

;;;###autoload
(defun isearchp-cycle-mismatch-removal () ; Bound to `M-k' in `isearch-mode-map'.
  "Cycle option `isearchp-drop-mismatch'."
  (interactive)
  (setq isearchp-drop-mismatch  (case isearchp-drop-mismatch
                                  (replace-last  nil)
                                  ((nil)         t)
                                  (otherwise     'replace-last)))
  (if (and isearchp-drop-mismatch  (not (eq 'replace-last isearchp-drop-mismatch)))
      (add-hook 'isearch-update-post-hook 'isearchp-remove-mismatch)
    (remove-hook 'isearch-update-post-hook 'isearchp-remove-mismatch))
  (case isearchp-drop-mismatch
    (replace-last  (message "Automatic REPLACEMENT of last mismatched input is now ON"))
    ((nil)         (message "Automatic removal of mismatched input is now OFF"))
    (otherwise     (message "Automatic removal of ALL mismatched input is now ON")))
  (sit-for 1)
  (isearch-update))

(defun isearchp-remove-mismatch ()
  "Remove the mismatched portion of the search string."
  (while (or (not isearch-success)  (if (boundp 'isearch-error)
                                        isearch-error
                                      isearch-invalid-regexp))
    (isearch-pop-state))
  (remove-hook 'isearch-update-post-hook 'isearchp-remove-mismatch)
  (isearch-update)
  (add-hook 'isearch-update-post-hook 'isearchp-remove-mismatch))

;;;###autoload
(defun isearchp-toggle-invisible ()     ; Bound to `C-+' in `isearch-mode-map'.
  "Toggle `search-invisible'."
  (interactive)
  (when search-invisible (setq isearchp-last-non-nil-invisible  search-invisible))
  (setq search-invisible  (if search-invisible nil isearchp-last-non-nil-invisible))
  (if search-invisible
      (message "Searching invisible text is now ON")
    (message "Searching invisible text is now OFF"))
  (sit-for 1)
  (isearch-update))

;;;###autoload
(defun isearchp-toggle-regexp-quote-yank () ; Bound to `C-`' in `isearch-mode-map'.
  "Toggle `isearchp-regexp-quote-yank-flag'."
  (interactive)
  (setq isearchp-regexp-quote-yank-flag  (not isearchp-regexp-quote-yank-flag))
  (if isearchp-regexp-quote-yank-flag
      (message "Escaping regexp special chars for yank is now ON")
    (message "Escaping regexp special chars for yank is now OFF"))
  (sit-for 1)
  (isearch-update))

;;;###autoload
(defun isearchp-toggle-set-region ()    ; Bound to `C-SPC' in `isearch-mode-map'.
  "Toggle `isearchp-set-region-flag'."
  (interactive)
  (setq isearchp-set-region-flag  (not isearchp-set-region-flag))
  (if isearchp-set-region-flag
      (message "Setting region around search target is now ON")
    (message "Setting region around search target is now OFF"))
  (sit-for 1)
  (isearch-update))


;; REPLACE ORIGINAL in `isearch.el' (Emacs 22+).
;;
;; 1. Turn off `isearch-regexp' when `isearch-word'.
;; 2. Show message about new state.
;;
;; From Juri Linkov, 2006-10-29, to emacs-devel@gnu.org
;; From Stefan Monnier, 2006-11-23, to help-gnu-emacs@gnu.org
;;
(defun isearch-toggle-word ()           ; Bound to `M-s w' in `isearch-mode-map'.
  "Toggle word searching on or off."
  ;; The status stack is left unchanged.
  (interactive)
  (setq isearch-word  (not isearch-word))
  (when isearch-word (setq isearch-regexp  nil)) ; Added to Juri's code by Stefan.
  (setq isearch-success   t
        isearch-adjusted  t)
  (if isearch-word (message "Whole word search is now ON") (message "Whole word search is now OFF"))
  (sit-for 1)
  (isearch-update))

;;;###autoload
(defun isearchp-set-region-around-search-target ()
  "Set the region around the last search or query-replace target."
  (interactive)
  (case last-command
    ((isearch-forward isearch-backward isearch-forward-regexp isearch-backward-regexp)
     (push-mark isearch-other-end t 'activate))
    (t (push-mark (match-beginning 0) t 'activate)))
  (setq deactivate-mark  nil))

(defun isearchp-message-prefix (&optional arg1 arg2 arg3)
  "Version of `isearch-message-prefix' that works for all Emacs releases."
  (if (or (< emacs-major-version 24)
          (and (= emacs-major-version 24)  (< emacs-minor-version 3)
               (not (string-match "^[0-9]+\\.[0-9]+\\.[0-9]+" emacs-version))))
      (isearch-message-prefix arg1 arg2 arg3) ; Emacs 20 through 24.2.
    (isearch-message-prefix arg1 arg2))) ; Emacs 24.1.N and 24.3+

(defun isearchp-message-suffix (&optional arg1 arg2)
  "Version of `isearch-message-suffix' that works for all Emacs releases."
  (if (or (< emacs-major-version 24)
          (and (= emacs-major-version 24)  (< emacs-minor-version 3)
               (not (string-match "^[0-9]+\\.[0-9]+\\.[0-9]+" emacs-version))))
      (isearch-message-suffix arg1 arg2) ; Emacs 20 through 24.2.
    (isearch-message-suffix arg1)))     ; Emacs 24.1.N and  24.3+


;; REPLACE ORIGINAL in `isearch.el'.
;;
;; Update minor-mode mode-line lighter to reflect case sensitivity.
;;
;;;###autoload
(defun isearch-toggle-case-fold ()      ; Bound to `M-c' in `isearch-mode-map'.
  "Toggle case folding in searching on or off.
The minor-mode lighter is `ISEARCH' for case-insensitive, `Isearch'
for case-sensitive."
  (interactive)
  (setq isearch-case-fold-search  (if isearch-case-fold-search nil 'yes)
        isearch-success           t
        isearch-adjusted          t)
  (isearchp-highlight-lighter)
  (let ((message-log-max  nil))
    (message "%s%s [case %ssensitive]" (isearchp-message-prefix nil nil isearch-nonincremental)
	     isearch-message (if isearch-case-fold-search "in" "")))
  (sit-for 1)
  (isearch-update))


;; REPLACE ORIGINAL in `isearch.el'.
;;
;; 1. Ends isearch: does `isearch-done' and `isearch-clean-overlays'
;;    instead of `isearch-update'.
;; 2. Lists isearch bindings too.
;;;###autoload
(defun isearch-mode-help ()             ; Bound to `C-h' in `isearch-mode-map'.
  "Display information on interactive search in buffer *Help*."
  (interactive)
  (describe-function 'isearch-forward)
  (isearch-done)
  (isearch-clean-overlays)
  (with-current-buffer "*Help*"
    (goto-char (point-max))
    (let ((buffer-read-only nil))
      (insert (substitute-command-keys "

Bindings in Isearch minor mode:
------------------------------

\\{isearch-mode-map}")))))



(when (and (> emacs-major-version 21)   ; Emacs 22 through 24.2
           (or (< emacs-major-version 24)  (and (= emacs-major-version 24)  (< emacs-minor-version 4))))

  (defmacro with-isearch-suspended (&rest body)
    "Exit Isearch mode, run BODY, and reinvoke the pending search.
BODY can involve use of the minibuffer, including recursive minibuffers.
You can update the global isearch variables by setting new values to
`isearch-new-string', `isearch-new-message', `isearch-new-forward',
`isearch-new-word', `isearch-new-case-fold'."
    ;; This code is very hairy for several reasons, explained in the code.
    ;; Mainly, `isearch-mode' must be terminated while suspended and then restarted.
    ;; If there were a way to catch any change of buffer from the minibuffer, this could be
    ;; simplified greatly.
    ;;
    ;; This code does not back up the search point. Should it, for use with `isearch-edit-string'?
    `(condition-case nil
      (progn
        (let ((enable-recursive-minibuffers  t)
              (isearch-nonincremental        isearch-nonincremental)
              ;; Locally bind all isearch global vars to protect them from recursive isearching.
              ;; isearch-string -message and -forward are not bound, so they can be changed.
              ;; Instead, save the values.
              (isearch-new-string            isearch-string)
              (isearch-new-message           isearch-message)
              (isearch-new-forward           isearch-forward)
              (isearch-new-word              isearch-word)
              (isearch-new-case-fold         isearch-case-fold-search)
              (isearch-regexp                isearch-regexp)
              (isearch-op-fun                isearch-op-fun)
              (isearch-cmds                  isearch-cmds)
              (isearch-success               isearch-success)
              (isearch-wrapped               isearch-wrapped)
              (isearch-barrier               isearch-barrier)
              (isearch-adjusted              isearch-adjusted)
              (isearch-yank-flag             isearch-yank-flag)
              (isearch-error                 isearch-error)

              ;; Do not bind this.  We want `isearch-search', below, to set it.  And the old value
              ;; does not matter after that.
              ;; (isearch-other-end         isearch-other-end)
              ;;
              ;; Perhaps some of these other variables should be bound for a shorter period,
              ;; ending before the next isearch-search.  But there doesn't seem to be a real bug,
              ;; so let's not risk it now.

              (isearch-opoint                isearch-opoint)
              (isearch-slow-terminal-mode    isearch-slow-terminal-mode)
              (isearch-small-window          isearch-small-window)
              (isearch-recursive-edit        isearch-recursive-edit)
              ;; Save the current configuration so we can restore it.
              (isearch-window-configuration  (current-window-configuration))

              ;; This could protect the index of the search rings, but we can't reliably count the
              ;; number of typed `M-p' in `read-from-minibuffer' to adjust the index accordingly.
              ;; So when the following is commented out, `isearch-mode' below resets the index to
              ;; the predictable value nil.
              ;; (search-ring-yank-pointer        search-ring-yank-pointer)
              ;; (regexp-search-ring-yank-pointer regexp-search-ring-yank-pointer)

              ;; Temporarily restore `minibuffer-message-timeout'.
              (minibuffer-message-timeout    isearch-original-minibuffer-message-timeout)
              (isearch-original-minibuffer-message-timeout
               isearch-original-minibuffer-message-timeout)
              old-point  old-other-end)
          ;; Suspend isearching until BODY is done.  This is so that the user can do anything
          ;; without failure, like switch buffers and start another isearch, and return.
          (condition-case nil (isearch-done t t) (exit nil)) ; was recursive editing
          ;; Save old point and `isearch-other-end' before reading from minibuffer, which can
          ;; change their values.
          (setq old-point      (point)
                old-other-end  isearch-other-end)
          (unwind-protect (progn ,@body)
            ;; Set point at the start (end) of old match if forward (backward), so after exiting
            ;; minibuffer isearch resumes at the start (end) of this match and can find it again.
            (if (and old-other-end  (eq old-point (point))  (eq isearch-forward
                                                                isearch-new-forward))
                (goto-char old-other-end))
            ;; Always resume isearching by restarting it.
            (isearch-mode isearch-forward isearch-regexp isearch-op-fun nil isearch-word)
            ;; Copy new local values to isearch globals
            (setq isearch-string            isearch-new-string
                  isearch-message           isearch-new-message
                  isearch-forward           isearch-new-forward
                  isearch-word              isearch-new-word
                  isearch-case-fold-search  isearch-new-case-fold))
          ;; Empty `isearch-string' means use default.
          (when (= 0 (length isearch-string))
            (setq isearch-string   (or (car (if isearch-regexp regexp-search-ring search-ring))
                                       "")
                  isearch-message  (mapconcat 'isearch-text-char-description
                                              isearch-string ""))
            ;; After taking the last element, adjust ring to previous one.
            (isearch-ring-adjust1 nil)))

        ;; This used to push the state as of before this `C-s', but it adds an inconsistent state
        ;; where some of the variables are from the previous search (e.g. `isearch-success'), and
        ;; some of the variables were just entered from the minibuffer (e.g. `isearch-string').
        ;; (isearch-push-state)

        (isearch-search)                ; Reinvoke the pending search.
        (isearch-push-state)            ; Push the correct state.
        (isearch-update)
        (when isearch-nonincremental
          ;; (sit-for 1) ;; needed if `isearch-done' does: (message "")
          (isearch-done)
          ;; The search-done message is confusing when the string is empty, so erase it.
          (when (equal isearch-string "") (message ""))))
      ;; Handle `abort-recursive-edit' outside of let to restore outside global values.
      (quit (isearch-abort)))))


(when (> emacs-major-version 21)        ; Emacs 22+

  ;; REPLACE ORIGINAL in `isearch.el'.
  ;;
  ;; 1. Start with point at the mismatch position - use `isearchp-message-prefix'.
  ;; 2. Use macro `with-isearch-suspended'.
  ;;
  (defun isearch-edit-string ()         ; Bound to `M-e' in `isearch-mode-map'.
    "Edit the search string in the minibuffer.
The following additional command keys are active while editing.
\\<minibuffer-local-isearch-map>
\\[exit-minibuffer] to resume incremental searching with the edited string.
\\[isearch-nonincremental-exit-minibuffer] to do one nonincremental search.
\\[isearch-forward-exit-minibuffer] to resume isearching forward.
\\[isearch-reverse-exit-minibuffer] to resume isearching backward.
\\[isearch-complete-edit] to complete the search string using the search ring.
\\[insert-char] to insert a Unicode character by name (with completion)."
    (interactive)
    (with-isearch-suspended
        (let* ((message-log-max            nil)
               ;; Do not add a new search string to the search ring here in `read-from-minibuffer'.
               ;; It should be added only by `isearch-update-ring', called from `isearch-done'.
               (history-add-new-input      nil)
               (minibuffer-history-symbol  nil)) ; Workaround for some incompatibility with `gmhist'.
          ;; FREE VARS here: `isearch-new-string', `isearch-new-message'.
          ;; Bound in `with-isearch-suspended'.
          (setq isearch-new-string   (read-from-minibuffer
                                      (isearchp-message-prefix nil nil isearch-nonincremental)
                                      (cons isearch-string (1+ (or (isearch-fail-pos)
                                                                   (length isearch-string))))
                                      minibuffer-local-isearch-map  nil
                                      (if isearch-regexp
                                          (cons 'regexp-search-ring
                                                (1+ (or regexp-search-ring-yank-pointer  -1)))
                                        (cons 'search-ring (1+ (or search-ring-yank-pointer  -1))))
                                      nil t)
                isearch-new-message  (mapconcat 'isearch-text-char-description isearch-new-string
                                                "")))))

  ;; Suggested by Juri Linkov: http://lists.gnu.org/archive/html/emacs-devel/2012-12/msg00281.html.
  ;;
  (defun isearchp-open-recursive-edit () ; Bound to `C-x o' in `isearch-mode-map'.
    "Invoke the editor command loop recursively, during Isearch.
Use `\\[exit-recursive-edit]' to end the recursive edit and resume searching from there.
Or use `abort-recursive-edit' to exit the recursive edit and cancel the previous search."
    (interactive)
    (with-isearch-suspended (recursive-edit))))

(when (< emacs-major-version 22)        ; Emacs 20-21.
  ;; Parts of the definition were taken from later Emacs versions, but are compatible with Emacs 20.
  (defun isearch-edit-string ()
    "Edit the search string in the minibuffer.
The following additional command keys are active while editing.
\\<minibuffer-local-isearch-map>
\\[exit-minibuffer] to resume incremental searching with the edited string.
\\[isearch-nonincremental-exit-minibuffer] to do one nonincremental search.
\\[isearch-forward-exit-minibuffer] to resume isearching forward.
\\[isearch-reverse-exit-minibuffer] to resume isearching backward.
\\[isearch-complete-edit] to complete the search string using the search ring.
\\<isearch-mode-map>
If first char entered is \\[isearch-yank-word], then do word search instead."
    ;; This code is very hairy for several reasons, explained in the code.
    ;; Mainly, `isearch-mode' must be terminated while editing and then restarted.
    ;; If there were a way to catch any change of buffer from the minibuffer, this could be
    ;; simplified greatly.
    ;; This code does not back up the search point. Should it, for use with `isearch-edit-string'?
    (interactive)
    (condition-case nil
        (progn (let ((isearch-nonincremental        isearch-nonincremental)
                     ;; Locally bind all isearch global variables to protect them from recursive
                     ;; isearching.  Do not bind `isearch-string', `isearch-message', and
                     ;; `isearch-forward', so they can be changed.  Instead, save their values.
                     (isearch-new-string            isearch-string)
                     (isearch-new-message           isearch-message)
                     (isearch-new-forward           isearch-forward)
                     (isearch-new-word              isearch-word)
                     (isearch-regexp                isearch-regexp)
                     (isearch-op-fun                isearch-op-fun)
                     (isearch-cmds                  isearch-cmds)
                     (isearch-success               isearch-success)
                     (isearch-wrapped               isearch-wrapped)
                     (isearch-barrier               isearch-barrier)
                     (isearch-adjusted              isearch-adjusted)
                     (isearch-yank-flag             isearch-yank-flag)
                     (isearch-invalid-regexp        isearch-invalid-regexp)
                     (isearch-within-brackets       isearch-within-brackets)
            ;;; Do not bind this.  We want isearch-search, below, to set it.
            ;;; And the old value won't matter after that.
            ;;;	     (isearch-other-end             isearch-other-end)
            ;;; Perhaps some of these other variables should be bound for a
            ;;; shorter period, ending before the next isearch-search.
            ;;; But there doesn't seem to be a real bug, so let's not risk it now.
                     (isearch-opoint                isearch-opoint)
                     (isearch-slow-terminal-mode    isearch-slow-terminal-mode)
                     (isearch-small-window          isearch-small-window)
                     (isearch-recursive-edit        isearch-recursive-edit)
                     ;; Save current configuration so we can restore it here.
                     (isearch-window-configuration  (current-window-configuration))
                     old-point old-other-end)
                 ;; Actually terminate isearching until editing is done.
                 ;; This is so that the user can do anything without failure,
                 ;; like switch buffers and start another isearch, and return.
                 (condition-case nil (isearch-done t t) (exit nil)) ; was recursive editing
                 ;; Save old point and `isearch-other-end' before reading from minibuffer, which
                 ;; can change their values.
                 (setq old-point      (point)
                       old-other-end  isearch-other-end)
                 (unwind-protect
                      (let* ((message-log-max            nil)
                             ;; Binding `minibuffer-history-symbol' to nil is a workaround for some
                             ;; incompatibility with `gmhist'.
                             (minibuffer-history-symbol  nil))
                        (setq isearch-new-string   (read-from-minibuffer
                                                    (isearchp-message-prefix nil nil
                                                                             isearch-nonincremental)
                                                    (cons isearch-string
                                                          (1+ (length isearch-string)))
                                                    minibuffer-local-isearch-map nil
                                                    (if isearch-regexp
                                                        (cons
                                                         'regexp-search-ring
                                                         (1+ (or regexp-search-ring-yank-pointer
                                                                 -1)))
                                                      (cons 'search-ring
                                                            (1+ (or search-ring-yank-pointer  -1))))
                                                    nil t)
                              isearch-new-message  (mapconcat 'isearch-text-char-description
                                                              isearch-new-string "")))
                   ;; Set point at the start (end) of old match if forward (backward),
                   ;; so after exiting minibuffer isearch resumes at the start (end)
                   ;; of this match and can find it again.
                   (if (and old-other-end  (eq old-point (point))  (eq isearch-forward
                                                                       isearch-new-forward))
                       (goto-char old-other-end))
                   ;; Always resume isearching by restarting it.
                   (isearch-mode isearch-forward isearch-regexp isearch-op-fun nil isearch-word)
                   ;; Copy new local values to isearch globals
                   (setq isearch-string   isearch-new-string
                         isearch-message  isearch-new-message
                         isearch-forward  isearch-new-forward
                         isearch-word     isearch-new-word))
                 ;; Empty isearch-string means use default.
                 (if (= 0 (length isearch-string))
                     (setq isearch-string   (or (car (if isearch-regexp
                                                         regexp-search-ring
                                                       search-ring))
                                                "")
                           isearch-message  (mapconcat 'isearch-text-char-description
                                                       isearch-string ""))
                   ;; This used to set the last search string, but it is not right to do that here.
                   ;; Only the string actually used should be saved.
                   ))

               ;; This used to push the state as of before this `C-s', but it adds an inconsistent
               ;; state where some of variables are from the previous search (e.g.
               ;; `isearch-success'), and some of variables are just entered from the minibuffer
               ;; (e.g. `isearch-string').
               ;; (isearch-push-state)

               (isearch-search)         ; Reinvoke the pending search.
               (isearch-push-state)     ; Push the correct state.
               (isearch-update)
               (when isearch-nonincremental
                 ;; (sit-for 1) ;; needed if isearch-done does: (message "")
                 (isearch-done)
                 ;; The search done message is confusing when the string
                 ;; is empty, so erase it.
                 (if (equal isearch-string "")
                     (message ""))))
      ;; Handle `abort-recursive-edit' outside of let to restore outside global values.
      (quit (isearch-abort)))))

(when (and (> emacs-major-version 22)   ; Emacs 23 (bc supports Unicode) through 24.2
           (or (< emacs-major-version 24)  (and (= emacs-major-version 24)  (< emacs-minor-version 3))))
  (defun isearch-insert-char-by-name () ; Bound to `C-x 8 RET' in `isearch-mode-map'.
    "Read a character by its Unicode name and insert it into search string."
    (interactive)
    (with-isearch-suspended
        (let ((char  (read-char-by-name "Insert character (Unicode name or hex): ")))
          (when char
            (setq isearch-new-string   (concat isearch-string (string char))
                  isearch-new-message  (concat isearch-message
                                               (mapconcat 'isearch-text-char-description (string char)
                                                          ""))))))))

(when (fboundp 'isearch-yank-internal)  ; Emacs 22+
  (defun isearchp-yank-char ()          ; Bound to `C-c' and `C-y C-c' in `isearch-mode-map'.
    "Yank next character from buffer onto search string.
You can repeat this by hitting the last key again..."
    (interactive)
    (require 'repeat nil t)
    (isearchp-repeat-command 'isearch-yank-char)) 

  (defun isearchp-yank-word-or-char ()  ; Bound to `C-w' and `C-y C-w' in `isearch-mode-map'.
    "Yank next word or character from buffer onto search string.
You can repeat this by hitting the last key again..."
    (interactive)
    (require 'repeat nil t)
    (isearchp-repeat-command 'isearch-yank-word-or-char)) 

  (defun isearchp-yank-line ()          ; Bound to `C-y C-e' in `isearch-mode-map'.
    "Yank text from buffer up to end of line onto search string.
You can repeat this by hitting the last key again..."
    (interactive)
    (require 'repeat nil t)
    (isearchp-repeat-command 'isearch-yank-line)) 

  (defun isearchp-yank-symbol-or-char-1 ()
    "Helper for `isearchp-yank-symbol-or-char'.
Not intended/needed as a user command."
    (interactive)
    (isearch-yank-internal
     (lambda ()
       (if (or (memq (char-syntax (or (char-after)  0)) '(?w ?_))
               (memq (char-syntax (or (char-after (1+ (point)))  0)) '(?w ?_)))
           (if (and (boundp 'subword-mode)  subword-mode) (subword-forward 1) (forward-symbol 1))
         (forward-char 1))
       (point))))

  (defun isearchp-yank-symbol-or-char () ; Bound to `C-_' and `C-y C-_' in `isearch-mode-map'.
    "Yank char, subword, word, or symbol from buffer into search string.
You can repeat this by hitting the last key again..."
    (interactive)
    (require 'repeat nil t)
    (isearchp-repeat-command 'isearchp-yank-symbol-or-char-1))

  (defun isearchp-yank-sexp-symbol-or-char-1 ()
    "Helper function for `isearchp-yank-sexp-symbol-or-char'.
Not intended/needed as a user command."
    (interactive)
    (isearch-yank-internal
     (lambda ()
       (if (or (= (char-syntax (or (char-after)  0)) ?\( )
               (= (char-syntax (or (char-after (1+ (point)))  0)) ?\( ))
           (forward-sexp 1)
         (if (or (memq (char-syntax (or (char-after)  0)) '(?w ?_))
                 (memq (char-syntax (or (char-after (1+ (point)))  0)) '(?w ?_)))
             (if (and (boundp 'subword-mode)  subword-mode) (subword-forward 1) (forward-symbol 1))
           (forward-char 1)))
       (point))))

  (defun isearchp-yank-sexp-symbol-or-char () ; Bound to `C-(' and `C-y C-(' in `isearch-mode-map'.
    "Yank sexp, symbol, subword, word, or char into search string.
You can repeat this by hitting the last key again..."
    (interactive)
    (require 'repeat nil t)
    (isearchp-repeat-command 'isearchp-yank-sexp-symbol-or-char-1)))

(defun isearchp-kill-ring-save ()       ; Bound to `M-w' in `isearch-mode-map'.
  "Copy the current search string to the kill ring.
For example, you can then use `C-s M-y' to search for the same thing
in another Emacs session."
  (interactive)
  (kill-new isearch-string)
  (let ((message-log-max  nil)) (message "Copied search string as kill"))
  (sit-for 1)
  (isearch-update))

(defun isearchp-retrieve-last-quit-search () ; Bound to `M-g' in `isearch-mode-map'.
  "Insert last successful search string from when you hit `C-g' in Isearch.
Bound to `\\<isearch-mode-map>\\[isearchp-retrieve-last-quit-search]' during Isearch."
  (interactive)
  (cond ((and isearch-regexp  isearchp-last-quit-regexp-search)
         (let ((isearchp-regexp-quote-yank-flag  nil))
           (isearch-yank-string isearchp-last-quit-regexp-search)))
        (isearchp-last-quit-search
         (isearch-yank-string isearchp-last-quit-search))))

(when (> emacs-major-version 20)
  (defun isearchp-fontify-buffer-now ()
    "Fontify buffer completely, right now.
This differs from `font-lock-fontify-buffer', which is lazy and does
not necessarily fontify the whole buffer."
    (interactive)
    (jit-lock-fontify-now)))

;;; $$$$$$ No longer used.  `M-e' puts point at this position automatically.
;;;   (defun isearchp-goto-success-end ()   ; `M-e' in `minibuffer-local-isearch-map'.
;;;     "Go to end of search string text that matches."
;;;     (interactive)
;;;     (goto-char (point-max))
;;;     (let ((cmds  isearch-cmds)
;;;           succ-msg)
;;;       (when (or (not isearch-success)  isearch-error)
;;;         (while (or (not (isearch-success-state (car cmds)))  (isearch-error-state (car cmds)))
;;;           (pop cmds))
;;;         (setq succ-msg  (and cmds  (isearch-message-state (car cmds))))
;;;         (backward-char (- (length isearch-string) (length succ-msg)))))))
 
;;(@* "Non-Interactive Functions")

;;; Non-Interactive Functions



;; REPLACE ORIGINAL in `isearch.el'.
;;
;; Save `isearchp-win-pt-line'.
;;
(defun isearch-mode (forward &optional regexp op-fun recursive-edit word)
  "Start Isearch minor mode.
It is called by the function `isearch-forward' and other related functions."

  ;; Initialize global vars.
  (setq isearch-forward forward
        isearch-regexp regexp
        isearch-word word
        isearch-op-fun op-fun
        isearch-last-case-fold-search isearch-case-fold-search
        isearch-case-fold-search case-fold-search
        isearch-string ""
        isearch-message ""
        isearch-cmds nil
        isearch-success t
        isearch-wrapped nil
        isearch-barrier (point)
        isearch-adjusted nil
        isearch-yank-flag nil
        isearch-invalid-regexp nil      ; Only for Emacs < 22.
        isearch-within-brackets nil     ; Only for Emacs < 22.
        isearch-error nil
        isearch-slow-terminal-mode (and (<= baud-rate search-slow-speed)
                                        (> (window-height)
                                           (* 4
                                              (abs search-slow-window-lines))))
        isearch-other-end nil
        isearch-small-window nil
        isearch-just-started t
        isearch-start-hscroll (window-hscroll)

        isearch-opoint (point)
        isearchp-win-pt-line (- (line-number-at-pos)
                                (line-number-at-pos (window-start)))
        search-ring-yank-pointer nil
        isearch-opened-overlays nil
        isearch-input-method-function input-method-function
        isearch-input-method-local-p (local-variable-p 'input-method-function)
        regexp-search-ring-yank-pointer nil

        ;; Save the original value of `minibuffer-message-timeout', and
        ;; set it to nil so that isearch's messages don't get timed out.
        isearch-original-minibuffer-message-timeout (and (boundp 'minibuffer-message-timeout)
                                                         minibuffer-message-timeout)
        minibuffer-message-timeout nil)

  ;; Bypass input method while reading key.  When a user types a printable char, appropriate
  ;; input method is turned on in minibuffer to read multibyte characters.
  (unless isearch-input-method-local-p  (make-local-variable 'input-method-function))
  (setq input-method-function  nil)
  (looking-at "")
  (setq isearch-window-configuration
        (if isearch-slow-terminal-mode (current-window-configuration) nil))

  ;; Maybe make minibuffer frame visible and/or raise it.
  (let ((frame (window-frame (minibuffer-window))))
    (unless (memq (frame-live-p frame) '(nil t))
      (unless (frame-visible-p frame)
        (make-frame-visible frame))
      (if minibuffer-auto-raise
          (raise-frame frame))))

  (setq	isearch-mode " Isearch");; forward? regexp?
  (force-mode-line-update)

  (setq overriding-terminal-local-map isearch-mode-map)
  (run-hooks 'isearch-mode-hook)

  ;; Pushing the initial state used to be before running isearch-mode-hook,
  ;; but a hook might set `isearch-push-state-function' used in
  ;; `isearch-push-state' to save mode-specific initial state.  (Bug#4994)
  (isearch-push-state)

  (isearch-update)

  (add-hook 'mouse-leave-buffer-hook 'isearch-done)
  (add-hook 'kbd-macro-termination-hook 'isearch-done)

  ;; isearch-mode can be made modal (in the sense of not returning to
  ;; the calling function until searching is completed) by entering
  ;; a recursive-edit and exiting it when done isearching.
  (if recursive-edit
      (let ((isearch-recursive-edit t))
        (recursive-edit)))
  isearch-success)


;; REPLACE ORIGINAL in `isearch.el'.
;;
;; Restore cursor position relative to window (`isearchp-win-pt-line').  Fixes Emacs bug #12253.
;;
(cond ((or (> emacs-major-version 23)   ; Emacs 23.2+
           (and (= emacs-major-version 23)  (> emacs-minor-version 1)))
       (defun isearch-cancel ()
         "Terminate the search and go back to the starting point."
         (interactive)
         (if (and isearch-push-state-function  isearch-cmds)
             ;; For defined push-state function, restore the first state.
             ;; This calls pop-state function and restores original point.
             (let ((isearch-cmds  (last isearch-cmds)))
               (if (fboundp 'isearch--set-state)
                   (isearch--set-state (car isearch-cmds)) ; Emacs 24.3+.
                 (isearch-top-state))   ; Emacs 23.2 to 24.2.
               (when isearchp-win-pt-line (recenter isearchp-win-pt-line)))
           (goto-char isearch-opoint)
           (when isearchp-win-pt-line (recenter isearchp-win-pt-line)))
         (isearch-done t)
         (isearch-clean-overlays)
         (signal 'quit nil)))
      (t                                ; Emacs 20 to 23.1.
       (defun isearch-cancel ()
         "Terminate the search and go back to the starting point."
         (interactive)
         (when (and (fboundp 'isearch-pop-fun-state) ; Emacs 22+.
                    (functionp (isearch-pop-fun-state (car (last isearch-cmds)))))
           (funcall (isearch-pop-fun-state (car (last isearch-cmds))) (car (last isearch-cmds))))
         (goto-char isearch-opoint)
         (when isearchp-win-pt-line (recenter isearchp-win-pt-line))
         (isearch-done t)
         (isearch-clean-overlays)
         (signal 'quit nil))))


;; REPLACE ORIGINAL in `isearch.el'.
;;
;; Save last successful search string or regexp as `isearchp-last-quit-search' or
;; `isearchp-last-quit-regexp-search', for retrieval via `isearchp-retrieve-last-quit-search'.
;;
(defun isearch-abort ()
  "Abort incremental search mode if searching is successful, signaling quit.
Otherwise, revert to previous successful search and continue searching.
Use `isearch-exit' to quit without signaling."
  (interactive)
  ;; (ding)  signal instead below, if quitting
  (discard-input)
  (if (and isearch-success
           (or (not (boundp 'isearch-error))  (not isearch-error))) ; Emacs 24+
      ;; If search is successful and has no incomplete regexp, move back to starting point and quit.
      (progn (setq isearch-success  nil)
             (set (if isearch-regexp 'isearchp-last-quit-regexp-search 'isearchp-last-quit-search)
                  isearch-string)
             ;; Exit isearch and pass on quit signal.
             (if (fboundp 'isearch-cancel) ; Emacs 22+
                 (isearch-cancel)
               (goto-char isearch-opoint) ; Emacs 20-21
               (isearch-done t)
               (isearch-clean-overlays)
               (signal 'quit nil)))
    ;; If search is failing, or has an incomplete regexp, rub out until it is once more successful.
    (while (or (not isearch-success)  (if (boundp 'isearch-error)
                                          isearch-error
                                        isearch-invalid-regexp))
      (isearch-pop-state))
    (isearch-update)))


;; REPLACE ORIGINAL in `isearch.el'.
;;
;; Respect `isearchp-regexp-quote-yank-flag'.
;;
(defun isearch-yank-string (string)
  "Yank STRING into Isearch search string."
  ;; Downcase the string if not supposed to case-fold yanked strings.
  (if (and isearch-case-fold-search  (eq 'not-yanks search-upper-case))
      (setq string  (downcase string)))
  (when (and isearch-regexp  isearchp-regexp-quote-yank-flag) (setq string  (regexp-quote string)))
  (setq isearch-yank-flag  t)           ; Don't move cursor in reverse search.
  (if (fboundp 'isearch-process-search-string) ; Emacs 24
      (isearch-process-search-string string (mapconcat 'isearch-text-char-description string ""))
    (setq isearch-string   (concat isearch-string string)
          isearch-message  (concat isearch-message (mapconcat 'isearch-text-char-description string
                                                              "")))
    (isearch-search-and-update)))


;; REPLACE ORIGINAL in `isearch.el'.
;;
;; 1. Respect `isearchp-mouse-2-flag'.
;;
;; 2. Works for older Emacs versions too: Set X selection, so `x-get-selection' returns non-nil.
;;
(defun isearch-mouse-2 (click)          ; Bound to `mouse-2' in `isearch-mode-map'.
  "Handle `mouse-2' in Isearch mode.
If `isearchp-mouse-2-flag' is non-nil, yank the X selection.
If `isearchp-mouse-2-flag' is nil, yank it only if the `mouse-2' click
is in the echo area.  Otherwise, invoke whatever `mouse-2' is bound to
outside of Isearch."
  (interactive "e")
  ;; For both the nil and non-nil `isearchp-mouse-2-flag' cases we need to explicitly set the X
  ;; selection, otherwise things won't work for older Emacs versions and depending on your
  ;; platform.  If not for that need, in Emacs 24+ we could simply use this for the non-nil case,
  ;; and make no change at all for the nil case:
  ;;
  ;; (let ((select-active-regions  t))
  ;;   (deactivate-mark)
  ;;   (isearch-yank-x-selection))
  ;;
  (if isearchp-mouse-2-flag
      (when (/= (region-beginning) (region-end)) (isearchp-set-sel-and-yank))
    (let ((win                            (posn-window (event-start click)))
          (overriding-terminal-local-map  nil)
          (binding                        (key-binding (this-command-keys-vector) t)))
      (if (and (window-minibuffer-p win)  (not (minibuffer-window-active-p win))) ; In echo area
          (isearchp-set-sel-and-yank)
        (when (functionp binding) (call-interactively binding))))))

(defun isearchp-set-sel-and-yank ()
  "Set X selection and yank it into echo area."
  (x-set-selection 'PRIMARY (buffer-substring-no-properties (region-beginning) (region-end)))
  (deactivate-mark)
  (isearch-yank-x-selection))


;; REPLACE ORIGINAL in `isearch.el'.
;;
;; If `isearchp-drop-mismatch' is `replace-last' then remove the last mismatched input.
;;
(defun isearch-printing-char ()
  "Add this ordinary printing character to the search string and search."
  (interactive)
  (when (eq isearchp-drop-mismatch 'replace-last)
    (while (or (not isearch-success)  (if (boundp 'isearch-error)
                                        isearch-error
                                      isearch-invalid-regexp))
      (isearch-pop-state)))
  (let ((char last-command-event))
    (if (= char ?\S-\ )
	(setq char ?\  ))
    (if current-input-method
	(isearch-process-search-multibyte-characters char)
      (isearch-process-search-char char))))


;; $$$$$$
;; (when (> emacs-major-version 21)        ; Emacs 22+
;;   (defun isearch-message (&optional c-q-hack ellipsis)
;;     ;; Generate and print the message string.
;;     (let ((cursor-in-echo-area ellipsis)
;;           (cmds isearch-cmds)
;;           succ-msg m)
;;       (while (not (isearch-success-state (car cmds))) (pop cmds))
;;       (setq succ-msg  (if (equal (isearch-message-state (car isearch-cmds)) isearch-message)
;;                           (and cmds  (isearch-message-state (car cmds)))
;;                         isearch-message))
;;       (setq m  (concat
;;                 (isearchp-message-prefix c-q-hack ellipsis isearch-nonincremental)
;;                 succ-msg
;;                 (and (not isearch-success)
;;                      (string-match (regexp-quote succ-msg) isearch-message)
;;                      (not (string= succ-msg isearch-message))
;;                      (propertize (substring isearch-message (match-end 0))
;;                                  'face 'isearch-fail))))
;;       (when (and (not isearch-success)  (string-match " +$" m))
;;         (put-text-property (match-beginning 0) (length m) 'face 'trailing-whitespace m))
;;       (setq m  (concat m (isearchp-message-suffix c-q-hack ellipsis)))
;;       (if c-q-hack m (let ((message-log-max nil)) (message "%s" m))))))



;; REPLACE ORIGINAL in `isearch.el'.
;;
;; Highlight failed part of search string in echo area, in face `isearch-fail'.
;; (Same as what I added to vanilla Emacs 24+.)
;;
(when (or (= emacs-major-version 22)  (= emacs-major-version 23)) ; Emacs 22 & 23.
  (defun isearch-message (&optional c-q-hack ellipsis)
    ;; Generate and print the message string.
    (let ((cursor-in-echo-area  ellipsis)
          (msg                  isearch-message)
          (fail-pos             (isearch-fail-pos t)))
      ;; Highlight failed part
      (when fail-pos
        (setq msg  (copy-sequence msg))
        (add-text-properties fail-pos (length msg) '(face isearch-fail) msg)
        (when (string-match " +$" msg)  ; Highlight trailing whitespace
          (add-text-properties (match-beginning 0) (match-end 0) '(face trailing-whitespace) msg)))
      (setq msg  (concat (isearch-message-prefix c-q-hack ellipsis isearch-nonincremental)
                         msg
                         (isearch-message-suffix c-q-hack ellipsis)))
      (if c-q-hack  msg  (let ((message-log-max  nil)) (message "%s" msg)))))

  (defun isearch-fail-pos (&optional msg)
    "Return position of first mismatch in search string, or nil if none.
If MSG is non-nil, use `isearch-message', otherwise `isearch-string'."
    (let ((cmds      isearch-cmds)
          (curr-msg  (if msg isearch-message isearch-string))
          succ-msg)
      (when (or (not isearch-success)  isearch-error)
        (while (or (not (isearch-success-state (car cmds)))  (isearch-error-state (car cmds)))
          (pop cmds))
        (setq succ-msg  (and cmds  (if msg
                                       (isearch-message-state (car cmds))
                                     (isearch-string-state (car cmds)))))
        (if (and (stringp succ-msg)
                 (< (length succ-msg) (length curr-msg))
                 (equal succ-msg (substring curr-msg 0 (length succ-msg))))
            (length succ-msg)
          0)))))


;; REPLACE ORIGINAL in `isearch.el'.
;;
;; Highlight message according to search characteristics.
;;
(when (and (> emacs-major-version 21)   ; Emacs 22 through Emacs 24.2
           (or (< emacs-major-version 24)  (and (= emacs-major-version 24)  (< emacs-minor-version 3))))
  (defun isearch-message-prefix (&optional _c-q-hack ellipsis nonincremental)
    ;; If about to search, and previous search regexp was invalid, check that it still is.
    ;; If it is valid now, let the message we display while searching say that it is valid.
    (and isearch-error  ellipsis  (condition-case ()
                                      (progn (re-search-forward isearch-string (point) t)
                                             (setq isearch-error  nil))
                                    (error nil)))
    ;; If currently failing, display no ellipsis.
    (unless isearch-success (setq ellipsis  nil))
    (let ((m (concat (and (not isearch-success)  (propertize "failing " 'face 'minibuffer-prompt))
                     (and isearch-adjusted  (propertize "pending " 'face 'minibuffer-prompt))
                     (and isearch-wrapped
                          (not isearch-wrap-function)
                          (if isearch-forward (> (point) isearch-opoint) (< (point) isearch-opoint))
                          (propertize "over" 'face 'isearchp-wrapped))
                     (and isearch-wrapped  (propertize "wrapped " 'face 'isearchp-wrapped))
                     (and isearch-word  (propertize "word " 'face 'isearchp-word))
                     (and isearch-regexp  (propertize "regexp " 'face 'isearchp-regexp))
                     (and (boundp 'multi-isearch-next-buffer-current-function) ; Emacs 23+
                          multi-isearch-next-buffer-current-function
                          (propertize "multi " 'face 'isearchp-multi))
                     (and (boundp 'isearch-message-prefix-add) ; Emacs 23+
                          isearch-message-prefix-add
                          (propertize isearch-message-prefix-add 'face 'minibuffer-prompt))
                     (propertize (if nonincremental "search" "I-search") 'face 'minibuffer-prompt)
                     (and (not isearch-forward)  (propertize " backward" 'face 'minibuffer-prompt))
                     (propertize (if (and (boundp 'bidi-display-reordering) ; Emacs 24+
                                          current-input-method)
                                     ;; Input methods for RTL languages use RTL chars for their
                                     ;; title.  That messes up display of search text after prompt.
                                     (bidi-string-mark-left-to-right
                                      (concat " [" current-input-method-title "]: "))
                                   ": ")
                                 'face 'minibuffer-prompt))))
      (concat (upcase (substring m 0 1)) (substring m 1)))))

(when (and (> emacs-major-version 23)   ; Emacs 24.3+
           (or (> emacs-major-version 24)  (and (= emacs-major-version 24)  (> emacs-minor-version 2))))
  (defun isearch-message-prefix (&optional ellipsis nonincremental)
    ;; If about to search, and previous search regexp was invalid,
    ;; check that it still is.  If it is valid now,
    ;; let the message we display while searching say that it is valid.
    (and isearch-error  ellipsis  (condition-case ()
                                      (progn (re-search-forward isearch-string (point) t)
                                             (setq isearch-error  nil))
                                    (error nil)))
    ;; If currently failing, display no ellipsis.
    (unless isearch-success (setq ellipsis  nil))
    (let ((m (concat (and (not isearch-success)  (propertize "failing " 'face 'minibuffer-prompt))
                     (and isearch-adjusted  (propertize "pending " 'face 'minibuffer-prompt))
                     (and isearch-wrapped
                          (not isearch-wrap-function)
                          (if isearch-forward (> (point) isearch-opoint) (< (point) isearch-opoint))
                          (propertize "over" 'face 'isearchp-wrapped))
                     (and isearch-wrapped  (propertize "wrapped " 'face 'isearchp-wrapped))
                     (and isearch-word
                          (propertize (or (and (symbolp isearch-word)
                                               (get isearch-word 'isearch-message-prefix))
                                          "word ")
                                      'face 'isearchp-word))
                     (and isearch-regexp  (propertize "regexp " 'face 'isearchp-regexp))
                     (and multi-isearch-next-buffer-current-function
                          (propertize "multi " 'face 'isearchp-multi))
                     (and isearch-message-prefix-add
                          (propertize isearch-message-prefix-add 'face 'minibuffer-prompt))
                     (propertize (if nonincremental "search" "I-search") 'face 'minibuffer-prompt)
                     (and (not isearch-forward)  (propertize " backward" 'face 'minibuffer-prompt))
                     (propertize (if current-input-method
                                     ;; Input methods for RTL languages use RTL chars for their
                                     ;; title.  That messes up display of search text after prompt.
                                     (bidi-string-mark-left-to-right
                                      (concat " [" current-input-method-title "]: "))
                                   ": ")
                                 'face 'minibuffer-prompt))))
      (concat (upcase (substring m 0 1)) (substring m 1)))))

(defun isearchp-read-face-names  (&optional empty-means-none-p only-one-p)
  "Read face names with completion, and return a list of their symbols.
If user hits `RET' with empty input immediately, then include all
faces.  Otherwise, read faces one by one, until user hits `RET' twice
consecutively.

Non-nil optional arg EMPTY-MEANS-NONE-P means return nil (no face
names) for empty user input.

Non-nil optional arg ONLY-ONE-P means read only one face name and
return its symbol."
  (let ((icicle-multi-completing-p                   t)
        (icicle-list-nth-parts-join-string           ": ")
        (icicle-list-join-string                     ": ")
        (icicle-list-use-nth-parts                   '(1))
        (icicle-proxy-candidates
         (and (boundp 'icicle-add-proxy-candidates-flag)  icicle-add-proxy-candidates-flag
              (append (and (fboundp 'eyedrop-face-at-point)  (list "*point face name*"))
                      (let ((ipc  ()))
                        (mapatoms
                         (lambda (cand)
                           (when (and (user-variable-p cand)  (eq (get cand 'custom-type) 'face))
                             (push `,(concat "'" (symbol-name cand) "'") ipc))))
                        ipc))))
        (face-cands                                  (mapcar
                                                      (if (and (boundp 'icicle-mode)  icicle-mode)
                                                          #'icicle-make-face-candidate
                                                        (lambda (face) (list (symbol-name face))))
                                                      (face-list)))
        (faces                                       ())
        (prompt1                                     "Face (RET for each, empty input to finish): ")
        (prompt2                                     "Face: ")
        (icicle-unpropertize-completion-result-flag  t)
        face)
    (when (and (boundp 'icicle-mode)  icicle-mode)
      (put-text-property 0 1 'icicle-fancy-candidates t prompt1)
      (put-text-property 0 1 'icicle-fancy-candidates t prompt2))
    (setq face  (isearchp-read-face-names--read prompt1 face-cands))
    (if (and empty-means-none-p  (string= "" face))
        ()
      (if only-one-p
          face
        (if (string= "" face)
            (setq faces  (face-list))
          (setq face-cands  (delete (assoc face face-cands) face-cands))
          (while (not (string= "" face))
            (add-to-list 'faces (intern face))
            (setq face        (isearchp-read-face-names--read prompt2 face-cands)
                  face-cands  (delete (assoc face face-cands) face-cands)))
          (nreverse faces))))))

(defun isearchp-read-face-names--read (prompt candidates)
  "Read a face name using PROMPT and face-name completion CANDIDATES."
  (if (and (boundp 'icicle-mode)  icicle-mode)
      (icicle-transform-multi-completion
       (completing-read
        prompt candidates nil (not (stringp icicle-WYSIWYG-Completions-flag)) nil
        (if (boundp 'face-name-history) 'face-name-history 'icicle-face-name-history)))
    (completing-read prompt candidates nil t nil 'face-name-history)))

(defun isearchp-read-sexps  (&optional only-one-p)
  "Read sexps with completion, and return them as a list.
Read sexps one by one, until user hits `RET' twice consecutively.
Non-nil ONLY-ONE-P means read only one sexp and return it."
  (let ((sexp-cands                         (mapcar #'list (isearchp-remove-duplicates
                                                            read-expression-history)))
        (sexps                              ())
        (prompt1                            "Sexp (RET for each, empty input to finish): ")
        (prompt2                            "Sexp: ")
        sexp)
    (setq sexp        (completing-read (if only-one-p prompt2 prompt1) sexp-cands
                                       nil nil nil 'read-expression-history)
          sexp-cands  (delete (assoc sexp sexp-cands) sexp-cands))
    (if only-one-p
        (car (read-from-string sexp))
      (while (not (string= "" sexp))
        (add-to-list 'sexps sexp)
        (setq sexp        (completing-read prompt2 sexp-cands nil nil nil 'read-expression-history)
              sexp-cands  (delete (assoc sexp sexp-cands) sexp-cands)))
      (prog1 (setq sexps  (nreverse (delete "" sexps)) ; Return the list of sexps.
                   sexps  (mapcar (lambda (sx) (car (read-from-string sx))) sexps))
        (when (interactive-p) (message "Sexps: %S" sexps))))))

(when (and (fboundp 'cl-puthash)  (not (fboundp 'puthash))) ; Emacs 20 with `cl-extra.el' loaded.
  (defalias 'puthash 'cl-puthash))

;; Same as `icicle-remove-duplicates'.
(if (fboundp 'puthash)                  ; Emacs 21+, or Emacs 20 with `cl-extra.el' loaded.
    (defun isearchp-remove-duplicates (sequence &optional test)
      "Copy of SEQUENCE with duplicate elements removed.
Optional arg TEST is the test function.  If nil, test with `equal'.
See `make-hash-table' for possible values of TEST."
      (setq test  (or test  #'equal))
      (let ((htable  (make-hash-table :test test)))
        (loop for elt in sequence
              unless (gethash elt htable)
              do     (puthash elt elt htable)
              finally return (loop for i being the hash-values in htable collect i))))

  (defun isearchp-remove-duplicates (list &optional use-eq)
    "Copy of LIST with duplicate elements removed.
Test using `equal' by default, or `eq' if optional USE-EQ is non-nil."
    (let ((tail  list)
          new)
      (while tail
        (unless (if use-eq (memq (car tail) new) (member (car tail) new))
          (push (car tail) new))
        (pop tail))
      (nreverse new))))

(defun isearchp-repeat-command (command)
  "Repeat COMMAND."
  (let ((repeat-message-function  'ignore))
    (setq last-repeatable-command  command)
    (repeat nil)))
 
;;(@* "Character-Property Search")

;;; Character-Property Search ----------------------------------------

(when (boundp 'isearch-filter-predicate) ; Emacs 23+

  (define-key isearch-mode-map "\C-t"    'isearchp-char-prop-forward)
  (define-key isearch-mode-map "\C-\M-t" 'isearchp-char-prop-forward-regexp)

  (defvar isearchp-filter-predicate-orig nil
    "Original value of `isearch-filter-predicate'.")

  (defvar isearchp-char-prop-type nil
    "Last property type used for `isearchp-char-prop-*' commands.")

  (defvar isearchp-char-prop-prop nil
    "Last property used for `isearchp-char-prop-*' commands.")

  (defvar isearchp-char-prop-values nil
    "Last property values used for `isearchp-char-prop-*' commands.")

  (defun isearchp-char-prop-forward (arg) ; Bound to `C-t' in `isearch-mode-map'.
    "Isearch forward for a character (overlay or text) property.
If you have not previously used an `isearch-char-prop-*' command, you
are prompted for:

 * the property type (`text', `overlay', or `text and overlay')
 * the property (e.g., `face', `mumamo-major-mode')
 * the property values (e.g., a list of faces, for property `face')

Otherwise:

 With no prefix arg, use the settings (property type, property,
 property values) from the last time you invoked an
 `isearch-char-prop-*' command.

 With a prefix arg you are prompted for the property and property
 values to use.  The particular prefix arg determines the property
 type to search, as follows:

  * plain prefix arg (`C-u'): both overlay and text property zones
  * negative prefix arg (e.g., `C--'): overlay property zones
  * non-negative prefix arg (e.g., `C-9'): text property zones

By default, an actual value of the property matches the value
you specify if it is `equal'.  Properties `mumamo-major-mode' and
`face' (or `font-lock-face') are exceptions.

For `mumamo-major-mode' you specify the major mode whose zones of text
you want to search.  The actual property value is a list whose car is
the major mode symbol.

For properties `face' and `font-lock-face', you can pick multiple
faces, using completion (hit `RET' with empty input to finish
choosing).  Text is searched that has a face property that includes
any of the faces you choose.  If you choose no face (empty input at
the outset), then text with any face at all is searched.

NOTE: If you search zones of property `face', and the property values
      include `font-lock' faces, then you might want to first make
      sure the entire buffer has been fontified.  You can do that
      using command `isearchp-fontify-buffer-now'.

NOTE: This command is available during normal Isearch, on key `C-t'.
      However, in order to be able to use a prefix arg with this
      command from within Isearch, you must set `isearch-allow-scroll'
      to non-nil.  Otherwise, a prefix arg during Isearch exits
      Isearch."
    (interactive "P")
    (isearchp-char-prop-1 'isearch-forward arg))

  (defun isearchp-char-prop-backward (arg)
    "Isearch backward for a character (overlay or text) property.
See `isearchp-char-prop-forward'."
    (interactive "P")
    (isearchp-char-prop-1 'isearch-backward arg))

  (defun isearchp-char-prop-forward-regexp (arg) ; Bound to `C-M-t' in `isearch-mode-map'.
    "Regexp Isearch forward for a character (overlay or text) property.
NOTE: This command is available during normal Isearch, on key `C-M-t'.
      However, in order to be able to use a prefix arg with this
      command, you must set `isearch-allow-scroll' to non-nil.
      Otherwise, a prefix arg during Isearch exits Isearch.
See `isearchp-char-prop-forward'."
    (interactive "P")
    (isearchp-char-prop-1 'isearch-forward-regexp arg))

  (defun isearchp-char-prop-backward-regexp (arg)
    "Regexp Isearch backward for a character (overlay or text) property.
See `isearchp-char-prop-backward'."
    (interactive "P")
    (isearchp-char-prop-1 'isearch-backward-regexp arg))

  (defun isearchp-char-prop-1 (search-fn arg)
    "Helper for `isearchp-char-prop-(forward|backward)(-regexp)'."
    (isearch-done)
    (let ((message-log-max  nil))
      (message "CHAR PROP %s%s" (isearchp-message-prefix nil nil isearch-nonincremental)
               isearch-message))
    (sit-for 1)
    (setq isearch-success   t
          isearch-adjusted  t)
    (let* ((enable-recursive-minibuffers    t)
           ;; Prevent invoking `isearch-edit-string', from `isearch-exit'.
           (search-nonincremental-instead   nil)
           ;; Test *-prop, not *-type, for TYPE, because nil means both.
           (type     (if (or arg  (not isearchp-char-prop-prop))
                         (if (not isearchp-char-prop-prop)
                             (let ((typname
                                    (completing-read
                                     "Type: " '(("text") ("overlay") ("text and overlay"))
                                     nil t nil nil "text and overlay")))
                               (and (not (string= "text and overlay" typname))  (intern typname)))
                           (and (atom arg) ; `C-u' means nil (both).
                                (if (wholenump (prefix-numeric-value arg)) 'text 'overlay)))
                       isearchp-char-prop-type))
           (props    (and (or arg  (not isearchp-char-prop-prop))
                          (mapcar #'(lambda (prop) (list (symbol-name prop)))
                                  (isearchp-char-properties-in-buffer
                                   (current-buffer) (point-min) (point-max) type))))
           (prop     (if (or arg  (not isearchp-char-prop-prop))
                         (intern (completing-read
                                  (format "%s property to search: "
                                          (if type (capitalize (symbol-name type)) "Character"))
                                  props nil nil nil nil "face"))
                       isearchp-char-prop-prop))
           (values   (if (or arg  (not isearchp-char-prop-values))
                         (if (memq prop '(face font-lock-face))
                             (isearchp-read-face-names)
                           (isearchp-read-sexps))
                       isearchp-char-prop-values)))
      (setq isearchp-filter-predicate-orig  isearch-filter-predicate
            isearch-filter-predicate        (isearchp-char-prop-filter-pred type prop values)
            isearchp-char-prop-type         type
            isearchp-char-prop-prop         prop
            isearchp-char-prop-values       values))
    (add-hook 'isearch-mode-end-hook 'isearchp-char-prop-end)
    (funcall search-fn))

  ;; Same as `icicle-char-properties-in-buffer', defined in `icicles-cmd2.el'.
  (defun isearchp-char-properties-in-buffer (&optional buffer beg end type)
    "List of all character properties in BUFFER between BEG and END.
Only the character properties are included, not their values.
TYPE can be `overlay', `text', or nil, meaning overlay properties,
text properties, or both, respectively."
    (unless buffer (setq buffer  (current-buffer)))
    (let ((props  ())
          ovrlays curr-props)
      (when (bufferp buffer)            ; Do nothing if BUFFER is not a buffer.
        (with-current-buffer buffer
          (unless (and beg  end)
            (setq beg  (point-min)
                  end  (point-max)))
          (when (or (not type)  (eq type 'overlay)) ; Get overlay properties.
            (setq ovrlays  (overlays-in beg end))
            (dolist (ovrly  ovrlays)
              (setq curr-props  (overlay-properties ovrly))
              (while curr-props
                (unless (memq (car curr-props) props) (push (car curr-props) props))
                (setq curr-props  (cddr curr-props)))))
          (when (or (not type)  (eq type 'text)) ; Get text properties.
            (while (< beg end)
              (setq beg         (or (next-property-change beg nil end)  end)
                    curr-props  (text-properties-at beg))
              (while curr-props
                (unless (memq (car curr-props) props) (push (car curr-props) props))
                (setq curr-props  (cddr curr-props)))))))
      props))

  (defun isearchp-char-prop-filter-pred (type prop values)
    "Return a predicate that uses `isearchp-char-prop-matches-p'.
TYPE, PROP, and VALUES are used by that function.
The predicate is suitable as a value of `isearch-filter-predicate'."
    (let ((tag  (make-symbol "isearchp-char-prop-filter-pred")))
      `(lambda (beg end)
        (and (isearch-filter-visible beg end)  (catch ',tag
                                                 (while (< beg end)
                                                   (unless (isearchp-char-prop-matches-p
                                                            ',type ',prop ',values
                                                            (isearchp-char-prop-default-match-fn
                                                             ',prop)
                                                            beg)
                                                     (throw ',tag nil))
                                                   (setq beg  (1+ beg)))
                                                 t)))))

  ;; Same as `icicle-search-char-prop-matches-p', defined in `icicles-cmd2.el'.
  (defun isearchp-char-prop-matches-p (type property values match-fn position)
    "Return non-nil if POSITION has PROPERTY with a value matching VALUES.
TYPE is `overlay', `text', or nil, and specifies the type of character
property - nil means look for both overlay and text properties.

Find text with a PROPERTY value that overlaps with VALUES: If the
value of PROPERTY is an atom, then it must be a member of VALUES.  If
it is a list, then at least one list element must be a member of
VALUES.

MATCH-FN is a binary predicate that is applied to each item of VALUES
and a zone of text with property PROP.  If it returns non-nil then the
zone is a search hit."
    (let* ((ovlyval  (and (or (not type)  (eq type 'overlay))
                          (get-char-property position property)))
           (textval  (and (or (not type)  (eq type 'text))
                          (get-text-property position property))))
      (or (and ovlyval  (isearchp-some values ovlyval match-fn))
          (and textval  (isearchp-some values textval match-fn)))))

  ;; Same as `icicle-some', defined in `icicles-fn.el'.
  (defun isearchp-some (list arg2 predicate)
    "Apply binary PREDICATE successively to an item of LIST and ARG2.
Return the first non-nil value returned by PREDICATE, or nil if none.
PREDICATE must be a function with two required arguments."
    (let ((result  nil))
      (catch 'isearchp-some
        (dolist (arg1  list)
          (when (setq result  (funcall predicate arg1 arg2))  (throw 'isearchp-some result))))
      result))

  ;; Same as `icicle-search-property-default-match-fn', defined in `icicles-cmd2.el'.
  (defun isearchp-char-prop-default-match-fn (prop)
    "Return the default match function for text or overlay property PROP.
Properties `face' and `mumamo-major-mode' are handled specially.
For other properties the values are matched using `equal'."
    (case prop
      ((face font-lock-face) (lambda (val rprop)
                               (if (consp rprop)
                                   (condition-case nil ; Allow for dotted cons.
                                       (member val rprop)
                                     (error nil))
                                 (eq val rprop))))
      ((mumamo-major-mode)   (lambda (val rprop) (equal val (car rprop))))
      (t                     #'equal)))

  (defun isearchp-char-prop-end ()
    "End Isearch for a character property."
    (setq isearch-filter-predicate  isearchp-filter-predicate-orig)
    (remove-hook 'isearch-mode-end-hook 'isearchp-char-prop-end))

  (defun isearchp-put-prop-on-region (property value beg end)
    "Add text PROPERTY with VALUE to the region from BEG to END.
If you have already used any of the commands `isearchp-char-prop-*'
and you do not use a prefix argument, then use the property and (the
first of) its values that you last specified for such searching.

Otherwise, you are prompted for the property and its value.

If the property is not `face' or `font-lock-face', then you enter a
sexp, which is read as the Lisp value to use.  E.g., if the property
is `mumamo-major-mode' then you might enter `(emacs-lisp-mode)' as the
value.

If the property is `face' or `font-lock-face' then you can specify
more than one face - their union is used as the property value.  If
you specify none (empty input immediately) then *all* faces are
*removed* from the region."
    (interactive
     (if (and (not current-prefix-arg)  isearchp-char-prop-prop  (car isearchp-char-prop-values))
         (list isearchp-char-prop-prop isearchp-char-prop-values (region-beginning) (region-end))
       (let* ((props  (and (or current-prefix-arg  (not isearchp-char-prop-prop))
                           (mapcar #'(lambda (prop) (list (symbol-name prop)))
                                   (isearchp-char-properties-in-buffer
                                    (current-buffer) (point-min) (point-max) 'text))))
              (prop   (intern (completing-read "Text property: " props nil nil nil nil "face")))
              (vals   (if (memq prop '(face font-lock-face))
                          (isearchp-read-face-names 'EMPTY-MEANS-NONE-P)
                        (isearchp-read-sexps 'ONLY-ONE-P))))
         (list prop vals (region-beginning) (region-end)))))
    (add-text-properties beg end (list property value)))

  )

 

(defadvice isearch-forward (before isearch+-doc activate)
  "
Isearch Plus
============

Commands
--------
Type \\<isearch-mode-map>\\[isearchp-char-prop-forward] to search for a character (overlay or text) property
Type \\[isearchp-char-prop-forward-regexp] to regexp-search for a character (overlay or text) property
Type \\[isearchp-cycle-mismatch-removal] to cycle option `isearchp-drop-mismatch'
Type \\[isearch-insert-char-by-name] to add a Unicode char to search string by Unicode name
Type \\[isearchp-open-recursive-edit] to invoke Emacs command loop recursively
Type \\[isearchp-toggle-set-region] to toggle setting region around search target
Type \\[isearchp-toggle-invisible] to toggle invisible-text sensitivity (`search-invisible')
Type \\[isearchp-toggle-regexp-quote-yank] to toggle quoting (escaping) of regexp special characters
Type \\[isearchp-retrieve-last-quit-search] to insert successful search string from when you hit `C-g'
Type \\[isearchp-yank-symbol-or-char] to yank a symbol or char from buffer onto search string
Type \\[isearchp-yank-sexp-symbol-or-char] to yank sexp, symbol, or char from buffer onto search string
Type \\[isearchp-put-prop-on-region] to add a text property to region
Type \\[isearchp-set-region-around-search-target] to select search hit

Options
-------
`isearchp-drop-mismatch' - how to handle input after a search mismatch
`isearchp-initiate-edit-commands' - keys that edit instead of exiting
`isearchp-mouse-2-flag' - `mouse-2' anywhere yanks the selection?
`isearchp-regexp-quote-yank-flag' - regexp-quote yanked text?
`isearchp-set-region-flag' - select last search target?")
 
;;(@* "Keys and Hooks")

;;; Keys and Hooks ---------------------------------------------------

(define-key isearch-mode-map [mouse-2]         'isearch-mouse-2)
;; Must not be just `nil'.  Need to override a global binding such as `mouse-flash-position-or-M-x'.
(define-key isearch-mode-map [down-mouse-2]    'ignore)

;; Must not be just `nil'.  Otherwise, if click `mouse-2' in a standalone minibuffer frame then
;; the `switch-frame' event exits Isearch and the following `down-mouse-2' invokes, e.g.,
;; `mouse-flash-position-or-M-x'.
(define-key isearch-mode-map [switch-frame]    'ignore)

;;; Use this instead of `ignore' for `switch-frame', if you want it to exit Isearch when you switch
;;; to any frame other than a standalone minibuffer frame.
;;; (defun isearchp-switch-frame-or-exit ()
;;;   "Return nil if switch to minibuffer frame.  Else exit Isearch.
;;; Bind to `switch-frame' event."
;;;   (interactive)
;;;   (let* ((vec   (this-command-keys-vector))
;;;          (evnt  (aref vec 0)))
;;;     (unless (and (consp evnt)  (eq 'switch-frame (car evnt))
;;;                  (cadr evnt)   (window-minibuffer-p (frame-selected-window (cadr evnt))))
;;;       (isearch-done)
;;;       (isearch-clean-overlays))))

;;; (define-key isearch-mode-map [switch-frame]    'isearchp-switch-frame-or-exit)

(define-key isearch-mode-map [(control ?+)]    'isearchp-toggle-invisible)
(define-key isearch-mode-map [(control ?`)]    'isearchp-toggle-regexp-quote-yank)
(define-key isearch-mode-map [(control ? )]    'isearchp-toggle-set-region)
(define-key isearch-mode-map "\C-h"            'isearch-mode-help)

;; An alternative to binding `isearch-edit-string' (but less flexible):
;; (setq search-exit-option  'edit) ; M- = edit search string, not exit.

(define-key isearch-mode-map "\M-c"            'isearch-toggle-case-fold)
;; This one is needed only for Emacs 20.  It is automatic after release 20.
(define-key isearch-mode-map "\M-e"            'isearch-edit-string)
(define-key isearch-mode-map "\M-g"            'isearchp-retrieve-last-quit-search)
(define-key isearch-mode-map "\M-k"            'isearchp-cycle-mismatch-removal)
;; This one is needed only for Emacs 20.  It is automatic after release 20.
(define-key isearch-mode-map "\M-r"            'isearch-toggle-regexp)
(when (< emacs-major-version 23)
  (define-key isearch-mode-map "\M-sw"         'isearch-toggle-word))
(define-key isearch-mode-map "\M-w"            'isearchp-kill-ring-save)
(when (fboundp 'isearch-yank-internal)
  (define-key isearch-mode-map "\C-c"          'isearchp-yank-char)
  (define-key isearch-mode-map "\C-_"          'isearchp-yank-symbol-or-char)
  (define-key isearch-mode-map [(control ?\()] 'isearchp-yank-sexp-symbol-or-char))
(when (and (fboundp 'goto-longest-line)  window-system) ; Defined in `misc-cmds.el'
  (define-key isearch-mode-map [(control end)] 'goto-longest-line))
(define-key isearch-mode-map [next]            'isearch-repeat-forward)
(define-key isearch-mode-map [prior]           'isearch-repeat-backward)
(when (and (eq system-type 'windows-nt) ; Windows uses M-TAB for something else.
           (not (lookup-key isearch-mode-map [C-M-tab])))
  (define-key isearch-mode-map [C-M-tab]       'isearch-complete))
(when (> emacs-major-version 21)
  (define-key isearch-mode-map "\C-x"          nil)
  (define-key isearch-mode-map "\C-xo"         'isearchp-open-recursive-edit) ; `o'pen edit session
  (when (and (> emacs-major-version 22)   ; Emacs 23 (supports Unicode) through 24.2
             (or (< emacs-major-version 24)  (and (= emacs-major-version 24)  (< emacs-minor-version 3))))
    (define-key isearch-mode-map "\C-x8"       nil)
    (define-key isearch-mode-map "\C-x8\r"     'isearch-insert-char-by-name)))

(define-key isearch-mode-map "\C-y"            nil) ; Put all yanking commands on prefix `C-y'.
(when (fboundp 'isearch-yank-internal)
  (define-key isearch-mode-map (kbd "C-y C-c") 'isearchp-yank-char)
  (define-key isearch-mode-map (kbd "C-y C-e") 'isearchp-yank-line)
  (define-key isearch-mode-map (kbd "C-y C-w") 'isearchp-yank-word-or-char)
  (define-key isearch-mode-map (kbd "C-y C-_") 'isearchp-yank-symbol-or-char)
  (define-key isearch-mode-map (kbd "C-y C-(") 'isearchp-yank-sexp-symbol-or-char))
(eval-after-load "second-sel"
  '(progn
    (define-key isearch-mode-map (kbd "C-y C-2") 'isearch-yank-secondary)
    (define-key isearch-mode-map (kbd "C-M-y") 'isearch-yank-secondary)))
(define-key isearch-mode-map "\C-y\C-y"        'isearch-yank-kill)
(define-key isearch-mode-map "\C-y\M-g"        'isearchp-retrieve-last-quit-search)
(when (fboundp 'isearch-yank-pop)
  (define-key isearch-mode-map "\C-y\M-y"      'isearch-yank-pop)) ; It is also just `M-y'.


(when (and (eq system-type 'windows-nt) ; Windows uses M-TAB for something else.
           (not (lookup-key minibuffer-local-isearch-map [C-M-tab])))
  (define-key minibuffer-local-isearch-map [C-M-tab] 'isearch-complete-edit))
(when (> emacs-major-version 22)
  (define-key minibuffer-local-isearch-map "\C-x8\r" 'insert-char))

(defun isearchp-set-region ()
  "Set the region around the search target, if `isearchp-set-region-flag'.
This is used only for Transient Mark mode."
  (when (and isearchp-set-region-flag  transient-mark-mode)
    (push-mark isearch-other-end t 'activate)))

(add-hook 'isearch-mode-end-hook 'isearchp-set-region)


;; ADVISE `isearch-update' to run `isearch-update-post-hook', in releases that do not do that.
(unless (boundp 'isearch-update-post-hook) ; Emacs 20-23
  (defadvice isearch-update (after run-isearch-update-post-hook activate)
    "Run `isearch-update-post-hook' at the end."
    (run-hooks 'isearch-update-post-hook)))

(defun isearchp-highlight-lighter ()
  "Update minor-mode mode-line lighter to reflect case sensitivity."
  (let ((case-fold-search  isearch-case-fold-search))
    (when (and (eq case-fold-search t)  search-upper-case)
      (setq case-fold-search  (isearch-no-upper-case-p isearch-string isearch-regexp)))
    ;; Vanilla Isearch uses the symbol `isearch-mode', hence the first of these.
    (setq minor-mode-alist  (delete '(isearch-mode isearch-mode) minor-mode-alist)
          minor-mode-alist  (delete '(isearch-mode " ISEARCH")   minor-mode-alist)
          minor-mode-alist  (delete '(isearch-mode " Isearch")   minor-mode-alist))
    (let ((lighter  (if case-fold-search " ISEARCH" " Isearch")))
      (add-to-list
       'minor-mode-alist
       `(isearch-mode ,(if (and (fboundp 'propertize)  isearch-wrapped) ;Emacs 22+
                           (propertize lighter 'face 'isearchp-wrapped)
                           lighter)))))
  (condition-case nil
      (if (fboundp 'redisplay) (redisplay t) (force-mode-line-update t))
    (error nil)))

(add-hook 'isearch-update-post-hook 'isearchp-highlight-lighter)

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'isearch+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; isearch+.el ends here
