;;; jabber-si-common.el --- stream initiation (JEP-0095)

;; Copyright (C) 2006  Magnus Henoch

;; Author: Magnus Henoch <mange@freemail.hu>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

(defvar jabber-si-stream-methods nil
  "Supported SI stream methods.

Each entry is a list, containing:
 * The namespace URI of the stream method
 * Active initiation function
 * Passive initiation function

The active initiation function should initiate the connection,
while the passive initiation function should wait for an incoming
connection.  Both functions take the same arguments:

 * JID of peer
 * SID
 * \"connection established\" function

The \"connection established\" function should be called when the
stream has been established and data can be transferred.  It is part
of the profile, and takes the following arguments:

 * JID of peer
 * SID
 * Either:
    - \"send data\" function, with one string argument
    - an error message, when connection failed

It returns an \"incoming data\" function.

The \"incoming data\" function should be called when data arrives on
the stream.  It takes these arguments:

 * JID of peer
 * SID
 * A string containing the received data, or nil on EOF

If it returns nil, the stream should be closed.")

(provide 'jabber-si-common)
;; arch-tag: 9e7a5c8a-bdde-11da-8030-000a95c2fcd0
;;; jabber-si-common.el ends here
