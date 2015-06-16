;;; exco-ntlm.el --- Override some NTLM (NT LanManager) authentication support

;; Copyright (C) 2001, 2007-2014 Free Software Foundation, Inc.

;; Author: Taro Kawagishi <tarok@transpulse.org>
;; Keywords: NTLM, SASL
;; Version: 1.00
;; Created: February 2001

;; This file is part of GNU Emacs.

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

;; Override ntlm-build-auth-response to add NTLMv2 authentication support.
;; This is a major change that should go into Emacs, but it's
;; backward-compatible if ntlm-compatibility-level is set to 0 or 1.

;; The NTLM2 session support parts of this patch are already applied to the
;; Emacs master branch (Emacs 25.1).  See
;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=15603

;;; Code:

(require 'ntlm)
(require 'hmac-md5)
(require 'calc)

(defgroup ntlm nil
  "NTLM (NT LanManager) authentication."
  :version "25.1"
  :group 'comm)

(defcustom ntlm-compatibility-level 5
  "The NTLM compatibility level.
Ordered from 0, the oldest, least-secure level through 5, the
newest, most-secure level.  Newer servers may reject lower
levels.  At levels 3 through 5, send LMv2 and NTLMv2 responses.
At levels 0, 1 and 2, send LM and NTLM responses.

In this implementation, levels 0, 1 and 2 are the same (old,
insecure), and levels 3, 4 and 5 are the same (new, secure).  If
NTLM authentication isn't working at level 5, try level 0.  The
other levels are only present because other clients have six
levels."
  :type '(choice (const 0) (const 1) (const 2) (const 3) (const 4) (const 5)))

(defun ntlm-build-auth-request (user &optional domain)
  "Return the NTLM authentication request string for USER and DOMAIN.
USER is a string representing a user name to be authenticated and
DOMAIN is a NT domain.  USER can include a NT domain part as in
user@domain where the string after @ is used as the domain if DOMAIN
is not given."
  (interactive)
  (let ((request-ident (concat "NTLMSSP" (make-string 1 0)))
	(request-msgType (concat (make-string 1 1) (make-string 3 0)))
					;0x01 0x00 0x00 0x00
	(request-flags (concat (make-string 1 7) (make-string 1 178)
			       (make-string 1 8) (make-string 1 0)))
					;0x07 0xb2 0x08 0x00
	lu ld off-d off-u)
    (when (string-match "@" user)
      (unless domain
	(setq domain (substring user (1+ (match-beginning 0)))))
      (setq user (substring user 0 (match-beginning 0))))
    ;; set fields offsets within the request struct
    (setq lu (length user))
    (setq ld (length domain))
    (setq off-u 32)			;offset to the string 'user
    (setq off-d (+ 32 lu))		;offset to the string 'domain
    ;; pack the request struct in a string
    (concat request-ident		;8 bytes
	    request-msgType	;4 bytes
	    request-flags		;4 bytes
	    (md4-pack-int16 lu)	;user field, count field
	    (md4-pack-int16 lu)	;user field, max count field
	    (md4-pack-int32 (cons 0 off-u)) ;user field, offset field
	    (md4-pack-int16 ld)	;domain field, count field
	    (md4-pack-int16 ld)	;domain field, max count field
	    (md4-pack-int32 (cons 0 off-d)) ;domain field, offset field
	    user			;buffer field
	    domain		;buffer field
	    )))

(defmacro ntlm-string-as-unibyte (string)
    (if (fboundp 'string-as-unibyte)
	`(string-as-unibyte ,string)
      string))

(defun ntlm-compute-timestamp ()
  "Compute an NTLMv2 timestamp.
Return a unibyte string representing the number of tenths of a
microsecond since January 1, 1601 as a 64-bit little-endian
signed integer."
  (let* ((s-to-tenths-of-us "mul(add(lsh($1,16),$2),10000000)")
	 (us-to-tenths-of-us "mul($3,10)")
	 (ps-to-tenths-of-us "idiv($4,100000)")
	 (tenths-of-us-since-jan-1-1601
	  (apply 'calc-eval (concat "add(add(add("
				    s-to-tenths-of-us ","
				    us-to-tenths-of-us "),"
				    ps-to-tenths-of-us "),"
				    ;; tenths of microseconds between
				    ;; 1601-01-01 and 1970-01-01
				    "116444736000000000)")
                 ;; add trailing zeros to support old current-time formats
		 'rawnum (append (current-time) '(0 0))))
	 result-bytes)
    (dotimes (byte 8)
      (push (calc-eval "and($1,16#FF)" 'rawnum tenths-of-us-since-jan-1-1601)
	    result-bytes)
      (setq tenths-of-us-since-jan-1-1601
	    (calc-eval "rsh($1,8,64)" 'rawnum tenths-of-us-since-jan-1-1601)))
    (apply 'unibyte-string (nreverse result-bytes))))

(defun ntlm-generate-nonce ()
  "Generate a random nonce, not to be used more than once.
Return a random eight byte unibyte string."
  (unibyte-string
   (random 256) (random 256) (random 256) (random 256)
   (random 256) (random 256) (random 256) (random 256)))

(defun ntlm-build-auth-response (challenge user password-hashes)
  "Return the response string to a challenge string CHALLENGE given by
the NTLM based server for the user USER and the password hash list
PASSWORD-HASHES.  NTLM uses two hash values which are represented
by PASSWORD-HASHES.  PASSWORD-HASHES should be a return value of
 (list (ntlm-smb-passwd-hash password) (ntlm-md4hash password))"
  (let* ((rchallenge (ntlm-string-as-unibyte challenge))
	 ;; get fields within challenge struct
	 ;;(ident (substring rchallenge 0 8))	;ident, 8 bytes
	 ;;(msgType (substring rchallenge 8 12))	;msgType, 4 bytes
	 (uDomain (substring rchallenge 12 20))	;uDomain, 8 bytes
	 (flags (substring rchallenge 20 24))	;flags, 4 bytes
	 (challengeData (substring rchallenge 24 32)) ;challengeData, 8 bytes
	 uDomain-len uDomain-offs
	 ;; response struct and its fields
	 lmRespData			;lmRespData, 24 bytes
	 ntRespData			;ntRespData, variable length
	 domain				;ascii domain string
	 lu ld ln off-lm off-nt off-d off-u off-w off-s)
    ;; extract domain string from challenge string
    (setq uDomain-len (md4-unpack-int16 (substring uDomain 0 2)))
    (setq uDomain-offs (md4-unpack-int32 (substring uDomain 4 8)))
    (setq domain
	  (ntlm-unicode2ascii (substring challenge
					 (cdr uDomain-offs)
					 (+ (cdr uDomain-offs) uDomain-len))
			      (/ uDomain-len 2)))
    ;; overwrite domain in case user is given in <user>@<domain> format
    (when (string-match "@" user)
      (setq domain (substring user (1+ (match-beginning 0))))
      (setq user (substring user 0 (match-beginning 0))))

    (unless (and (integerp ntlm-compatibility-level)
		 (>= ntlm-compatibility-level 0)
		 (<= ntlm-compatibility-level 5))
      (error "Invalid ntlm-compatibility-level value"))
    (if (and (>= ntlm-compatibility-level 3)
	     (<= ntlm-compatibility-level 5))
	;; extract target information block, if it is present
	(if (< (cdr uDomain-offs) 48)
	    (error "Failed to find target information block")
	  (let* ((targetInfo-len (md4-unpack-int16 (substring rchallenge
							      40 42)))
		 (targetInfo-offs (md4-unpack-int32 (substring rchallenge
							       44 48)))
		 (targetInfo (substring rchallenge
					(cdr targetInfo-offs)
					(+ (cdr targetInfo-offs)
					   targetInfo-len)))
		 (upcase-user (upcase (ntlm-ascii2unicode user (length user))))
		 (ntlmv2-hash (hmac-md5 (concat upcase-user
						(ntlm-ascii2unicode
						 domain (length domain)))
					(cadr password-hashes)))
		 (nonce (ntlm-generate-nonce))
		 (blob (concat (make-string 2 1)
			       (make-string 2 0)	; blob signature
			       (make-string 4 0)	; reserved value
			       (ntlm-compute-timestamp) ; timestamp
			       nonce			; client nonce
			       (make-string 4 0)	; unknown
			       targetInfo		; target info
			       (make-string 4 0)))	; unknown
		 ;; for reference: LMv2 interim calculation
		 ;; (lm-interim (hmac-md5 (concat challengeData nonce)
		 ;;                       ntlmv2-hash))
		 (nt-interim (hmac-md5 (concat challengeData blob)
				       ntlmv2-hash)))
	    ;; for reference: LMv2 field, but match other clients that
	    ;; send all zeros
	    ;; (setq lmRespData (concat lm-interim nonce))
	    (setq lmRespData (make-string 24 0))
	    (setq ntRespData (concat nt-interim blob))))
      ;; compatibility level is 2, 1 or 0
      ;; level 2 should be treated specially but it's not clear how,
      ;; so just treat it the same as levels 0 and 1
      ;; check if "negotiate NTLM2 key" flag is set in type 2 message
      (if (not (zerop (logand (aref flags 2) 8)))
	  (let (randomString
		sessionHash)
	    ;; generate NTLM2 session response data
	    (setq randomString (ntlm-generate-nonce))
	    (setq sessionHash (secure-hash 'md5
					   (concat challengeData randomString)
					   nil nil t))
	    (setq sessionHash (substring sessionHash 0 8))
	    (setq ntRespData (ntlm-smb-owf-encrypt
			      (cadr password-hashes) sessionHash))
	    (setq lmRespData (concat randomString (make-string 16 0))))
	;; generate response data
	(setq ntRespData
	      (ntlm-smb-owf-encrypt (cadr password-hashes) challengeData))
	(setq lmRespData
	      (ntlm-smb-owf-encrypt (car password-hashes) challengeData))))

    ;; get offsets to fields to pack the response struct in a string
    (setq lu (length user))
    (setq ld (length domain))
    (setq ln (length ntRespData))
    (setq off-lm 64)			;offset to string 'lmResponse
    (setq off-nt (+ 64 24))		;offset to string 'ntResponse
    (setq off-d (+ 64 24 ln))		;offset to string 'uDomain
    (setq off-u (+ 64 24 ln (* 2 ld)))	;offset to string 'uUser
    (setq off-w (+ 64 24 ln (* 2 (+ ld lu)))) ;offset to string 'uWks
    (setq off-s (+ 64 24 ln (* 2 (+ ld lu lu)))) ;offset to string 'sessionKey
    ;; pack the response struct in a string
    (concat "NTLMSSP\0"			;response ident field, 8 bytes
	    (md4-pack-int32 '(0 . 3))	;response msgType field, 4 bytes

	    ;; lmResponse field, 8 bytes
	    ;;AddBytes(response,lmResponse,lmRespData,24);
	    (md4-pack-int16 24)		;len field
	    (md4-pack-int16 24)		;maxlen field
	    (md4-pack-int32 (cons 0 off-lm)) ;field offset

	    ;; ntResponse field, 8 bytes
	    ;;AddBytes(response,ntResponse,ntRespData,ln);
	    (md4-pack-int16 ln)	;len field
	    (md4-pack-int16 ln)	;maxlen field
	    (md4-pack-int32 (cons 0 off-nt)) ;field offset

	    ;; uDomain field, 8 bytes
	    ;;AddUnicodeString(response,uDomain,domain);
	    ;;AddBytes(response, uDomain, udomain, 2*ld);
	    (md4-pack-int16 (* 2 ld))	;len field
	    (md4-pack-int16 (* 2 ld))	;maxlen field
	    (md4-pack-int32 (cons 0 off-d)) ;field offset

	    ;; uUser field, 8 bytes
	    ;;AddUnicodeString(response,uUser,u);
	    ;;AddBytes(response, uUser, uuser, 2*lu);
	    (md4-pack-int16 (* 2 lu))	;len field
	    (md4-pack-int16 (* 2 lu))	;maxlen field
	    (md4-pack-int32 (cons 0 off-u)) ;field offset

	    ;; uWks field, 8 bytes
	    ;;AddUnicodeString(response,uWks,u);
	    (md4-pack-int16 (* 2 lu))	;len field
	    (md4-pack-int16 (* 2 lu))	;maxlen field
	    (md4-pack-int32 (cons 0 off-w)) ;field offset

	    ;; sessionKey field, 8 bytes
	    ;;AddString(response,sessionKey,NULL);
	    (md4-pack-int16 0)		;len field
	    (md4-pack-int16 0)		;maxlen field
	    (md4-pack-int32 (cons 0 (- off-s off-lm))) ;field offset

	    ;; flags field, 4 bytes
	    flags			;

	    ;; buffer field
	    lmRespData			;lmResponse, 24 bytes
	    ntRespData			;ntResponse, 24 bytes
	    (ntlm-ascii2unicode domain	;Unicode domain string, 2*ld bytes
				(length domain)) ;
	    (ntlm-ascii2unicode user	;Unicode user string, 2*lu bytes
				(length user)) ;
	    (ntlm-ascii2unicode user	;Unicode user string, 2*lu bytes
				(length user)) ;
	    )))

(provide 'exco-ntlm)

;;; exco-ntlm.el ends here
