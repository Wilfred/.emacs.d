;;; lush-theme.el --- A dark theme with strong colors
;;
;; Filename: lush-theme.el
;; Author: Andre Richter <andre.o.richter@gmail.com>
;; Version: 20140922.1117
;; X-Original-Version: 0.1
;; URL: https://github.com/andre-richter/emacs-lush-theme
;; Package-Requires: ((emacs "24"))
;; Keywords: theme, dark, strong colors
;;
;;; Commentary:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lush, A Dark Theme with strong colors for Emacs24
;;
;; Change History:
;;   Andre Richter, 2014-09:
;;     * Changed colors for personal taste
;;     * Made Emacs24 out-of-the-box compatible
;;
;;
;; This theme is based on JD Huntington's Blackboard theme.
;; Following is the original info from the theme (with obsolete install instructions):
;;
;;   Blackboard Colour Theme for Emacs.
;;
;;   Defines a colour scheme resembling that of the original TextMate Blackboard colour theme.
;;   To use add the following to your .emacs file (requires the color-theme package):
;;
;;   (require 'color-theme)
;;   (color-theme-initialize)
;;   (load-file "~/.emacs.d/themes/color-theme-blackboard.el")
;;
;;   And then (color-theme-blackboard) to activate it.
;;
;;   MIT License Copyright (c) 2008 JD Huntington <jdhuntington at gmail dot com>
;;   Credits due to the excellent TextMate Blackboard theme
;;
;;   All patches welcome
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(deftheme lush
  "Dark color theme adapted from JD Huntington's TextMate Blackboard theme by Andre Richter.
See https://github.com/andre-richter/emacs-lush-theme")

(let* ((lush/background "#202020")
       (lush/foreground "#E0E0E0")
       (lush/turquoise  "#2AA198")
       (lush/orange     "#FF951B")
       (lush/pink       "#FF88FF")
       (lush/yellow     "#FFE329")
       (lush/green      "#61CE3C")
       (lush/light-blue "#82A6DF")
       (lush/dark-blue  "#253B76")
       (lush/light-red  "#FA583F"))

  (custom-theme-set-faces
   `lush
   `(bold                                ((t (:bold t))))
   `(bold-italic                         ((t (:bold t))))
   `(border-glyph                        ((t (nil))))
   `(default                             ((t (:foreground ,lush/foreground :background ,lush/background))))  
   `(buffers-tab                         ((t (:foreground ,lush/foreground :background ,lush/background))))
   `(font-lock-builtin-face              ((t (:foreground ,lush/foreground))))
   `(font-lock-comment-face              ((t (:foreground ,lush/turquoise :italic t))))
   `(font-lock-constant-face             ((t (:foreground ,lush/orange))))
   `(font-lock-doc-string-face           ((t (:foreground "DarkOrange"))))
   `(font-lock-function-name-face        ((t (:foreground ,lush/pink))))
   `(font-lock-keyword-face              ((t (:foreground ,lush/yellow))))
   `(font-lock-preprocessor-face         ((t (:foreground "Aquamarine"))))
   `(font-lock-reference-face            ((t (:foreground "SlateBlue"))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground "#E9C062"))))
   `(font-lock-regexp-grouping-construct ((t (:foreground "red"))))
   `(font-lock-string-face               ((t (:foreground ,lush/green))))
   `(font-lock-type-face                 ((t (:foreground ,lush/light-blue))))
   `(font-lock-variable-name-face        ((t (:foreground ,lush/light-red))))
   `(font-lock-warning-face              ((t (:foreground "Pink"     :bold t))))
   `(gui-element                         ((t (:foreground "black"    :background "#D4D0C8"))))  
   `(mode-line                           ((t (:foreground "#F0F0F0"  :background "#444444" :box nil))))
   `(mode-line-highlight                 ((t (:foreground ,lush/pink :box nil))))
   `(text-cursor                         ((t (:foreground "black"    :background "yellow"))))
   `(zmacs-region                        ((t (:foreground "ble"      :background "snow"))))
   `(region                              ((t (:background ,lush/dark-blue))))
   `(highlight                           ((t (:background "#222222"))))
   `(highline-face                       ((t (:background "SeaGreen"))))
   `(italic                              ((t (nil))))
   `(left-margin                         ((t (nil))))  
   `(toolbar                             ((t (nil))))
   `(underline                           ((nil (:underline nil))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
	       (file-name-as-directory (file-name-directory load-file-name))))

;;;###autoload
(defun lush-theme()
  "Load lush-theme."
  (interactive)
  (load-theme 'lush t))

(provide-theme 'lush)

;;; lush-theme.el ends here
