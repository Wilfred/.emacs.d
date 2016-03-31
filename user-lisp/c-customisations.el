(require 'flycheck)

;; todo: show eldoc in modeline, since the minibuffer is used by
;; flycheck.
(add-hook 'c-mode-hook #'c-turn-on-eldoc-mode)

;; TODO: this doesn't belong here.
(require 'which-func)
(setq which-func-modes (list #'c-mode))
(which-function-mode)

;; C++
(add-hook
 'c++-mode-hook
 (lambda ()
   (setq flycheck-clang-language-standard "c++11"
         flycheck-clang-definitions '("__STDC_LIMIT_MACROS"
                                      "__STDC_CONSTANT_MACROS"))))

(require 's)
(require 'dash)

(defvar wh/pkgconfig-libs nil)

(defun wh/s-ignore-case-less-p (s1 s2)
  (string< (downcase s1) (downcase s2)))

(defun wh/flycheck-add-c-includes ()
  "Configure flycheck to use additional includes
when checking the current buffer."
  (interactive)
  ;; Find out all the libraries installed on this system.
  (unless wh/pkgconfig-libs
    (let* ((all-libs-with-names
            (shell-command-to-string "pkg-config --list-all"))
           (lines (s-split "\n" (s-trim all-libs-with-names)))
           (libs (--map (-first-item (s-split " " it)) lines)))
      (setq wh/pkgconfig-libs (-sort #'wh/s-ignore-case-less-p libs))))

  (let* (;; Prompt for a library name.
         (lib-name (completing-read "Library name: " wh/pkgconfig-libs))
         ;; Find the include flags, e.g. "-I/usr/lib/foo"
         (pkgconfig-cmd (format "pkg-config --cflags %s" lib-name))
         (cc-args (s-trim (shell-command-to-string pkgconfig-cmd))))
    (if (s-starts-with? "-I" cc-args)
        ;; pkg-config has found a library with this name.
        (let* ((include-args (s-split " " cc-args))
               (lib-paths (--map (s-chop-prefix "-I" it) include-args)))
          ;; Only set in this buffer.
          (make-local-variable 'flycheck-clang-include-path)
          ;; Add include paths to `flycheck-clang-include-path' unless
          ;; already present.
          (setq flycheck-clang-include-path
                (-union flycheck-clang-include-path
                        lib-paths))
          (message "flycheck-clang-include-path: %s"
                   flycheck-clang-include-path))
      ;; Otherwise, no such library with this name.
      (user-error cc-args))))

(provide 'c-customisations)
