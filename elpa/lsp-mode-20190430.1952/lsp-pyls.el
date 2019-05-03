;;; lsp-pyls.el --- pyls configuration               -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Ivan Yonchovski

;; Author: Ivan Yonchovski <yyoncho@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; PYLS configuration

;;; Code:

(require 'lsp-mode)

(defgroup lsp-pyls nil
  "Settings for pyls."
  :group 'tools
  :tag "Language Server")

(defcustom lsp-clients-python-library-directories '("/usr/")
  "List of directories which will be considered to be libraries."
  :risky t
  :type '(repeat string))

(defcustom lsp-clients-python-command '("pyls")
  "PYLS command."
  :risky t
  :type 'list)

(defcustom lsp-pyls-configuration-sources ["pycodestyle"]
  "List of configuration sources to use."
  :type '(repeat string))

(defcustom lsp-pyls-plugins-jedi-completion-enabled t
  "Enable or disable the plugin."
  :type 'boolean)

(defcustom lsp-pyls-plugins-jedi-completion-include-params t
  "Auto-completes methods and classes with tabstops for each
parameter."
  :type 'boolean)

(defcustom lsp-pyls-plugins-jedi-definition-enabled t "Enable or disable the plugin." :type 'boolean)

(defcustom lsp-pyls-plugins-jedi-definition-follow-imports t
  "The goto call will follow imports."
  :type 'boolean)

(defcustom lsp-pyls-plugins-jedi-definition-follow-builtin-imports t
  "If follow_imports is True will decide if it follow builtin
imports."

  :type 'boolean)

(defcustom lsp-pyls-plugins-jedi-hover-enabled t
  "Enable or disable the plugin."
  :type 'boolean)

(defcustom lsp-pyls-plugins-jedi-references-enabled t
  "Enable or disable the plugin."
  :type 'boolean)

(defcustom lsp-pyls-plugins-jedi-signature-help-enabled t
  "Enable or disable the plugin."
  :type 'boolean)

(defcustom lsp-pyls-plugins-jedi-symbols-enabled t
  "Enable or disable the plugin."
  :type 'boolean)

(defcustom lsp-pyls-plugins-jedi-symbols-all-scopes t
  "If True lists the names of all scopes instead of only the
module namespace."
  :type 'boolean)
(defcustom lsp-pyls-plugins-mccabe-enabled t
  "Enable or disable the plugin."
  :type 'boolean)

(defcustom lsp-pyls-plugins-mccabe-threshold 15
  "The minimum threshold that triggers warnings about cyclomatic
complexity."
  :type 'number)

(defcustom lsp-pyls-plugins-preload-enabled t
  "Enable or disable the plugin."
  :type 'boolean)

(defcustom lsp-pyls-plugins-preload-modules nil
  "List of modules to import on startup"
  :type '(repeat string))

(defcustom lsp-pyls-plugins-pylint-enabled t
  "Enable or disable the plugin."
  :type 'boolean)

(defcustom lsp-pyls-plugins-pycodestyle-enabled t
  "Enable or disable the plugin."
  :type 'boolean)

(defcustom lsp-pyls-plugins-pycodestyle-exclude nil
  "Exclude files or directories which match these patterns."
  :type '(repeat string))

(defcustom lsp-pyls-plugins-pycodestyle-filename nil
  "When parsing directories, only check filenames matching these
patterns."
  :type '(repeat string))

(defcustom lsp-pyls-plugins-pycodestyle-select nil
  "Select errors and warnings"
  :type '(repeat string))

(defcustom lsp-pyls-plugins-pycodestyle-ignore nil
  "Ignore errors and warnings"
  :type '(repeat string))

(defcustom lsp-pyls-plugins-pycodestyle-hang-closing nil
  "Hang closing bracket instead of matching indentation of
opening bracket's line."
  :type 'boolean)

(defcustom lsp-pyls-plugins-pycodestyle-max-line-length nil
  "Set maximum allowed line length."
  :type 'number)

(defcustom lsp-pyls-plugins-pydocstyle-enabled nil
  "Enable or disable the plugin."
  :type 'boolean)

(defcustom lsp-pyls-plugins-pydocstyle-convention nil
  "Choose the basic list of checked errors by specifying an
existing convention."
  :type '(choice (:tag "pep257" "numpy")))

(defcustom lsp-pyls-plugins-pydocstyle-add-ignore nil
  "Ignore errors and warnings in addition to the specified
convention."
  :type '(repeat string))

(defcustom lsp-pyls-plugins-pydocstyle-add-select nil
  "Select errors and warnings in addition to the specified
convention."
  :type '(repeat string))

(defcustom lsp-pyls-plugins-pydocstyle-ignore nil
  "Ignore errors and warnings"
  :type '(repeat string))

(defcustom lsp-pyls-plugins-pydocstyle-select nil
  "Select errors and warnings"
  :type '(repeat string))

(defcustom lsp-pyls-plugins-pydocstyle-match "(?!test_).*\\.py"
  "Check only files that exactly match the given regular
expression; default is to match files that don't start with
'test_' but end with '.py'."
  :type 'string)

(defcustom lsp-pyls-plugins-pydocstyle-match-dir "[^\\.].*"
  "Search only dirs that exactly match the given regular
expression; default is to match dirs which do not begin with a
dot."
  :type 'string)

(defcustom lsp-pyls-plugins-pyflakes-enabled t
  "Enable or disable the plugin."
  :type 'boolean)

(defcustom lsp-pyls-plugins-rope-completion-enabled t
  "Enable or disable the plugin."
  :type 'boolean)

(defcustom lsp-pyls-plugins-yapf-enabled t
  "Enable or disable the plugin."
  :type 'boolean)

(defcustom lsp-pyls-rope-extension-modules nil
  "Builtin and c-extension modules that are allowed to be
imported and inspected by rope."
  :type 'string)

(defcustom lsp-pyls-rope-rope-folder nil
  "The name of the folder in which rope stores project
configurations and data. Pass `null` for not using such a folder
at all."
  :type '(repeat string))

(lsp-register-custom-settings
 '(("pyls.rope.ropeFolder" lsp-pyls-rope-rope-folder)
   ("pyls.rope.extensionModules" lsp-pyls-rope-extension-modules)
   ("pyls.plugins.yapf.enabled" lsp-pyls-plugins-yapf-enabled t)
   ("pyls.plugins.rope_completion.enabled" lsp-pyls-plugins-rope-completion-enabled t)
   ("pyls.plugins.pyflakes.enabled" lsp-pyls-plugins-pyflakes-enabled t)
   ("pyls.plugins.pydocstyle.matchDir" lsp-pyls-plugins-pydocstyle-match-dir)
   ("pyls.plugins.pydocstyle.match" lsp-pyls-plugins-pydocstyle-match)
   ("pyls.plugins.pydocstyle.select" lsp-pyls-plugins-pydocstyle-select)
   ("pyls.plugins.pydocstyle.ignore" lsp-pyls-plugins-pydocstyle-ignore)
   ("pyls.plugins.pydocstyle.addSelect" lsp-pyls-plugins-pydocstyle-add-select)
   ("pyls.plugins.pydocstyle.addIgnore" lsp-pyls-plugins-pydocstyle-add-ignore)
   ("pyls.plugins.pydocstyle.convention" lsp-pyls-plugins-pydocstyle-convention)
   ("pyls.plugins.pydocstyle.enabled" lsp-pyls-plugins-pydocstyle-enabled t)
   ("pyls.plugins.pycodestyle.maxLineLength" lsp-pyls-plugins-pycodestyle-max-line-length)
   ("pyls.plugins.pycodestyle.hangClosing" lsp-pyls-plugins-pycodestyle-hang-closing t)
   ("pyls.plugins.pycodestyle.ignore" lsp-pyls-plugins-pycodestyle-ignore)
   ("pyls.plugins.pycodestyle.select" lsp-pyls-plugins-pycodestyle-select)
   ("pyls.plugins.pycodestyle.filename" lsp-pyls-plugins-pycodestyle-filename)
   ("pyls.plugins.pycodestyle.exclude" lsp-pyls-plugins-pycodestyle-exclude)
   ("pyls.plugins.pycodestyle.enabled" lsp-pyls-plugins-pycodestyle-enabled t)
   ("pyls.plugins.pylint.enabled" lsp-pyls-plugins-pylint-enabled t)
   ("pyls.plugins.preload.modules" lsp-pyls-plugins-preload-modules)
   ("pyls.plugins.preload.enabled" lsp-pyls-plugins-preload-enabled t)
   ("pyls.plugins.mccabe.threshold" lsp-pyls-plugins-mccabe-threshold)
   ("pyls.plugins.mccabe.enabled" lsp-pyls-plugins-mccabe-enabled t)
   ("pyls.plugins.jedi_symbols.all_scopes" lsp-pyls-plugins-jedi-symbols-all-scopes t)
   ("pyls.plugins.jedi_symbols.enabled" lsp-pyls-plugins-jedi-symbols-enabled t)
   ("pyls.plugins.jedi_signature_help.enabled" lsp-pyls-plugins-jedi-signature-help-enabled t)
   ("pyls.plugins.jedi_references.enabled" lsp-pyls-plugins-jedi-references-enabled t)
   ("pyls.plugins.jedi_hover.enabled" lsp-pyls-plugins-jedi-hover-enabled t)
   ("pyls.plugins.jedi_definition.follow_builtin_imports" lsp-pyls-plugins-jedi-definition-follow-builtin-imports t)
   ("pyls.plugins.jedi_definition.follow_imports" lsp-pyls-plugins-jedi-definition-follow-imports t)
   ("pyls.plugins.jedi_definition.enabled" lsp-pyls-plugins-jedi-definition-enabled t)
   ("pyls.plugins.jedi_completion.include_params" lsp-pyls-plugins-jedi-completion-include-params t)
   ("pyls.plugins.jedi_completion.enabled" lsp-pyls-plugins-jedi-completion-enabled t)
   ("pyls.configurationSources" lsp-pyls-configuration-sources)))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda () lsp-clients-python-command))
                  :major-modes '(python-mode)
                  :priority -1
                  :server-id 'pyls
                  :library-folders-fn (lambda (_workspace) lsp-clients-python-library-directories)
                  :initialized-fn (lambda (workspace)
                                    (with-lsp-workspace workspace
                                      (lsp--set-configuration (lsp-configuration-section "pyls"))))))

(provide 'lsp-pyls)
;;; lsp-pyls.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
