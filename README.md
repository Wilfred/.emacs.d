This is the Emacs environment I (Wilfred Hughes) use for development.

I'm aiming to keep this as portable as possible, and
self-contained. Code I've written (init.el and files under
`user-lisp/`) is BSD licensed, see COPYING.

This code is primarily tested with Emacs 24.X, but bug fixes for other
Emacs versions (or other Emacsen) are welcomed.

### Conventions

Any code in user-lisp is written by me. `*-customisations.el` are
interactive commands, configuration and key bindings. `*-utils.el` are
elisp convenience functions.

Where possible, functions are depended on using `autoload` instead of
`require`. Macros are depended on using `(eval-when-compile (require 'foo))`.
 Variables are also depended on by the function using `autoload`.

### Installing

Remove any existing .emacs.d configuration (or move elsewhere):

    $ rm -r ~/.emacs.d
    
Clone the repo:

    $ git clone git://github.com/Wilfred/.emacs.d.git
    
### Major packages written by me

I've moved the following packages to their own repos. These packages
have been properly documented and don't depend on any convenience
functions I've written.

* [flymake-jshint](https://github.com/Wilfred/flymake-jshint.el)
* [ag.el](https://github.com/Wilfred/ag.el)
* [loop.el](https://github.com/Wilfred/loop.el)

### Major third party packages

Third party packages are installed from MELPA or Marmalade, but stored
locally in the repo so there are no external dependencies for this
repo. These are stored in `elpa/`.

Package that haven't been written by me but aren't available as
package yet live in `third-party-lisp`. These include:

* [dired+](http://www.emacswiki.org/cgi-bin/wiki/dired+.el)
* [csv-mode](http://centaur.maths.qmul.ac.uk/Emacs/)
* [color-theme](http://www.nongnu.org/color-theme/) (note Tango theme is
  not part of the default collection)

### External tools required

* [Pyflakes](https://github.com/kevinw/pyflakes) (note not the PyPI
  version, since that crashes on unfinished dicts: `{foo}`)
* [JSHint](https://github.com/jshint/jshint/) (requires node.js to be installed)
* [Jedi](https://github.com/davidhalter/jedi) (use `pip install -r elpa/jedi/requirements.txt`)
