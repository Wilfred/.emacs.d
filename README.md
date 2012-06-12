This is the Emacs environment I (Wilfred Hughes) use for development.

I'm aiming to keep this as portable as possible, and
self-contained. Code I've written (init.el and files under
`user-lisp/`) is BSD licensed, see COPYING.

Emacs 23.X and Emacs 24.X are both tested with this code.

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
    
Clone the repo including submodules:

    $ git clone git://github.com/Wilfred/.emacs.d.git

### Major third party packages

To avoid problems where code becomes abandoned or changes hosting,
third party packages are included directly rather than as git
submodules.

* [Magit](https://github.com/magit/magit)
* [markdown-mode](http://jblevins.org/projects/markdown-mode/)
* [auto-complete](http://cx4a.org/software/auto-complete/)
* [dired+](http://www.emacswiki.org/cgi-bin/wiki/dired+.el)
* [csv-mode](http://centaur.maths.qmul.ac.uk/Emacs/)
* [zencoding-mode](https://github.com/rooney/zencoding)
* [color-theme](http://www.nongnu.org/color-theme/) (note Tango theme is
  not part of the default collection)
* autopair

### Flymake tools

* [Pyflakes](https://github.com/kevinw/pyflakes) (note not the PyPI version, since that crashes on unfinished dicts: {foo})
* [JSHint](https://github.com/jshint/jshint/) (requires node.js to be installed)
