This is the Emacs environment I (Wilfred Hughes) use for development.

I'm aiming to keep this as portable as possible, and
self-contained. Code I've written (init.el and the *-customisations.el
files) is BSD licensed.

### Major third party packages

To avoid problems where code becomes abandoned or changes hosting,
third party packages are included directly rather than as git
submodules.

* Magit
* markdown-mode
* auto-complete
* dired+
* csv-mode
* zencoding-mode
* color-theme
* autopair

### Flymake tools

* [Pyflakes](https://github.com/kevinw/pyflakes) (note not the PyPI version, since that crashes on unfinished dicts: {foo})
* JSHint (requires node.js installed)
