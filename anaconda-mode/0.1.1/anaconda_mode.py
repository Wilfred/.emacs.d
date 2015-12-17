"""
    anaconda_mode
    ~~~~~~~~~~~~~

    This is anaconda_mode autocompletion server.

    :copyright: (c) 2013-2015 by Artem Malyshev.
    :license: GPL3, see LICENSE for more details.
"""

from __future__ import (
    absolute_import, unicode_literals, division, print_function)

import sys
from functools import wraps

from jedi import Script, NotFoundError
from service_factory import service_factory


def script_method(f):
    """Create jedi.Script instance and apply f to it."""

    @wraps(f)
    def wrapper(source, line, column, path):
        try:
            return f(Script(source, line, column, path))
        except NotFoundError:
            return []

    return wrapper


def process_definitions(f):
    """Call f and convert it result into json dumpable format."""

    @wraps(f)
    def wrapper(script):

        return [{'name': definition.name,
                 'type': definition.type,
                 'module-name': definition.module_name,
                 'module-path': definition.module_path,
                 'line': definition.line,
                 'column': definition.column,
                 'docstring': definition.docstring(),
                 'description': definition.description,
                 'full-name': definition.full_name}
                for definition in f(script)]

    return wrapper


@script_method
@process_definitions
def complete(script):
    """Select auto-complete candidates for source position."""

    return script.completions()


@script_method
@process_definitions
def goto_definitions(script):
    """Get definitions for thing under cursor."""

    return script.goto_definitions()


@script_method
@process_definitions
def goto_assignments(script):
    """Get assignments for thing under cursor."""

    return script.goto_assignments()


@script_method
@process_definitions
def usages(script):
    """Get usage information for thing under cursor."""

    return script.usages()


@script_method
def eldoc(script):
    """Return eldoc format documentation string or ''."""

    signatures = script.call_signatures()
    if len(signatures) == 1:
        signature = signatures[0]
        return {
            'name': signature.name,
            'index': signature.index,
            'params': [param.description for param in signature.params]
        }


app = [complete, goto_definitions, goto_assignments, usages, eldoc]


if __name__ == '__main__':
    host = sys.argv[1] if len(sys.argv) == 2 else '127.0.0.1'
    service_factory(app, host, 'auto', 'anaconda_mode port {port}')
