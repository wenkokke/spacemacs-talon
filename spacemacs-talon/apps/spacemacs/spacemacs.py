from talon import Context, actions, fs, ui, Module, app, clip, scope

import os

ctx = Context()
ctx.matches = r"""
app: Emacs
and title: /Spacemacs/
"""

mod = Module()
mod.apps.spacemacs = """
os: mac
and app.bundle: org.gnu.Emacs
and title: /Spacemacs/
"""

TALON_DIRECTORY = os.path.expanduser("~/.talon")
SPACEMACS_INFO_DIRECTORY = os.path.join(TALON_DIRECTORY, 'spacemacs-info')

fs.watch(SPACEMACS_INFO_DIRECTORY, lambda path, flags: print(path, flags))
