from talon import Context, actions, fs, ui, Module, app, clip, scope

import glob
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


# Initialise and update the editor state based on the files written by
# the talon-integration package
#
# NOTE: ensure that the variable 'SPACEMACS_INTEGRATION_ROOT' is set
#       to the same value as the the variable 'talon-integration-root'
#       in Spacemacs. To check the value of this variable, use:
#
#       SPC SPC described-variable talon-integration-root
#
SPACEMACS_INTEGRATION_ROOT = \
    os.path.expanduser("~/.emacs.d/.cache/spacemacs-monitor/")

# This is a GLOBAL variable which holds the current editor state:
SPACEMACS = {}

def spacemacs_integration_read(path):
    """Update the editor state by reading the specified file"""
    global SPACEMACS
    relative_path = os.path.relpath(path, start=SPACEMACS_INTEGRATION_ROOT)
    with open(path, 'r') as f:
        SPACEMACS[relative_path] = f.read()

def spacemacs_integration_init():
    """Initialise the editor state by reading all files"""
    for path in glob.iglob(os.path.join(SPACEMACS_INTEGRATION_ROOT, '*')):
        spacemacs_integration_read(path)

# Initialise the editor state when this file is reloaded:
spacemacs_integration_init()

def spacemacs_integration_update(path, flags):
    """Update the editor state in response to a file system event"""
    global SPACEMACS
    if flags.exists:
        spacemacs_integration_read(path)
    else:
        SPACEMACS.pop(relative_path, None)

fs.watch(SPACEMACS_INTEGRATION_ROOT, spacemacs_integration_update)


def spacemacs_mode():
    """Get the current mode"""
    global SPACEMACS
    return SPACEMACS.get('editor-state', None)


# Select the language mode based on the currently visited file
@ctx.action_class("win")
class WinActions:
    def filename():
        global SPACEMACS
        return SPACEMACS.get('file-name', '')

# Actions
@ctx.action_class("user")
class UserActions:

    def spacemacs_insert_mode():
        """Change to insert mode"""
        print(actions.spacemacs_mode())
        pass

    def spacemacs_normal_mode():
        """Change to normal mode"""
        pass
