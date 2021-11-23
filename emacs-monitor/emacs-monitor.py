from talon import Module, ui, fs

import glob
import os

mod = Module()

emacs_monitor_directory = mod.setting(
    "emacs_monitor_directory",
    type=str,
    default=os.path.expanduser('~/.emacs.d/.cache/emacs-monitor/'),
    desc="The directory for emacs-monitor",
)

# Global variable holding the content of the Emacs monitor files:
emacs_monitor = {'emacs_monitor_running': False}

@mod.scope
def scope():
    """Enable or disable the monitor and update the monitor state."""
    global emacs_monitor
    if emacs_running():
        emacs_monitor_start()
    else:
        emacs_monitor_stop()
    return emacs_monitor


def emacs_running():
    """Check if Emacs is running."""
    return any(app.name == "Emacs" for app in ui.apps())


def emacs_monitor_key(path):
    """Convert Emacs Lisp symbol names to Python names."""
    return 'emacs_{}'.format(
        os.path.relpath(path, start=emacs_monitor_directory.get())
        .replace('-','_')
        .replace('/','_'))


def emacs_monitor_delete(path):
    """Delete a key from the Emacs monitor state by path."""
    global emacs_monitor
    emacs_monitor.pop(emacs_monitor_key(path), None)


def emacs_monitor_marshal(s):
    """Marshall the contents of an Emacs monitor file to a Python value."""
    vals = s.splitlines()

    # Convert Emacs Lisp 't' to Python 'True'
    vals = [v == 't' or v for v in vals]

    # Extract the singleton element:
    vals = vals[0] if len(vals) == 1 else vals

    return vals


def emacs_monitor_read(path):
    """Read an Emacs monitor file into the monitor state."""
    global emacs_monitor
    with open(path, 'r') as f:
        emacs_monitor[emacs_monitor_key(path)] = \
            emacs_monitor_marshal(f.read())


def emacs_monitor_read_all():
    """Read all Emacs monitor files into the monitor state."""
    for path in glob.iglob(os.path.join(emacs_monitor_directory.get(), '*')):
        emacs_monitor_read(path)


def emacs_monitor_update(path, flags):
    """Update the monitor state in response to a file system event."""
    if flags.exists:
        emacs_monitor_read(path)
    else:
        emacs_monitor_delete(path)
    scope.update()


def emacs_monitor_start():
    """Start the Emacs monitor and read the current state."""
    global emacs_monitor
    if not emacs_monitor['emacs_monitor_running']:
        emacs_monitor['emacs_monitor_running'] = True
        emacs_monitor_read_all()
        fs.watch(emacs_monitor_directory.get(), emacs_monitor_update)


def emacs_monitor_stop():
    """Stop the Emacs monitor and clear the monitor state."""
    global emacs_monitor
    if emacs_monitor['emacs_monitor_running']:
        emacs_monitor = {'emacs_monitor_running': False}
        fs.unwatch(emacs_monitor_directory.get(), emacs_monitor_update)

ui.register('ready', scope.update)
ui.register('app_launch', scope.update)
ui.register('app_close', scope.update)
