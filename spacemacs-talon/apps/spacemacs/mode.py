from talon import actions, scope
from functools import partial

# Helper for temporarily switching modes:
class mode:

    # TODO: support evilified and hybrid states

    @staticmethod
    def get():
        """Get the current mode."""
        return scope['user'].get('emacs_evil_state', None)

    @staticmethod
    def set(mode_name: str):
        """Enter the given mode."""
        mode.exit_to_normal_mode(mode.get())
        mode.enter_from_normal_mode(mode_name)

    @staticmethod
    def test(mode_name: str):
        """Check the current mode."""
        return mode.get() == mode_name

    @staticmethod
    def enter_visual_mode(line=False):
        if not test('visual'):
            if line:
                actions.key('V')
            else:
                actions.key('v')

    @staticmethod
    def leave_visual_mode():
        if test('visual'):
            actions.key('escape')

    @staticmethod
    def enter_replace_mode():
        if not test('replace'):
            actions.key('i')

    @staticmethod
    def leave_replace_mode():
        if test('replace'):
            actions.key('escape')

    @staticmethod
    def enter_insert_mode():
        if not test('insert'):
            actions.key('i')

    @staticmethod
    def leave_insert_mode():
        if test('insert'):
            actions.key('escape')

    @staticmethod
    def enter_emacs_mode():
        if not test('emacs'):
            actions.key('ctrl-z')

    @staticmethod
    def leave_emacs_mode():
        if test('emacs'):
            actions.key('ctrl-z')

    @staticmethod
    def enter_unknown_mode(mode_name: str):
        app.notify('Spacemacs: cannot enter {} mode'.format(mode_name))

    @staticmethod
    def leave_unknown_mode(mode_name: str):
        app.notify('Spacemacs: cannot leave {} mode'.format(mode_name))

    @staticmethod
    def enter_from_normal_mode(mode_name: str):
        """Enter the given mode_name from normal mode."""
        {
            'normal'      : noop,
            'insert'      : enter_insert_mode,
            'replace'     : enter_replace_mode,
            'visual'      : enter_visual_mode,
            'visual-line' : partial(enter_visual_mode, line=True),
            'emacs'       : enter_emacs_mode
        }.get(mode_name,partial(mode.enter_unknown_mode,mode_name))()

    @staticmethod
    def exit_to_normal_mode(mode_name: str):
        """Exit the given mode to normal mode."""
        {
            'normal'      : noop,
            'insert'      : leave_insert_mode,
            'replace'     : leave_replace_mode,
            'visual'      : leave_visual_mode,
            'visual-line' : leave_visual_mode,
            'emacs'       : leave_emacs_mode
        }.get(mode_name,partial(mode.leave_unknown_mode,mode_name))()

    def __init__(self, new_mode: str):
        """Create a temporary mode context manager."""
        self.old_mode = mode.get()
        self.new_mode = new_mode

    def __enter__(self):
        """Switch from the old to the new mode, via normal mode."""
        if self.old_mode != self.new_mode:
            mode.exit_to_normal_mode(self.old_mode)
            mode.enter_from_normal_mode(self.new_mode)

    def __exit__(self, exn_type, exn_value, exn_traceback):
        """Switch from the new to the old mode, via normal mode."""
        if self.old_mode != self.new_mode:
            mode.exit_to_normal_mode(self.new_mode)
            mode.enter_from_normal_mode(self.old_mode)
