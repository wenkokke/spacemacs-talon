from talon import actions, scope

# Helper for temporarily switching modes:
class mode:

    # TODO: support evilified and hybrid states

    @staticmethod
    def get():
        """Get the current mode."""
        return scope['user'].get('emacs_evil_state', None)

    @staticmethod
    def test(mode_name: str):
        """Check the current mode."""
        return mode.get() == mode_name

    @staticmethod
    def normal():
        """Create a context manager for normal mode."""
        return mode('normal')

    @staticmethod
    def visual():
        """Create a context manager for visual mode."""
        return mode('visual')

    @staticmethod
    def insert():
        """Create a context manager for insert mode."""
        return mode('insert')

    @staticmethod
    def replace():
        """Create a context manager for replace mode."""
        return mode('replace')

    @staticmethod
    def emacs():
        """Create a context manager for emacs mode."""
        return mode('emacs')

    @staticmethod
    def enter_normal():
        """Switch to normal mode."""
        if mode.test('emacs'):
            actions.key('ctrl-z')
        if not mode.test('normal'):
            actions.key('escape')

    @staticmethod
    def enter_visual(line=False):
        """Switch to visual mode."""
        if not mode.test('visual'):
            mode.normal()
            actions.key('V' if line else 'v')

    @staticmethod
    def enter_visual_line():
        """Switch to visual-line mode."""
        mode.enter_visual(line=True)

    @staticmethod
    def enter_replace():
        """Switch to replace mode."""
        if not mode.test('replace'):
            mode.normal()
            actions.key('R')

    @staticmethod
    def enter_insert():
        """Switch to insert mode."""
        if not mode.test('insert'):
            mode.normal()
            actions.key('i')

    @staticmethod
    def enter_emacs():
        """Switch to emacs mode."""
        if not mode.test('emacs'):
            actions.key('ctrl-z')

    @staticmethod
    def enter(mode_name: str):
        """Enter the given mode_name from normal mode."""
        {
            'normal'      : noop,
            'insert'      : mode.enter_insert,
            'replace'     : mode.enter_replace,
            'visual'      : mode.enter_visual,
            'visual-line' : mode.enter_visual_line,
            'emacs'       : mode.enter_emacs
        }.get(mode_name,noop)()

    def __init__(self, new_mode: str):
        """Create a temporary mode context manager."""
        self.old_mode = mode.get()
        self.new_mode = new_mode

    def __enter__(self):
        """Switch from the old to the new mode, via normal mode."""
        if self.old_mode != self.new_mode:
            mode.normal()
            mode.enter(self.new_mode)

    def __exit__(self, exn_type, exn_value, exn_traceback):
        """Switch from the new to the old mode, via normal mode."""
        if self.old_mode != self.new_mode:
            mode.normal()
            mode.enter(self.old_mode)
