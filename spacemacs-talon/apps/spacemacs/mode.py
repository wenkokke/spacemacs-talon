from talon import actions, scope

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
    def enter_from_normal_mode(mode_name: str):
        """Enter the given mode_name from normal mode."""
        if   mode_name == 'normal'  : return
        elif mode_name == 'insert'  : actions.key('i')
        elif mode_name == 'replace' : actions.key('R')
        elif mode_name == 'visual'  : actions.key('v')
        elif mode_name == 'emacs'   : actions.key('ctrl-z')
        else:
            app.notify('Spacemacs: cannot enter {} mode'.format(mode_name))

    @staticmethod
    def exit_to_normal_mode(mode_name: str):
        """Exit the given mode to normal mode."""
        if   mode_name == 'normal'  : return
        elif mode_name == 'insert'  : actions.key('escape')
        elif mode_name == 'replace' : actions.key('escape')
        elif mode_name == 'visual'  : actions.key('escape')
        elif mode_name == 'emacs'   : actions.key('ctrl-z')
        else:
            app.notify('Spacemacs: cannot exit {} mode'.format(mode_name))

    def __init__(self, new_mode: str):
        """Create a temporary mode context manager.

           NOTE: The code executed within the scope of `with mode(..)`
                 should not change the mode.
        """
        self.old_mode = mode.get()
        self.new_mode = new_mode

    def __enter__(self):
        """Switch from the old to the new mode, via normal mode."""
        mode.exit_to_normal_mode(self.old_mode)
        mode.enter_from_normal_mode(self.new_mode)

    def __exit__(self, exn_type, exn_value, exn_traceback):
        """Switch from the new to the old mode, via normal mode."""
        mode.exit_to_normal_mode(self.new_mode)
        mode.enter_from_normal_mode(self.old_mode)
