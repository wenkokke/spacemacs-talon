from talon import Context, actions, fs, ui, Module, app, clip, scope

from .mode import mode

mod = Module()
mod.apps.spacemacs = """
os: mac
and app.bundle: org.gnu.Emacs
and title: /Spacemacs/
"""

ctx = Context()
ctx.matches = r"""
app: spacemacs
"""


@mod.action_class
class EmacsActions:
    def emacs_eval(expr: str):
        """Send Emacs Lisp expression to the editor."""
        actions.key('alt-:')
        actions.insert(expr)
        actions.key('enter')


@ctx.action_class("win")
class WinActions:
    def filename():
        """Select the language mode based on the currently visited file."""
        return scope['user'].get('emacs_buffer_file_name','')


@ctx.action_class("edit")
class EditActions:

    # NOTE: use the OS defaults because they work in any mode
    #
    # def left():
    #   """Move cursor left one column"""
    #
    # def right():
    #   """Move cursor right one column"""
    #
    # def up():
    #   """Move cursor up one row"""
    #
    # def down():
    #   """Move cursor down one row"""
    #
    # def copy():
    #   """Copy selection to clipboard"""
    #
    # def cut():
    #   """Cut selection to clipboard"""
    #
    # def paste():
    #   """Paste clipboard at cursor"""
    #
    # def paste_match_style():
    #   """Paste clipboard without style information"""

    def delete():
        """Delete selection"""
        with mode.visual():
            actions.key('d')

    def delete_line():
        """Delete line under cursor"""
        with mode.normal():
            actions.insert('d d')

    def delete_paragraph():
        """Delete paragraph under cursor"""
        pass

    def delete_sentence():
        """Delete sentence under cursor"""
        pass

    def delete_word():
        """Delete word under cursor"""
        pass

    def extend_again():
        """Extend selection again in the same way"""
        pass

    def extend_column(n: int):
        """Extend selection to column <n>"""
        pass

    def extend_down():
        """Extend selection down one row"""
        pass

    def extend_file_end():
        """Extend selection to end of file"""
        pass

    def extend_file_start():
        """Extend selection to start of file"""
        pass

    def extend_left():
        """Extend selection left one column"""
        pass

    def extend_line(n: int):
        """Extend selection to include line <n>"""
        pass

    def extend_line_down():
        """Extend selection down one full line"""
        pass

    def extend_line_end():
        """Extend selection to end of line"""
        pass

    def extend_line_start():
        """Extend selection to start of line"""
        pass

    def extend_line_up():
        """Extend selection up one full line"""
        pass

    def extend_page_down():
        """Extend selection down one page"""
        pass

    def extend_page_up():
        """Extend selection up one page"""
        pass

    def extend_paragraph_end():
        """Extend selection to the end of the current paragraph"""
        pass

    def extend_paragraph_next():
        """Extend selection to the start of the next paragraph"""
        pass

    def extend_paragraph_previous():
        """Extend selection to the start of the previous paragraph"""
        pass

    def extend_paragraph_start():
        """Extend selection to the start of the current paragraph"""
        pass

    def extend_right():
        """Extend selection right one column"""
        pass

    def extend_sentence_end():
        """Extend selection to the end of the current sentence"""
        pass

    def extend_sentence_next():
        """Extend selection to the start of the next sentence"""
        pass

    def extend_sentence_previous():
        """Extend selection to the start of the previous sentence"""
        pass

    def extend_sentence_start():
        """Extend selection to the start of the current sentence"""
        pass

    def extend_up():
        """Extend selection up one row"""
        pass

    def extend_word_left():
        """Extend selection left one word"""
        pass

    def extend_word_right():
        """Extend selection right one word"""
        pass

    def file_end():
        """Move cursor to end of file (start of line)"""
        pass

    def file_start():
        """Move cursor to start of file"""
        pass

    def find(text: str = None):
        """Open Find dialog, optionally searching for text"""
        pass

    def find_next():
        """Select next Find result"""
        pass

    def find_previous():
        """Select previous Find result"""
        pass

    def indent_less():
        """Remove a tab stop of indentation"""
        mode.enter_visual_line()
        actions.key('<')

    def indent_more():
        """Add a tab stop of indentation"""
        mode.enter_visual_line()
        actions.key('>')

    def jump_column(n: int):
        """Move cursor to column <n>"""
        pass

    def jump_line(n: int):
        """Move cursor to line <n>"""
        pass

    def line_clone():
        """Create a new line identical to the current line"""
        pass

    def line_down():
        """Move cursor to start of line below"""
        pass

    def line_end():
        """Move cursor to end of line"""
        pass

    def line_insert_down():
        """Insert line below cursor"""
        pass

    def line_insert_up():
        """Insert line above cursor"""
        pass

    def line_start():
        """Move cursor to start of line"""
        pass

    def line_swap_down():
        """Swap the current line with the line below"""
        pass

    def line_swap_up():
        """Swap the current line with the line above"""
        pass

    def line_up():
        """Move cursor to start of line above"""
        pass

    def move_again():
        """Move cursor again in the same way"""
        pass

    def page_down():
        """Move cursor down one page"""
        pass

    def page_up():
        """Move cursor up one page"""
        pass

    def paragraph_end():
        """Move cursor to the end of the current paragraph"""
        pass

    def paragraph_next():
        """Move cursor to the start of the next paragraph"""
        pass

    def paragraph_previous():
        """Move cursor to the start of the previous paragraph"""
        pass

    def paragraph_start():
        """Move cursor to the start of the current paragraph"""
        pass

    def print():
        """Open print dialog"""
        pass

    def redo():
        """Redo"""
        pass

    def save():
        """Save current document"""
        pass

    def save_all():
        """Save all open documents"""
        pass

    def select_all():
        """Select all text in the current document"""
        pass

    def select_line(n: int = None):
        """Select entire line <n>, or current line"""
        pass

    def select_lines(a: int, b: int):
        """Select entire lines from <a> to <b>"""
        pass

    def select_none():
        """Clear current selection"""
        pass

    def select_paragraph():
        """Select the entire nearest paragraph"""
        pass

    def select_sentence():
        """Select the entire nearest sentence"""
        pass

    def select_word():
        """Select word under cursor"""
        mode.enter_normal()
        actions.key('b')
        mode.enter_visual()
        actions.key('e')

    def selected_text() -> str:
        """Get currently selected text"""
        pass

    def selection_clone():
        """Insert a copy of the current selection"""
        pass

    def sentence_end():
        """Move cursor to the end of the current sentence"""
        pass

    def sentence_next():
        """Move cursor to the start of the next sentence"""
        pass

    def sentence_previous():
        """Move cursor to the start of the previous sentence"""
        pass

    def sentence_start():
        """Move cursor to the start of the current sentence"""
        pass

    def undo():
        """Undo"""
        pass

    def word_left():
        """Move cursor left one word"""
        pass

    def word_right():
        """Move cursor right one word"""
        pass

    def zoom_in():
        """Zoom in"""
        actions.insert('space z x + q')

    def zoom_out():
        """Zoom out"""
        actions.insert('space z x - q')

    def zoom_reset():
        """Zoom to original size"""
        actions.insert('space z x 0 q')


@ctx.action_class("code")
class CodeActions:

    def complete():
        """Trigger code autocomplete"""
        pass

    def extend_scope_end():
        """Extend selection to end of current scope"""
        pass

    def extend_scope_in():
        """Extend selection to start of first inner scope"""
        pass

    def extend_scope_next():
        """Extend selection to start of next sibling scope"""
        pass

    def extend_scope_out():
        """Extend selection to start of outer scope"""
        pass

    def extend_scope_previous():
        """Extend selection to start of previous sibling scope"""
        pass

    def extend_scope_start():
        """Extend selection to start of current scope"""
        pass

    # TODO: select language mode based on current emacs major mode
    #
    # def language() -> str:
    #   """Return the active programming language"""
    #   pass

    def rename(name: str):
        """Rename selection to <name>"""
        pass

    def scope_end():
        """Move cursor to end of current scope"""
        pass

    def scope_in():
        """Move cursor to start of first inner scope"""
        pass

    def scope_next():
        """Move cursor to start of next sibling scope"""
        pass

    def scope_out():
        """Move cursor to start of outer scope"""
        pass

    def scope_previous():
        """Move cursor to start of previous sibling scope"""
        pass

    def scope_start():
        """Move cursor to start of current scope"""
        pass

    def select_scope():
        """Select scope under cursor"""
        pass

    def toggle_comment():
        """Toggle comments on the current line(s)"""
        with mode.emacs():
            actions.key('ctrl-x')
            actions.key('ctrl-;')


@ctx.action_class("app")
class AppActions:


    # NOTE: tabs are emacs buffers

    def tab_close():
        """Close the current tab"""
        with mode.normal():
            actions.insert('space b d')

    def tab_detach():
        """Move the current tab to a new window"""
        with mode.normal():
            actions.insert('space w F')

    def tab_next():
        """Switch to next tab for this window"""
        with mode.normal():
            actions.insert('space b n')

    def tab_open():
        """Open a new tab"""
        with mode.normal():
            actions.insert('space b N n')

    def tab_previous():
        """Switch to previous tab for this window"""
        with mode.normal():
            actions.insert('space b p')

    def tab_reopen():
        """Re-open the last-closed tab"""
        with mode.normal():
            actions.insert('space b u')

    # NOTE: windows are emacs frames
    #
    # def window_close():
    #     """Close the current window"""
    # def window_hide():
    #     """Hide the current window"""
    # # def window_hide_others():
    #     """Hide all other windows"""
    # def window_next():
    #     """Switch to next window for this app"""
    # def window_open():
    #     """Open a new window"""
    # def window_previous():
    #     """Switch to previous window for this app"""


@ctx.action_class('self')
class SplitsActions:

    # NOTE: splits are emacs windows

    def split_window_right():
        """Move active tab to right split"""
        with mode.normal():
            actions.insert('space w l')

    def split_window_left():
        """Move active tab to left split"""
        with mode.normal():
            actions.insert('space w h')

    def split_window_down():
        """Move active tab to lower split"""
        with mode.normal():
            actions.insert('space w j')

    def split_window_up():
        """Move active tab to upper split"""
        with mode.normal():
            actions.insert('space w k')

    def split_window_vertically():
        """Splits window vertically"""
        with mode.normal():
            actions.insert('space w /')

    def split_window_horizontally():
        """Splits window horizontally"""
        with mode.normal():
            actions.insert('space w -')

    def split_flip():
        """Flips the orietation of the active split"""
        with mode.normal():
            actions.insert('space w +')

    def split_window():
        """Splits the window"""
        actions.split_window_vertically()

    def split_clear():
        """Clears the current split"""
        with mode.normal():
            actions.insert('space w d')

    def split_clear_all():
        """Clears all splits"""
        actions.user.emacs_eval('(delete-other-windows)')

    def split_next():
        """Goes to next split"""
        with mode.normal():
            actions.insert('space w w')

    def split_last():
        """Goes to last split"""

    def split_number(index: int):
        """Navigates to the specified split"""
        with mode.normal():
            actions.insert('space {}'.format(index))
