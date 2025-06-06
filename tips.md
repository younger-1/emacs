# The Emacs Editor

> Emacs is the advanced, extensible, customizable, self-documenting editor.

## Important General Concepts
### Screen::              How to interpret what you see on the screen.
### User Input::          Kinds of input events (characters, function keys).
### Keys::                Key sequences: what you type to request editing action.
### Mouse Input::         Using the mouse and keypads.
### Commands::            Named functions run by key sequences to do editing.
### Other Input::         Input besides the mouse, keyboard and keypads.
### Entering Emacs::      Starting Emacs from the shell.
### Exiting::             Stopping or killing Emacs.

## Fundamental Editing Commands
### Basic::               The most basic editing commands.

‘M-r’
     Without moving the text on the screen, reposition point on the left
     margin of the center-most text line of the window; on subsequent
     consecutive invocations, move point to the left margin of the
     top-most line, the bottom-most line, and so forth, in cyclic order
     (‘move-to-window-line-top-bottom’).

### Minibuffer::          Entering arguments that are prompted for.
### M-x::                 Invoking commands by their names.
### Help::                Commands for asking Emacs about its commands.

## Important Text-Changing Commands
### Mark::                The mark: how to delimit a region of text.
### Killing::             Killing (cutting) and yanking (pasting) text.
### Registers::           Saving a text string or a location in the buffer.
### Display::             Controlling what text is displayed.
### Search::              Finding or replacing occurrences of a string.

You can also search multiple files under the control of ‘xref’ (*note
Identifier Search::) or through the Dired ‘A’ command (*note Operating
on Files::), or ask the ‘grep’ program to do it (*note Grep
Searching::).

#### Incremental Search::        Search happens as you type the string.

##### Repeat Isearch

After exiting a search, you can search for the same string again by
typing just ‘C-s C-s’.

To reuse earlier search strings, use the “search ring”.  The commands
‘M-p’ (‘isearch-ring-retreat’) and ‘M-n’ (‘isearch-ring-advance’) move
through the ring to pick a search string to reuse.  These commands leave
the selected search ring element in the minibuffer, where you can edit
it.

To edit the current search string in the minibuffer without replacing
it with items from the search ring, type ‘M-e’ (‘isearch-edit-string’)
or click ‘mouse-1’ in the minibuffer.

##### Isearch Yank::

> Commands that grab text into the search or else edit the search string.

‘C-w’ (‘isearch-yank-word-or-char’)

‘C-M-w’ (‘isearch-yank-symbol-or-char’) appends the next character or
symbol at point to the search string.

‘C-M-d’ (‘isearch-del-char’) deletes the last character from the
search string, and ‘C-M-y’ (‘isearch-yank-char’) appends the character
after point to the search string.  An alternative method to add the
character after point is to enter the minibuffer with ‘M-e’ (*note
Repeat Isearch::) and type ‘C-f’ or ‘<RIGHT>’ at the end of the search
string in the minibuffer.

To begin a new incremental search with the text near point yanked
into the initial search string, type ‘M-s M-.’ that runs the command
‘isearch-forward-thing-at-point’.

##### Error in Isearch::

> When your string is not found.

Or you can type ‘C-g’, which removes from the search string the
characters that could not be found (the ‘T’ in ‘FOOT’), leaving those
that were found (the ‘FOO’ in ‘FOOT’).  A second ‘C-g’ at that point
cancels the search entirely, returning point to where it was when the
search started.

##### Special Isearch::      Special input in incremental search.

To toggle lax space matching (*note lax space matching: Lax Search.), type ‘M-s <SPC>’.

To toggle case sensitivity of the search, type ‘M-c’ or ‘M-s c’.  If the search string includes upper-case letters, the search is case-sensitive by default.

To toggle whether or not the search will consider similar and equivalent characters as a match, type ‘M-s '’.

To toggle between non-regexp and regexp incremental search, type ‘M-r’ or ‘M-s r’ (‘isearch-toggle-regexp’).

To toggle symbol mode, type ‘M-s _’.

Typing ‘M-s o’ in incremental search invokes ‘isearch-occur’

Typing ‘M-%’ (‘isearch-query-replace’) in incremental search invokes ‘query-replace’ or ‘query-replace-regexp’ (depending on search mode)

Typing ‘M-<TAB>’ in incremental search invokes ‘isearch-complete’, which attempts to complete the search string using the search ring.  The completed string is then editable in the minibuffer.

You can exit the search while leaving the matches highlighted by typing ‘M-s h r’ (‘isearch-highlight-regexp’).

When incremental search is active, typing ‘M-s M->’ will go to the last occurrence of the search string, and ‘M-s M-<’ will go to the first occurrence.

##### Not Exiting Isearch::  Prefix argument and scrolling commands.
##### Isearch Minibuffer::   Incremental search of the minibuffer history.




### Fixit::               Commands especially useful for fixing typos.
### Keyboard Macros::     Recording a sequence of keystrokes to be replayed.

## Major Structures of Emacs
### Files::               All about handling files.
### Buffers::             Multiple buffers; editing several files at once.
### Windows::             Viewing multiple pieces of text in one frame.
### Frames::              Using multiple windows on your display.
### International::       Using non-ASCII character sets.

## Advanced Features
### Modes::               Major and minor modes alter Emacs’s basic behavior.
### Indentation::         Editing the white space at the beginnings of lines.
### Text::                Commands and modes for editing human languages.
### Programs::            Commands and modes for editing programs.
### Building::            Compiling, running and debugging programs.
### Maintaining::         Features for maintaining large programs.
### Abbrevs::             Defining text abbreviations to reduce typing.
### Dired::               Directory and file manager.
### Calendar/Diary::      Calendar and diary facilities.
### Sending Mail::        Sending mail in Emacs.
### Rmail::               Reading mail in Emacs.
### Gnus::                A flexible mail and news reader.
### Host Security::       Security issues on a single computer.
### Network Security::    Managing the network security.
### Document View::       Viewing PDF, PS and DVI files.
### Shell::               Executing shell commands from Emacs.
### Emacs Server::        Using Emacs as an editing server.
### Printing::            Printing hardcopies of buffers or regions.
### Sorting::             Sorting lines, paragraphs or pages within Emacs.
### Picture Mode::        Editing pictures made up of text characters.
### Editing Binary Files::  Editing binary files with Hexl mode.
### Saving Emacs Sessions:: Saving Emacs state from one session to the next.
### Recursive Edit::      Performing edits while within another command.
### Hyperlinking::        Following links in buffers.
### Amusements::          Various games and hacks.
### Packages::            Installing additional features.
### Customization::       Modifying the behavior of Emacs.

## Recovery from Problems
### Quitting::            Quitting and aborting.
### Lossage::             What to do if Emacs is hung or malfunctioning.
### Bugs::                How and when to report a bug.
### Contributing::        How to contribute improvements to Emacs.
### Service::             How to get help for your own Emacs needs.

## Appendices
### Glossary::            Terms used in this manual.
### Acknowledgments::     Major contributors to GNU Emacs.

## Indexes (each index contains a large menu)
### Key Index::           An item for each standard Emacs key sequence.
### Option Index::        An item for every command-line option.
### Command Index::       An item for each standard command name.
### Variable Index::      An item for each variable documented in this manual.
### Concept Index::       An item for concepts and other general subjects.
