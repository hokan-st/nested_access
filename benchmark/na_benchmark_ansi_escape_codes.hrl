%% -----------------------------------------------------------------------------
%% ANSI escape codes.
%%
%% Use these to format text sent to the shell and to control where text is
%% drawn in the shell. The shells terminal is essentially a 2D character grid
%% similar to a regular pixel buffer.
%% By moving the input/drawing cursor we can implement things like an animated
%% progress-bar.
%%
%% Note: ANSI escape codes work on UNIX like systems like Linux and macOS.
%%       according to https://en.wikipedia.org/wiki/ANSI_escape_code#Platform_support
%%       they also work on Windows 10.
%% Note: the current escape codes assume a xterm-256color terminal (this is
%%       macOS default).
%% Note: based on https://jvns.ca/blog/2025/03/07/escape-code-standards/
%%       and other sources, it seems that xterm support is the de facto standard
%%
%% source: https://en.wikipedia.org/wiki/ANSI_escape_code
%%
%% -----------------------------------------------------------------------------
%% Formatting codes
%%
%% Note: formatting on the same line looks as expected.
%% Note: pad text with spaces if a bg colour should span a specific number of columns.
%% Note: formatting seems to be applied inconsistently when text contains
%%       newlines, sometimes bg colour stops at the newline sometimes it extends
%%       to the end of the line (in the macOS Terminal app).
%% -----------------------------------------------------------------------------

%% undo formatting (style & color)
-define(RESET, "\e[0m").

%% bold text
-define(BOLD, "\e[1m").

%% pick bg colour from colour table, light grey
-define(BG_TITLE, "\e[48;5;253m").

%% pick bg colour from colour table, yellow
-define(BG_NOTE, "\e[48;5;228m").

%% bg colour progress-bar
-define(PROGRESSBAR_TODO, "\e[48;5;253m"). % light grey
-define(PROGRESSBAR_DONE, "\e[48;5;118m"). % light green

%% -----------------------------------------------------------------------------
%% Codes to control (drawing) cursor
%%
%% Note: top left of shell/terminal window is cursor pos 1,1 (row:column)
%% -----------------------------------------------------------------------------

%% Current row becomes top row
%% Note: this may or may not move the cursor to the left (1,1)
-define(CLEAR_SCREEN, "\e[2J").

-define(MOVE_CURSOR(ROW, COL), "\e[" ++ integer_to_list(ROW) ++ ";" ++ integer_to_list(COL) ++ "H").

-define(CR, "\r"). % go back to start of line

%% Note: the "hide" seems to delay processing of user input until the cursor is "shown"
%%       (at least in the macOS Terminal app, with default xterm-256color,
%%        on macOS 14.7.5 Sonoma)
-define(HIDE_CURSOR, "\e[?25l"). % hide the visible (input) cursor
-define(SHOW_CURSOR, "\e[?25h"). % show the cursor

-define(CURSOR_UP(N), "\e[" ++ integer_to_list(N) ++ "A").

-define(CURSOR_FORWARD(N), "\e[" ++ integer_to_list(N) ++ "C").

%% -----------------------------------------------------------------------------







