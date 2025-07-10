-module(na_benchmark_helper).

-include("na_benchmark_ansi_escape_codes.hrl").

-export([
         run_actions/1,
         print_info/2,
         page_width/0,
         get_avrg/2,
         shuffle/1
        ]).

-export_type([
              test_name/0,
              action/0,
              action_results/0
             ]).


-type test_name() :: term().
-type test_to_run() :: fun().
-type action() :: {test_name(), test_to_run()}.
-type action_results() :: #{test_name() => [integer()]}.

%% --------------------------------------------------------------------------------------------------
-spec run_actions(Actions :: [action()]) -> action_results().

%% Run each action() and store the time it took to run it,
%% in a map using the test_name() of the action.
%% The same action() may occur multiple times, so the map may contain multiple time results.
run_actions(Actions) ->
    try
        draw_progressbar_initial(),
        run_actions(Actions, #{}, length(Actions), 1)
    catch
        %% on error un-hide the cursor as final_text() never gets run
        Class:Reason:Stacktrace ->
            io:format(?SHOW_CURSOR, []),
            erlang:raise(Class, Reason, Stacktrace)
    end.

run_actions([], Res, _, _) ->
    draw_progressbar_final(),
    Res;
run_actions([{TestName, TestFun} = _Action | R], Res, LoopCount, N) ->
    draw_progressbar(LoopCount, N),
    {Time, _} = timer:tc(TestFun),
    NewRes = case maps:get(TestName, Res, undefined) of
                 undefined ->
                     Res#{TestName => [Time]};
                 Vals ->
                     Res#{TestName => [Time | Vals]}

             end,
    run_actions(R, NewRes, LoopCount, N + 1).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% The progress bar consists of 4 lines:
%% 1. top padding
%% 2. progress bar
%% 3. line showing completion percentage
%% 4. bottom padding

%% Hide cursor while drawing the progress  bar, final_text() un-hides it.
%% If this not done then the progress bar will have a visible (input) cursor on the left side
%% of the progress bar
draw_progressbar_initial() ->
    io:format(?HIDE_CURSOR ++ "~n", []). % pad with 1 line above progressbar

%% cursor will start on progress bar line
%% move to next line, the one below bottom padding line
draw_progressbar_final() ->
    io:format("~n~n~n" ++ ?SHOW_CURSOR, []).

%% cursor will start on progress bar line
draw_progressbar(AllSteps, CurrentStep) when AllSteps >= 1 andalso CurrentStep >= 1 ->
    PageWidth = page_width(),
    Done = round(CurrentStep / AllSteps * PageWidth),
    ToDo = PageWidth - Done,

    case Done of
        0 -> % no progress, draw arrow to indicate that work started
            io:format(?PROGRESSBAR_DONE ++ "»" ++ ?RESET, []);
        1 -> % got 1 progress "bar" - only got space to draw arrow
            io:format(?PROGRESSBAR_DONE ++ "»" ++ ?RESET, []);
        _ -> % draw progress body (Done - 1 "bars") and arrow
            io:format(?PROGRESSBAR_DONE ++ "~s" ++ ?RESET, [lists:duplicate(Done - 1, $\s)]),
            io:format(?PROGRESSBAR_DONE ++ "»" ++ ?RESET, [])
    end,
    case ToDo of
        0 -> ok;
        _ -> io:format(?PROGRESSBAR_TODO ++ "~s" ++ ?RESET, [lists:duplicate(ToDo, $\s)])
    end,

    %% display progress in %, centred, below progress bar
    %% a underline is shown below the completed part of the progress bar (but not under the arrow)

    io:format("~n", []),
    io:format("~ts", [lists:duplicate(Done, 16#203E)]), % 203E = Overline
    %% move back and paint over part of the current line
    io:format(?CR, []),
    Progress = round(CurrentStep / AllSteps * 100),
    io:format(?CURSOR_FORWARD((PageWidth div 2) - 4) ++ " ~p % ", [Progress]),

    %% draw bottom padding, then move back up to the beginning of the progress bar line
    io:format("~n" ++ ?CURSOR_UP(2) ++ ?CR, []).

%% --------------------------------------------------------------------------------------------------
print_info(Dups, Iterations) ->
    Text = [
            io_lib:format("Each test action has ~p separate runs.", [Dups]),
            io_lib:format("Each test action runs its test code ~p times.", [Iterations]),
            "Each individual test action run - is run in random order,",
            "to minimise the effects of other actions (like Erlang GC),",
            "on the time measurements."
           ],
    Lines = lists:map(fun(Str) ->
                      ?BG_NOTE ++ string:pad(Str, page_width()) ++ ?RESET ++ "\n"
              end,
              Text),

    io:format("~s", [Lines]).

%% --------------------------------------------------------------------------------------------------
page_width() ->
    80.

%% --------------------------------------------------------------------------------------------------
-spec get_avrg(TestName :: test_name(), Res :: action_results()) -> float() | integer().

%% Get the average for a specific test_name() in the result of a run_actions() call.
get_avrg(TestName, Res) ->
    #{TestName := TimeVals} = Res,
    lists:sum(TimeVals) / length(TimeVals).


%% --------------------------------------------------------------------------------------------------
%% Randomise the order of a list of elements.
%% This is essentially a reverse quick sort.
shuffle([]) ->
    [];
shuffle([E]) ->
    [E];
shuffle(L) ->
    {Left, Right} = shuffle(L, [], []),
    shuffle(Left) ++ shuffle(Right).

shuffle([], Left, Right) ->
    {Left, Right};
shuffle([E | R], Left, Right) ->
    case rand:uniform(2) of
        1 ->
            shuffle(R, [E | Left], Right);
        2 ->
            shuffle(R, Left, [E | Right])
    end.

