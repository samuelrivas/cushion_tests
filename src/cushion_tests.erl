%%%-------------------------------------------------------------------
%%% Copyright 2010 Samuel Rivas <samuelrivas@gmail.com>
%%%
%%% This file is part of Cushion.
%%%
%%% Cushion is free software: you can redistribute it and/or modify it under
%%% the terms of the GNU General Public License as published by the Free
%%% Software Foundation, either version 3 of the License, or (at your option)
%%% any later version.
%%%
%%% Cushion is distributed in the hope that it will be useful, but WITHOUT ANY
%%% WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
%%% FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
%%% details.
%%%
%%% You should have received a copy of the GNU General Public License along with
%%% Cushion.  If not, see <http://www.gnu.org/licenses/>.
%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
%%% @author Samuel Rivas <samuelrivas@gmail.com>
%%% @copyright (C) 2010, Samuel Rivas
%%% @doc Functions to launch all cushion tests
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cushion_tests).

%% Testing API
-export([available_tests/0, run_all/2, run_tests/3, run_test/1,
         disable_tty_logger/0, enable_tty_logger/0, generate_qc_suite/1,
         generate_qc_suite/2]).

%% Useful functions for manual testing
-export([cover_compile_app/1, write_cover_results/1]).

available_tests() ->
    [json, http_api, cushion, cushion_util].

run_all(AppsToCover, CoverLogDir) ->
    run_tests(available_tests(), AppsToCover, CoverLogDir).

run_tests(Tests, AppsToCover, CoverLogDir) ->
    cover:reset(),
    lists:foreach(fun cover_compile_app/1, AppsToCover),
    Failed = lists:foldl(fun run_test/2, [], Tests),
    write_cover_results(CoverLogDir),
    report_failed(Failed).

run_test(Test) ->
    run_test(Test, []).

run_test(http_api, Failed) ->
    run_eunit(cushion_couch_api_test, Failed);
run_test(json, Failed) ->
    % Run also ktuo tests, just in case (and to get test code covered)
    run_quickcheck(
      cushion_json_encode_test, prop_roundtrip,
      run_quickcheck(
        cushion_json_decode_test, prop_parse, Failed));
run_test(cushion, Failed) ->
    run_quickcheck(cushion_fsm_test, prop_cushion, Failed);
run_test(cushion_util, Failed) ->
    run_quickcheck_module(cushion_util_tests, Failed).

write_cover_results(CoverLogDir) ->
    io:format(
      " * Analysing clause coverage:~n"
      "--------------------------------------------------~n"),
    lists:foreach(
      fun(Module) ->
              analyse_clause_coverage(Module)
      end,
      cover:modules()),

    io:format(
      "--------------------------------------------------~n"
      " * Writing line coverage analysis to ~s~n", [CoverLogDir]),

    lists:foreach(
      fun(Module) ->
              analyse_line_coverage(Module, CoverLogDir)
      end,
      cover:modules()),
    io:format(" * Analysis done~n").

generate_qc_suite(Prop) ->
    generate_qc_suite(Prop, []).

generate_qc_suite(Prop, ModuleBlackList) ->
    cover:stop(),
    cover_compile_app(cushion),
    cover_compile_files(
      source_files(code:lib_dir(mochiweb, src), [mochijson2, mochinum])),
    eqc_suite:feature_based(
      eqc_suite:line_coverage(cover:modules() -- ModuleBlackList, Prop)).

%%%-------------------------------------------------------------------
%%% Internals
%%%-------------------------------------------------------------------
cover_compile_app(App) ->
    io:format("Cover-compiling ~p:~n", [App]),
    cover_compile_files(source_files(App)).

cover_compile_files(Files) ->
    lists:foreach(
      fun(File) ->
              io:format(" * cover-compiling ~s~n", [File]),
              cushion_util:untuple(cover:compile(File))
      end,
      Files).

source_files(App) ->
    AppSrc = code:lib_dir(App, src),
    source_files(AppSrc, cushion_util:app_modules(App)).

source_files(AppSrc, Modules) ->
    [get_src(Module, AppSrc) || Module <- Modules].

%% XXX This could be done more elegantly using Module:module_info, but after
%% cover compiling the information in the source field is wrong
get_src(Module, Src) ->
    filename:join(Src, cushion_util:format("~p.erl", [Module])).

analyse_clause_coverage(Module) ->
    Analysis = cushion_util:untuple(cover:analyse(Module, coverage, clause)),
    {Covered, NotCovered} =
        lists:foldl(
          fun({_, {C, NC}}, {AccC, AccNC}) -> {AccC + C, AccNC + NC} end,
          {0, 0}, Analysis),
    Total = Covered + NotCovered,
    io:format(
      "~7.2f% ~p (~p/~p)~n", [Covered*100/Total, Module, Covered, Total]).

analyse_line_coverage(Module, CoverLogDir) ->
    cushion_util:untuple(
      cover:analyse_to_file(
        Module,
        filename:join(CoverLogDir, cushion_util:format("~p.html", [Module])),
        [html])).

run_quickcheck(Mod, Prop, Failed) ->
    FormerFlag = process_flag(trap_exit, true),
    try eqc:quickcheck(Mod:Prop()) of
        true ->
            Failed;
        false ->
            [Mod | Failed]
    catch
        X:Y ->
            io:format("~n~p:~p() failed -- ~w:~w~n~n", [Mod, Prop, X, Y]),
            [Mod | Failed]
    after
        process_flag(trap_exit, FormerFlag)
    end.

run_quickcheck_module(Mod, Failed) ->
    Fail = eqc:module(Mod),
    Fail ++ Failed.

run_eunit(Mod, Failed) ->
    case Mod:test() of
        ok ->
            Failed;
        error ->
            [Mod | Failed]
    end.

report_failed([]) ->
    ok;
report_failed(Failed) ->
    io:format(" * SOME TESTS FAILED: ~w~n", [Failed]).

%% Some tests start and stop applications a lot. The standard error logger event
%% handler logs them to TTY, wiping any useful information out. You can disable
%% that logger with this function and enable it later
disable_tty_logger() ->
    error_logger:delete_report_handler(error_logger_tty_h).

enable_tty_logger() ->
    error_logger:add_report_handler(error_logger_tty_h).
