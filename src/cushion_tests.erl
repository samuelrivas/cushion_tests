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
-export([available_tests/0, run_all/2, run_tests/3, run_test/1]).

available_tests() ->
    [json, http_api, cushion].

run_all(AppsToCover, CoverLogDir) ->
    run_tests(available_tests(), AppsToCover, CoverLogDir).

run_tests(Tests, AppsToCover, CoverLogDir) ->
    cover:reset(),
    lists:foreach(fun cover_compile_app/1, AppsToCover),
    lists:foreach(fun run_test/1, Tests),
    write_results(CoverLogDir).

run_test(http_api) ->
    cushion_couch_api_test:test();
run_test(json) ->
    % Run also ktuo tests, just in case (and to get test code covered)
    ktuo_parse_utils:test(),
    ktj_parse:test(),
    ktj_encode:test(),
    eqc:quickcheck(cushion_json_test:prop_roundtrip());
run_test(cushion) ->
    eqc:quickcheck(cushion_fsm_test:prop_cushion()).

%%%-------------------------------------------------------------------
%%% Internals
%%%-------------------------------------------------------------------
cover_compile_app(App) ->
    io:format("Cover-compiling ~p:~n", [App]),
    lists:foreach(
      fun(File) ->
              io:format(" * cover-compiling ~s~n", [File]),
              cushion_util:untuple(cover:compile(File))
      end,
      source_files(App)).

source_files(App) ->
    Src = code:lib_dir(App, src),
    [get_src(Module, Src) || Module <- cushion_util:app_modules(App)].

%% XXX This could be done more elegantly using Module:module_info, but after
%% cover compiling the information in the source field is wrong
get_src(Module, Src) ->
    filename:join(Src, cushion_util:format("~p.erl", [Module])).

write_results(CoverLogDir) ->
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
