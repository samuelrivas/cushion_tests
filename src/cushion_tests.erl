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
-export([available_tests/0, run_test/3, run_test/1]).

available_tests() ->
    [json, http_api, cushion].


run_test(Test, AppsToCover, CoverLogDir) ->
    cover:reset(),
    lists:foreach(fun cover_compile_app/1, AppsToCover),
    run_test(Test),
    write_results(CoverLogDir).

run_test(http_api) ->
    cushion_couch_api_test:test().

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
    lists:foreach(
      fun(Module) -> analyse(Module, CoverLogDir) end, cover:modules()).

analyse(Module, CoverLogDir) ->
    cushion_util:untuple(
      cover:analyse_to_file(
        Module,
        filename:join(CoverLogDir, cushion_util:format("~p.html", [Module])),
        [html])).
