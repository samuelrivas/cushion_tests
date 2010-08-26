%%%-------------------------------------------------------------------
%%% Copyright 2006, 2007, 2010 Samuel Rivas <samuelrivas@gmail.com>
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
%%% @author Samuel <samuelrivas@gmail.com>
%%% @copyright (C) 2010, Samuel
%%% @doc Tests for cushion's cushion_couch_api module
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cushion_couch_api_test).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_DOC(Opt), test_doc(?FILE, ?LINE, Opt)).

create_document_test_() ->
    Host = get_host(),
    Port = get_port(),
    Db = get_db(),

    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     [?_test(
	 cushion_couch_api:create_document(Host, Port, Db, ?TEST_DOC(empty)))]}.

%%%-------------------------------------------------------------------
%%% Internals
%%%-------------------------------------------------------------------

test_doc(File, Line, empty) ->
    io_lib:format("{\"test_file\" : ~p, \"line\" : \"~p\"}", [File, Line]).

%% XXX Configuration is hardcoded for now. The database cushion_tests must be
%% created beforehand
get_host() ->
    "localhost".

get_port() ->
    5984.

get_db() ->
    "cushion_tests".
