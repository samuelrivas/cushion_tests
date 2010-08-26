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
%%% @author Samuel <samuelrivas@gmail.com>
%%% @copyright (C) 2010, Samuel
%%% @doc Tests for cushion's cushion_couch_api module
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cushion_couch_api_test).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_DOC(Opt), test_doc(?FILE, ?LINE, Opt)).

%% Creating an auto-id document should never fail.
%% XXX we cannot do finer tests until we have the response parser, so far we
%% just execute the function as a smoke test
create_document_test_() ->
    {Host, Port, Db} = conf(),
    standard_fixture(
      [lists:duplicate(
	 3,
	 ?_test(
	    cushion_couch_api:create_document(
	      Host, Port, Db, ?TEST_DOC(empty))))]).

%% Test the most common document API
document_test_() ->
    {Host, Port, Db} = conf(),
    Id = "test1",
    standard_fixture(
      [
       % Create a new document and get it
       ?_test(cushion_couch_api:update_document(
		Host, Port, Db, Id, ?TEST_DOC(empty))),
       ?_test(cushion_couch_api:get_document(Host, Port, Db, Id)),

       % Now, test get and update negative cases
       ?_assertThrow(
	  {couchdb_error, {409, _}},
	  cushion_couch_api:update_document(
	    Host, Port, Db, Id, ?TEST_DOC(empty))),
       ?_assertThrow(
	  {couchdb_error, {404, _}},
	  cushion_couch_api:get_document(Host, Port, Db, "some_id")),

       % XXX We cannot test positive delete cases yet
       ?_assertThrow(
	  {couchdb_error, {400, _}},
	  cushion_couch_api:delete_document(Host, Port, Db, Id, "")),
       ?_assertThrow(
	  {couchdb_error, {404, _}},
	  cushion_couch_api:delete_document(
	    Host, Port, Db, "some_id", fake_rev())),
       ?_assertThrow(
	  {couchdb_error, {409, _}},
	  cushion_couch_api:delete_document(Host, Port, Db, Id, fake_rev()))]).


%% Test the DB API
%% Creation and deletion are already tested in setups and cleanups, here we just
%% test the negative cases.
create_database_test_() ->
    {Host, Port, Db} = conf(),
    standard_fixture(
      [?_assertThrow(
	  {couchdb_error, {412, _}},
	  cushion_couch_api:create_db(Host, Port, Db)),
       ?_assertThrow(
	  {couchdb_error, {404, _}},
	  cushion_couch_api:delete_db(Host, Port, "fake_db_shouldnt_exist"))]).

%%%-------------------------------------------------------------------
%%% Internals
%%%-------------------------------------------------------------------

standard_fixture(Test) ->
    {setup, fun standard_setup/0, fun standard_cleanup/1, Test}.

standard_setup() ->
    Apps = start(cushion),
    create_db(),
    Apps.

standard_cleanup(Apps) ->
    delete_db(),
    lists:foreach(
      fun(App) -> application:stop(App) end,
      Apps).

start(App) ->
    lists:flatten(start(App, 30)).

start(_, 0) ->
    throw(too_much_recursion);
start(App, N) ->
    case application:start(App) of
	{error, {already_started, App}} ->
	    [];
        {error, {not_started, OtherApp}} ->
            [start(OtherApp, N - 1),
             start(App, N - 1)];
        ok ->
            [App]
    end.

create_db() ->
    % Tentatively delete the tests database just in case
    try delete_db()
    catch
	{couchdb_error, {404, _}} ->
	    ok
    end,
    cushion_couch_api:create_db(get_host(), get_port(), get_db()).

delete_db() ->
    cushion_couch_api:delete_db(get_host(), get_port(), get_db()).

test_doc(File, Line, empty) ->
    io_lib:format("{\"test_file\" : ~p, \"line\" : \"~p\"}", [File, Line]).

%% Can actually be valid by chance, but that would be extreme bad luck.
fake_rev() ->
    "1-fe46b1a37e32aa544edb754885c0864b".

%% XXX Configuration is hardcoded for now. The database cushion_tests must be
%% created beforehand
conf() ->
    {get_host(), get_port(), get_db()}.

get_host() ->
    "localhost".

get_port() ->
    5984.

get_db() ->
    "cushion_tests".
