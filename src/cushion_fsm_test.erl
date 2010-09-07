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
%%% @author Samuel Rivas <samuelrivas@gmail.com>
%%% @copyright (C) 2010, Samuel Rivas
%%% @doc This module tests the couch api fsm for a single database.
%%%
%%% In short, the transitions could be:
%%% <ul>
%%% <li>Configure couchdb access</li>
%%% <li>Create a database</li>
%%% <li>Create a document</li>
%%% <li>Update a document</li>
%%% <li>Retrieve a document</li>
%%% <li>Delete a document</li>
%%% <li>Delete a database</li>
%%%
%%% CouchDB access object will be static and thus need not be deleted. The GC
%%% will take care of it :)
%%%
%%% @end
%%% Created :  7 Sep 2010 by Samuel Rivas <samuelrivas@gmail.com>

-module(cushion_fsm_test).

%% eqc_fsm callbacks
-export([initial_state/0, initial_state_data/0, next_state_data/5, weight/3,
         precondition/4, postcondition/5]).

%% States
-export([init_state/1, access_created/1, got_existing_dbs/1, db_created/1]).

%% Wrappers
-export([new_access/2, get_dbs/1, create_db/2, delete_db/2]).

%% Public API
-export([prop_cushion/0]).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_fsm.hrl").

-record(state,{
          access,       % cushion_access()
          db            % string()
         }).

%%%-------------------------------------------------------------------
%%% generators
%%%-------------------------------------------------------------------
db_name() ->
    ?LET(
       H, eqc_gen:choose($a, $z),
       ?LET(T, eqc_gen:list(db_name_char()), [H | T])).

%% XXX according to the documentation, couchdb should accept + as db name
%% character, but right now that's failing unless + is encoded as %2B. Also /
%% must be encoded to %2F, that's not yet implemented.
db_name_char() ->
    eqc_gen:elements(
      lists:flatten([lists:seq($a, $z), lists:seq($0, $9), "_$()-"])).

%%%-------------------------------------------------------------------
%%% eqc_fsm callbacks
%%%-------------------------------------------------------------------

%% Definition of the states. Each state is represented by a function,
%% listing the transitions from that state, together with generators
%% for the calls to make each transition.
init_state(_S) ->
    [
     {access_created,
      {call,?MODULE,new_access,[default_host(), default_port()]}}
     ].

access_created(S) ->
    [
     {got_existing_dbs, {call, ?MODULE, get_dbs, [S#state.access]}}
    ].

got_existing_dbs(S) ->
    [
     {db_created, {call, ?MODULE, create_db, [S#state.access, db_name()]}}
    ].

db_created(S) ->
    Access = S#state.access,
    Db = S#state.db,
    [
     {got_existing_dbs, {call, ?MODULE, delete_db, [Access, Db]}}
    ].

%% Identify the initial state
initial_state() ->
    init_state.

%% Initialize the state data
initial_state_data() ->
    #state{}.

%% Next state transformation for state data.
%% S is the current state, From and To are state names
next_state_data(_,_,S,V,{call,_,new_access,_}) ->
    S#state{access = V};
next_state_data(_,_,S,_V,{call,_,create_db,[_, Db]}) ->
    S#state{db = Db};
next_state_data(_From,_To,S,_V,{call,_,_,_}) ->
    S.

%% Precondition (for state data).
%% Precondition is checked before command is added to the command sequence
precondition(_From,_To,_S,{call,_,_,_}) ->
    true.

%% Postcondition, checked after command has been evaluated
%% OBS: S is the state before next_state_data(From,To,S,_,<command>)
postcondition(init_state, access_created,_S,{call,_,new_access,_},Res) ->
    case Res of
        {error, _} ->
            false;
        _ ->
            true
    end;
postcondition(access_created, got_existing_dbs, _S, {call,_,get_dbs, _}, Res) ->
    is_list(Res);
postcondition(got_existing_dbs, db_created,_S,{call,_,create_db,_},Res) ->
    Res == ok;
postcondition(db_created, got_existing_dbs,_S,{call,_,delete_db,_},Res) ->
    Res == ok;
postcondition(db_created, access_created, _S,{call,_,delete_db,_},Res) ->
    Res == ok.

%% Weight for transition (this callback is optional).
%% Specify how often each transition should be chosen
weight(_From,_To,{call,_,_,_}) ->
    1.

%%%-------------------------------------------------------------------
%%% Wrappers
%%%-------------------------------------------------------------------
new_access(Host, Port) ->
    cushion:new_access(Host, Port).

get_dbs(Access) ->
    cushion:get_dbs(Access).

create_db(Access, Name) ->
    cushion:create_db(Access, Name).

delete_db(Access, Name) ->
    cushion:delete_db(Access, Name).

%%%-------------------------------------------------------------------
%%% Properties
%%%-------------------------------------------------------------------
prop_cushion() ->
    ?FORALL(Cmds,commands(?MODULE),
            begin
                Dbs = cushion:get_dbs(default_access()),
                {H,S,Res} = run_commands(?MODULE,Cmds),
                restore_dbs(Dbs),
                ?WHENFAIL(
                   io:format("History: ~p\nState: ~p\nRes: ~p\n",[H,S,Res]),
                   Res == ok)
            end).

%%%-------------------------------------------------------------------
%%% Internals
%%%-------------------------------------------------------------------
default_host() ->
    "localhost".

default_port() ->
    5984.

default_access() ->
    cushion:new_access(default_host(), default_port()).

%% Delete all dbs but those present in BlackList (we need cushion:get_dbs/1 and
%% cushion:delete_db/2 to be working here)
restore_dbs(BlackList) ->
    Access = default_access(),
    Dbs = cushion:get_dbs(Access),
    lists:foreach(
      fun(Db) ->
              case lists:member(Db, BlackList) of
                  true ->
                      ok;
                  false ->
                      try
                          cushion:delete_db(Access, Db)
                      catch
                          {couchdb_error, {404, _}} ->
                              ok
                      end
              end
      end,
      Dbs).
