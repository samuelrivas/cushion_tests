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
-export([ready/1]).

%% Wrappers
-export([initialise/1, new_access/2, get_dbs/1, create_db/2, delete_db/2]).

%% Public API
-export([prop_cushion/0]).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_fsm.hrl").

-record(state,{
          access = undefined_acces, % cushion_access(), the access to couchdb
          db_names = [],            % [string()], a random set of valid db names
          dbs = []                  % [string()], dbs created in the test
         }).

%%%-------------------------------------------------------------------
%%% generators
%%%-------------------------------------------------------------------
db_names(Blacklist) ->
    eqc_gen:non_empty(eqc_gen:list(blacklisted_db_name(Blacklist))).

blacklisted_db_name(Blacklist) ->
    ?SUCHTHAT(N, db_name(), not lists:member(N, Blacklist)).

%% Generate names according to CouchDB name rules
db_name() ->
    ?LET(
       H, eqc_gen:choose($a, $z),
       ?LET(T, eqc_gen:list(db_name_char()), [H | T])).

%% To favour repetition and, at the same time, look for possible broken
%% character combinations, we generate a set of random names and use them
%% through all fsm testing
db_name(S) ->
    eqc_gen:elements(S#state.db_names).

db_name_char() ->
    eqc_gen:elements(valid_db_name_chars()).

%% XXX according to the documentation, couchdb should accept + as db name
%% character, but right now that's failing unless + is encoded as %2B. Also /
%% must be encoded to %2F, that's not yet implemented.
valid_db_name_chars() ->
      lists:flatten([lists:seq($a, $z), lists:seq($0, $9), "_$()-"]).

%%%-------------------------------------------------------------------
%%% eqc_fsm callbacks
%%%-------------------------------------------------------------------

%% Definition of the states. Each state is represented by a function,
%% listing the transitions from that state, together with generators
%% for the calls to make each transition.
ready(S) ->
    Access = S#state.access,
    [
     {ready, {call, ?MODULE, create_db, [Access, db_name(S)]}},
     {ready, {call, ?MODULE, delete_db, [Access, db_name(S)]}}
    ].

%% Identify the initial state
initial_state() ->
    ready.

%% Initialize the state data
initial_state_data() ->
    % We must start in a known state using commands/2, so this callback is never
    % used
    erlang:error(should_not_be_reached).

%% Next state transformation for state data.
%% S is the current state, From and To are state names
next_state_data(_,_,S,V,{call,_,create_db,[_, Db]}) ->
    case V of
        ok ->
            S#state{dbs = [Db | S#state.dbs]};
        _ ->
            S
    end;
next_state_data(_,_,S,V,{call,_,delete_db,[_, Db]}) ->
    case V of
        ok ->
            S#state{dbs =  lists:delete(Db, S#state.dbs)};
        _ ->
            S
    end;
next_state_data(_From,_To,S,_V,{call,_,_,_}) ->
    S.

%% Precondition (for state data).
%% Precondition is checked before command is added to the command sequence
precondition(_From,_To,_S,{call,_,_,_}) ->
    true.

%% Postcondition, checked after command has been evaluated
%% OBS: S is the state before next_state_data(From,To,S,_,<command>)
postcondition(ready, ready,S, {call,_,create_db,[_Access,Db]},Res) ->

    Existed = lists:member(Db, S#state.dbs),
    case Res of
        ok ->
            not Existed;
        {error, 412} ->
            Existed
    end;
postcondition(ready, ready,S,{call,_,delete_db,[_Access,Db]},Res) ->
    Existed = lists:member(Db, S#state.dbs),
    case Res of
        ok ->
            Existed;
        {error, 404} ->
            not Existed
    end.

%% Weight for transition (this callback is optional).
%% Specify how often each transition should be chosen
weight(_From,_To,{call,_,_,_}) ->
    1.

%%%-------------------------------------------------------------------
%%% Wrappers
%%%-------------------------------------------------------------------

%% XXX Does nothing, but we need this to insert the allowed db name set in the
%% state, see next_state_data.
initialise(_DbNames) ->
    ok.

new_access(Host, Port) ->
    cushion:new_access(Host, Port).

get_dbs(Access) ->
    cushion:get_dbs(Access).

create_db(Access, Name) ->
    catch_error(fun() -> cushion:create_db(Access, Name) end).

delete_db(Access, Name) ->
    catch_error(fun() -> cushion:delete_db(Access, Name) end).

catch_error(F) ->
    try F()
    catch
        {couchdb_error, {Code, _}} ->
            {error, Code}
    end.

%%%-------------------------------------------------------------------
%%% Properties
%%%-------------------------------------------------------------------
prop_cushion() ->
    Access = default_access(),
    ExistingDbs = cushion:get_dbs(Access),
    ?FORALL(
       DbNames, db_names(ExistingDbs),
       ?FORALL(
          Cmds,
          commands(
            ?MODULE, {ready, #state{db_names = DbNames, access = Access}}),

          begin
              {H,S,Res} = run_commands(?MODULE,Cmds),
              restore_dbs(ExistingDbs),
              aggregate(
                eqc_statem:zip(
                  eqc_fsm:state_names(H),eqc_statem:command_names(Cmds)),
                ?WHENFAIL(
                   io:format("History: ~p\nState: ~p\nRes: ~p\n",[H,S,Res]),
                   Res == ok))
          end)).

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
