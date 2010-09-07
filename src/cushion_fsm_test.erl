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
-export([init_state/1, access_created/1]).

%% Wrappers
-export([new_access/2, noop/0]).

%% Public API
-export([prop_cushion/0]).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_fsm.hrl").

-record(state,{}).

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

access_created(_S) ->
    [
     {access_created, {call, ?MODULE, noop, []}}
     ].

%% Identify the initial state
initial_state() ->
    init_state.

%% Initialize the state data
initial_state_data() ->
    [].

%% Next state transformation for state data.
%% S is the current state, From and To are state names
next_state_data(_From,_To,S,_V,{call,_,_,_}) ->
    S.

%% Precondition (for state data).
%% Precondition is checked before command is added to the command sequence
precondition(_From,_To,_S,{call,_,_,_}) ->
    true.

%% Postcondition, checked after command has been evaluated
%% OBS: S is the state before next_state_data(From,To,S,_,<command>)
postcondition(_From,_To,_S,{call,_,_,_},_Res) ->
    true.

%% Weight for transition (this callback is optional).
%% Specify how often each transition should be chosen
weight(_From,_To,{call,_,_,_}) ->
    1.

%%%-------------------------------------------------------------------
%%% Wrappers
%%%-------------------------------------------------------------------
new_access(_Host, _Port) ->
    ok.

noop() ->
    ok.

%%%-------------------------------------------------------------------
%%% Properties
%%%-------------------------------------------------------------------
prop_cushion() ->
    ?FORALL(Cmds,commands(?MODULE),
            begin
                {H,S,Res} = run_commands(?MODULE,Cmds),
                ?WHENFAIL(
                   io:format("History: ~p\nState: ~p\nRes: ~p\n",[H,S,Res]),
                   Res == ok)
            end).

%%%-------------------------------------------------------------------
%%% Internals
%%%-------------------------------------------------------------------
default_port() ->
    5984.

default_host() ->
    "localhost".
