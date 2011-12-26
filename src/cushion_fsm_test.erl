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
%%% @doc This module tests couch api generating random fsm's.
%%%
%%% These tests take care of existing databases in the configured CouchDB
%%% server. They wouldn't be used for testing. The tests will create and destroy
%%% new databases, and create, update, and delete documents on them. After the
%%% test finish, any remaining testing database will be deleted.
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
-export([initialise/1, new_access/2, get_dbs/1, create_db/2, fail_create_db/2,
         fail_delete_db/2, delete_db/2, create_doc/3, delete_doc/2,
         fail_delete_doc/2, get_doc/2]).

%% Public API
-export([prop_cushion/0]).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_fsm.hrl").

-record(state,{
          access = undefined_acces, % cushion_access(), the access to couchdb
          db_names = [],            % [string()], a random set of valid db names
          dbs_and_docs = [],        % [{string(), [doc_ref()]],
                                    % dbs created in the test. A doc_ref is
                                    % {id, vsn}
          docs = []                 % {db(), doc_ref(), doc()}
         }).

%%%-------------------------------------------------------------------
%%% generators
%%%-------------------------------------------------------------------
db_names(Blacklist) ->
    eqc_gen:non_empty(eqc_gen:list(blacklisted_db_name(Blacklist))).

blacklisted_db_name(Blacklist) ->
    ?SUCHTHAT(N, db_name(), not lists:member(N, Blacklist)).

%% Generate names according to CouchDB name rules
%%
%% To favour repetition and, at the same time, look for possible broken
%% character combinations, we generate a set of random names and use them
%% through all fsm testing
db_name() ->
    ?LET(
       H, eqc_gen:choose($a, $z),
       ?LET(T, eqc_gen:list(db_name_char()), [H | T])).

db_name(#state{db_names = Names}) ->
    eqc_gen:elements(Names).

db_name_char() ->
    eqc_gen:elements(valid_db_name_chars()).

%% Next two generators allow us to control whether db operations should pass or
%% fail
new_db_name(#state{dbs_and_docs = Dbs, db_names = DbNames}) ->
    eqc_gen:elements(DbNames -- remove_docs(Dbs)).

existing_db_name(#state{dbs_and_docs = Dbs}) ->
    eqc_gen:elements(remove_docs(Dbs)).

existing_db_and_doc_ref(S = #state{dbs_and_docs = Dbs}) ->
    ?LET(
       Db, existing_db_name(S),
       begin
           {value, {Db, Docs}} = lists:keysearch(Db, 1, Dbs),
           {Db, eqc_gen:elements(Docs)}
       end).

bad_db_and_doc_ref(S) ->
    ?SUCHTHAT(
       {Db, Ref}, {db_name(S), eqc_gen:elements(get_all_refs(S))},
       not lists:member(Ref, get_db_docs(S, Db))).

%% XXX according to the documentation, couchdb should accept + as db name
%% character, but right now that's failing unless + is encoded as %2B. Also /
%% must be encoded to %2F, that's not yet implemented.
valid_db_name_chars() ->
      lists:flatten([lists:seq($a, $z), lists:seq($0, $9), "_$()-"]).

%% Avoid shrink to non-object JSONs
doc() ->
    ?SUCHTHAT(
       Obj, cushion_json_encode_test:in_object(),
       case Obj of
           {struct, _} -> true;
           _ -> false
       end).

get_all_refs(#state{dbs_and_docs = DbsAndDocs}) ->
    {_Bds, Refs} = lists:unzip(DbsAndDocs),
    lists:flatten(Refs).

get_db_docs(#state{dbs_and_docs = DbsAndDocs}, Db) ->
    case lists:keysearch(Db, 1, DbsAndDocs) of
        {value, {Db, Docs}} -> Docs;
        false -> []
    end.

%%%-------------------------------------------------------------------
%%% eqc_fsm callbacks
%%%-------------------------------------------------------------------

%% Definition of the states. Each state is represented by a function,
%% listing the transitions from that state, together with generators
%% for the calls to make each transition.
ready(S) ->
    Access = S#state.access,
    [
     {ready, {call, ?MODULE, create_db, [Access, new_db_name(S)]}},
     {ready, {call, ?MODULE, fail_create_db, [Access, existing_db_name(S)]}},
     {ready, {call, ?MODULE, delete_db, [Access, existing_db_name(S)]}},
     {ready, {call, ?MODULE, fail_delete_db, [Access, new_db_name(S)]}},
     {ready, {call, ?MODULE, create_doc, [Access, existing_db_name(S), doc()]}},
     {ready, {call, ?MODULE, delete_doc, [Access, existing_db_and_doc_ref(S)]}},
     {ready, {call, ?MODULE, fail_delete_doc, [Access, bad_db_and_doc_ref(S)]}},
     {ready, {call, ?MODULE, get_doc, [Access, existing_db_and_doc_ref(S)]}}
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
next_state_data(_,_,S,_V,{call,_,create_db,[_, Db]}) ->
    S#state{dbs_and_docs = [{Db, []} | S#state.dbs_and_docs]};

next_state_data(_,_,S,_V,{call,_,delete_db,[_, Db]}) ->
    S#state{dbs_and_docs = lists:keydelete(Db, 1, S#state.dbs_and_docs)};

next_state_data(_,_,S,V,{call,_,create_doc,[_, Db, Doc]}) ->
    {value, {Db, Docs}} = lists:keysearch(Db, 1, S#state.dbs_and_docs),
    S#state{
      dbs_and_docs =
        lists:keyreplace(Db, 1, S#state.dbs_and_docs, {Db, [V | Docs]}),
      docs = [{Db, V, Doc} | S#state.docs]};

next_state_data(_,_,S,_V,{call,_,delete_doc,[_, {Db, Ref}]}) ->
    {value, {Db, Docs}} = lists:keysearch(Db, 1, S#state.dbs_and_docs),
    S#state{
      dbs_and_docs =
        lists:keyreplace(Db, 1, S#state.dbs_and_docs, {Db, Docs -- [Ref]})};

next_state_data(_From,_To,S,_V,{call,_,_,_}) ->
    S.

%% Precondition (for state data).
%% Precondition is checked before command is added to the command sequence
precondition(_From,_To,S,{call,_,create_db,[_, Db]}) ->
    not lists:member(Db, remove_docs(S#state.dbs_and_docs));
precondition(_From,_To,S,{call,_,fail_create_db,[_, Db]}) ->
    lists:member(Db, remove_docs(S#state.dbs_and_docs));

precondition(_From,_To,S,{call,_,delete_db,[_, Db]}) ->
    lists:member(Db, remove_docs(S#state.dbs_and_docs));
precondition(_From,_To,S,{call,_,fail_delete_db,[_, Db]}) ->
    not lists:member(Db, remove_docs(S#state.dbs_and_docs));

precondition(_From,_To,S,{call,_,create_doc,[_, Db, _Doc]}) ->
    lists:member(Db, remove_docs(S#state.dbs_and_docs));

precondition(_From,_To,S,{call,_,delete_doc,[_, {Db, Ref}]}) ->
    lists:member(Ref, get_db_docs(S, Db));
precondition(_From,_To,S,{call,_,fail_delete_doc,[_, {Db, Ref}]}) ->
    not lists:member(Ref, get_db_docs(S, Db));
precondition(_From,_To,_S,{call,_,_,_}) ->
    true.

%% Postcondition, checked after command has been evaluated
%% OBS: S is the state before next_state_data(From,To,S,_,<command>)
postcondition(ready, ready,_S, {call,_,create_db,[_Access,_Db]},Res) ->
    Res == ok;
postcondition(ready, ready,_S, {call,_,fail_create_db,[_Access,_Db]},Res) ->
    Res == {error, 412};
postcondition(ready, ready,_S,{call,_,delete_db,[_Access,_Db]},Res) ->
    Res == ok;
postcondition(ready, ready,_S,{call,_,fail_delete_db,[_Access,_Db]},Res) ->
    Res == {error, 404};
postcondition(ready, ready,_S,{call,_,create_doc,[_Access,_Db, _Doc]}, Res) ->
    case Res of
        {error, _} ->
            false;
        {_Id, _Vsn} ->
            true;
        _ ->
            false
    end;
postcondition(ready,ready,_S,{call,_,delete_doc,[_Access,{_Db,_Ref}]}, Res) ->
    Res == ok;
postcondition(
  ready,ready,_S,{call,_,fail_delete_doc,[_Access,{_Db,_Ref}]},Res) ->
    Res == {error, 404};
postcondition(ready,ready,S,{call,_,get_doc,[_Access,{Db,Ref}]},Res) ->
    obj_equals(find_doc(S, Db, Ref), Res).

%% Weight for transition (this callback is optional).
%% Specify how often each transition should be chosen
weight(_From,_To,{call,_,create_db,_}) ->
    10;
weight(_From,_To,{call,_,fail_create_db,_}) ->
    5;
weight(_From,_To,{call,_,delete_db,_}) ->
    10;
weight(_From,_To,{call,_,fail_delete_db,_}) ->
    2;
weight(_From,_To,{call,_,create_doc,_}) ->
    20;
weight(_From,_To,{call,_,delete_doc,_}) ->
    30;
weight(_From,_To,{call,_,fail_delete_doc,_}) ->
    20;
weight(_From,_To,{call,_,get_doc,_}) ->
    30;
weight(_From,_To,{call,_,_,_}) ->
    10.

%%%-------------------------------------------------------------------
%%% Wrappers
%%%-------------------------------------------------------------------

%%% fail_xxx wrappers do the same as xxx but the fsm expects them to fail. It's
%%% done this way since we cannot depend on the result in next_state_data or in
%%% state functions, so we just steer testing process using transition names

%% XXX Does nothing, but we need this to insert the allowed db name set in the
%% state, see next_state_data.
initialise(_DbNames) ->
    ok.

new_access(Host, Port) ->
    cushion:new_access(Host, Port).

get_dbs(Access) ->
    cushion:get_dbs(Access).

fail_create_db(Access, Name) ->
    create_db(Access, Name).

create_db(Access, Name) ->
    catch_error(fun() -> cushion:create_db(Access, Name) end).

fail_delete_db(Access, Name) ->
    delete_db(Access, Name).

delete_db(Access, Name) ->
    catch_error(fun() -> cushion:delete_db(Access, Name) end).

create_doc(Access, Db, Doc) ->
    catch_error(fun() -> cushion:create_doc(Access, Db, Doc) end).

fail_delete_doc(Access, {Db, DocRef}) ->
    delete_doc(Access, {Db, DocRef}).

delete_doc(Access, {Db, DocRef}) ->
    catch_error(fun() -> cushion:delete_doc(Access, Db, DocRef) end).

get_doc(Access, {Db, {Id, _Vsn}}) ->
    catch_error(fun() -> cushion:get_doc(Access, Db, Id) end).

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
    ExistingDbs = get_existing_dbs(Access),
    ?FORALL(
       DbNames, db_names(ExistingDbs),
       ?FORALL(
          Cmds,
          commands(
            ?MODULE, {ready, #state{db_names = DbNames, access = Access}}),

          begin
              Apps = cushion_util:start_app(cushion),
              try
                  {H,S,Res} = run_commands(?MODULE,Cmds),
                  restore_dbs(ExistingDbs),
                  aggregate(
                    eqc_statem:zip(
                      eqc_fsm:state_names(H),eqc_statem:command_names(Cmds)),
                    ?WHENFAIL(
                       io:format("History: ~p\nState: ~p\nRes: ~p\n",[H,S,Res]),
                       Res == ok))
              after
                  cushion_util:stop_apps(Apps)
              end
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

remove_docs(DbsAndDocs) ->
    [Db || {Db, _Docs} <- DbsAndDocs].

find_doc(#state{docs = Docs}, Db, {Id, _Vsn}) ->
    find_doc_rec(Docs, Db, Id).

find_doc_rec([], Db, Id) ->
    erlang:error({document_not_found, Db, Id});
find_doc_rec([{Db, {Id, _}, Doc} | _], Db, Id) ->
    Doc;
find_doc_rec([{_, _, _} | T], Db, Id) ->
    find_doc_rec(T, Db, Id).

%% Compare that all fields in A are in B. This is not reflexive, since B may
%% have additional special fields, such as _rev or _id
obj_equals({struct, A}, {struct, B}) ->
    field_equals(A, B);
obj_equals(A, B) ->
    erlang:error({different, A, B}).

field_equals(A, B) ->
    lists:all(
      fun({K, V}) ->
              case lists:keysearch(K, 1, B) of
                  {value, {K, V2}} ->
                      value_equals(V, V2);
                  false ->
                      erlang:error({field_not_found, K, B})
              end
      end,
      A).

value_equals({struct, A}, B) ->
    obj_equals({struct, A}, B);
value_equals(A, A) ->
    true;
value_equals(A, B) ->
    erlang:error({different, A, B}).

get_existing_dbs(Access) ->
    Apps = cushion_util:start_app(cushion),
    try
        cushion:get_dbs(Access)
    after
        cushion_util:stop_apps(Apps)
    end.
