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
%%% @doc Tests for cushion_json
%%%
%%% This suite tests that any json term we can construct in the erlang side can
%%% be serialised to a JSON string and deserialised from it safely.
%%%
%%% It doesn't fully test that any JSON string can be deserialised safely, since
%%% only strings created by the serialiser are tested.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cushion_json_test).
-include_lib("eqc/include/eqc.hrl").
-export([prop_roundtrip/0]).

%% Generators used in other tests
-export([in_object/0]).

%%%-------------------------------------------------------------------
%%% Generators
%%%-------------------------------------------------------------------

%% This generator mimics ktuo (ktj_encode) documentation
in_value() ->
    ?SIZED(S, in_value(S)).

in_value(0) ->
    eqc_gen:oneof(terminals());
in_value(S) ->
    eqc_gen:oneof(terminals() ++ composites(S)).

terminals() ->
    [in_bool(), in_string(), in_number(), in_null()].

composites(S) ->
    [in_array(S), in_object(S)].

%% Generate a list of unicode characters and encode it to an utf-8 binary
in_string() ->
    ?LET(
       S,
       eqc_gen:list(cushion_tests_gen:unicode_char()),
       cushion_util:unicode_to_binary(S)).

in_number() ->
    eqc_gen:oneof([eqc_gen:int(), eqc_gen:real()]).

in_bool() ->
    eqc_gen:elements([false, true]).

in_null() ->
    null.

%% Arrays shrink to the terms they contain
in_array(S) ->
    ?LETSHRINK(
       L, eqc_gen:list(in_value(S div 2)), L).

%% Objects shrink to one of their field values or field names, otherwise deeply
%% nested object with a failing case in them would be difficult to debug.
in_object() ->
    ?SIZED(S, in_object(S, true)).

in_object(S) ->
    in_object(S, false).

in_object(S, Special) ->
    ?LET(
       Fields, fields(S, Special),
       ?SHRINK(
          {obj, Fields},
          [V || {_K, V} <- Fields] ++ [K || {K, _V} <- Fields])).

fields(S, Special) ->
    ?LET(
       Fields, eqc_gen:list(field(S div 2, Special)),
       remove_dups(Fields)).

%% Remove duplicated field keys to avoid inconsistencies
remove_dups(Fields) ->
    remove_dups(Fields, []).

remove_dups([{K, V}|T], Ks) ->
    case lists:member(K, Ks) of
        true ->
            remove_dups(T, Ks);
        false ->
            [{K, V} | remove_dups(T, [K|Ks])]
    end;
remove_dups([], _) ->
    [].

field(S, Special) ->
    {field_name(Special), in_value(S)}.

field_name(false) ->
    in_string();
field_name(true) ->
    ?SUCHTHAT(
       S, in_string(),
       case S of
           <<"_", _/binary>> ->
               false;
           _ ->
               true
       end).

%%%-------------------------------------------------------------------
%%% Properties
%%%-------------------------------------------------------------------

%% Typical roundtrip test: test that encode(decode(X)) == X
prop_roundtrip() ->
    ?FORALL(
       V,
       in_value(),
       eqc:equals(cushion_json:json2erl(cushion_json:erl2json(V)), V)).
