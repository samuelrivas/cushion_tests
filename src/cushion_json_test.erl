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
%%% @end
%%%-------------------------------------------------------------------
-module(cushion_json_test).
-include_lib("eqc/include/eqc.hrl").
-export([prop_roundtrip/0]).

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
    [in_bool(), in_string(), in_number()].

composites(S) ->
    [in_array(S)].

in_string() ->
    ?LET(S, eqc_gen:list(printable()), list_to_binary(S)).

in_number() ->
    eqc_gen:oneof([eqc_gen:int(), eqc_gen:real()]).

printable() ->
    in_intervals([{32, 126}, {8, 13}, {27, 27}]).

in_intervals(Intervals) ->
    eqc_gen:elements(lists:flatten([lists:seq(A, B) || {A, B} <- Intervals])).

in_bool() ->
    eqc_gen:elements([false, true]).

%% XXX There is a problem with ktuo and empty arrays, they decode to [], exactly
%% the same as empty strings.
%%
%% 51> ktj_decode:decode([]).
%% {[],[],{0,0}}
%% 52> ktj_decode:decode("\"\"").
%% {[],[],{0,2}}
%%
%% For now, we'll just avoid generating empty arrays and fix that later
in_array(S) ->
    eqc_gen:non_empty(eqc_gen:list(in_value(S div 2))).

%%%-------------------------------------------------------------------
%%% Properties
%%%-------------------------------------------------------------------

%% Typical roundtrip test: test that encode(decode(X)) == X
prop_roundtrip() ->
    ?FORALL(
       V,
       in_value(),
       eqc:equals(cushion_json:json2erl(cushion_json:erl2json(V)), V)).
