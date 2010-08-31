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
    eqc_gen:oneof([in_bool(), in_string(), in_number()]).

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

%%%-------------------------------------------------------------------
%%% Properties
%%%-------------------------------------------------------------------

%% Typical roundtrip test: test that encode(decode(X)) == X
prop_roundtrip() ->
    ?FORALL(
       V,
       in_value(),
       eqc:equals(cushion_json:json2erl(cushion_json:erl2json(V)), V)).
