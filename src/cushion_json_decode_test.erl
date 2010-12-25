%%%-------------------------------------------------------------------
%%% @author Samuel Rivas <samuelrivas@gmail.com>
%%% @copyright (C) 2010, Samuel Rivas
%%% @doc Generate serialised JSON terms and check we can import them into erlang
%%% terms
%%%
%%% @end
%%% Created : 25 Dec 2010 by Samuel Rivas <samuelrivas@gmail.com>
%%%-------------------------------------------------------------------
-module(cushion_json_decode_test).

-include_lib("eqc/include/eqc.hrl").
-compile({parse_transform, eqc_grammar}).
-eqc_grammar({yecc_tokens, "../priv/json.yrl"}).

-export([prop_parse/0]).

%%%-------------------------------------------------------------------
%%% Terminal generators
%%%-------------------------------------------------------------------
false() -> terminal(false).
true() -> terminal(true).
null() -> terminal(null).
number() -> terminal(number, eqc_gen:int()).
string() -> terminal(string, string_gen()).

terminal(Terminal) ->
    {Terminal, eqc_gen:nat()}.

terminal(Terminal, Value) ->
    {Terminal, Value, eqc_gen:nat()}.

string_gen() ->
    eqc_gen:list(printable()).

printable() ->
    in_intervals([{32, 126}, {8, 13}, {27, 27}]).

in_intervals(Intervals) ->
    eqc_gen:elements(lists:flatten([lists:seq(A, B) || {A, B} <- Intervals])).

%%%-------------------------------------------------------------------
%%% Printer
%%%-------------------------------------------------------------------
print(Toks) ->
    [tok2string(Tok) || Tok <- Toks].

tok2string([]) ->
    "";
tok2string({false, _}) ->
    "false";
tok2string({true, _}) ->
    "true";
tok2string({null, _}) ->
    "null";
tok2string({number, N, _}) ->
    integer_to_list(N);
tok2string({string, S, _}) ->
    [$", S, $"].

%%%-------------------------------------------------------------------
%%% Properties
%%%-------------------------------------------------------------------
prop_parse() ->
    ?FORALL(
       SymExpr, 'Value'(),
       begin
           cushion_json:json2erl(print(eqc_grammar:eval(SymExpr))),
           true
       end).
