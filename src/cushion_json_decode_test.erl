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

%% Functions to manually explore symbol values generation
-export([json_value/0, json_number/0, json_string/0]).

%%%-------------------------------------------------------------------
%%% Debug generators. You can use those to sample pieces of the JSON grammar
%%% generators
%%%-------------------------------------------------------------------
json_value() ->
    ?LET(Tree, 'Value'(), lists:flatten(print(eqc_grammar:eval(Tree)))).

json_number() ->
    ?LET(Tree, 'Number'(), lists:flatten(print(eqc_grammar:eval(Tree)))).

json_string() ->
    ?LET(Tree, 'String'(), lists:flatten(print(eqc_grammar:eval(Tree)))).

%%%-------------------------------------------------------------------
%%% Terminal generators
%%%-------------------------------------------------------------------
false() -> terminal(false).
true() -> terminal(true).
null() -> terminal(null).
character() -> terminal(char, char_gen()).
digit() -> terminal(digit, in_intervals([{$0, $9}])).
digit19() -> terminal(digit19, in_intervals([{$1, $9}])).
zero() -> terminal(zero).
minus() -> terminal(minus).
'$empty'() -> terminal(empty).
decimal_point() -> terminal(decimal_point).
exp() -> terminal(exp, eqc_gen:elements([$E, $e])).
quotation_mark() -> terminal(quotation_mark).
plus() -> terminal(plus).

terminal(Terminal) ->
    {Terminal, eqc_gen:nat()}.

terminal(Terminal, Value) ->
    {Terminal, Value, eqc_gen:nat()}.

char_gen() ->
    eqc_gen:list(in_intervals([{$a, $z}, {$A, $Z}])).

in_intervals(Intervals) ->
    eqc_gen:elements(lists:flatten([lists:seq(A, B) || {A, B} <- Intervals])).

%%%-------------------------------------------------------------------
%%% Printer
%%%-------------------------------------------------------------------
print(Toks) ->
    [tok2string(Tok) || Tok <- Toks].

tok2string({empty, _}) -> [];
tok2string({false, _}) -> "false";
tok2string({true, _}) -> "true";
tok2string({null, _}) -> "null";
tok2string({string, S, _}) -> [$", S, $"];
tok2string({digit, N, _}) -> N;
tok2string({digit19, N, _}) -> N;
tok2string({zero, _}) -> $0;
tok2string({char, C, _}) -> C;
tok2string({quotation_mark, _}) -> $";
tok2string({decimal_point, _}) -> $.;
tok2string({exp, Exp, _}) -> Exp;
tok2string({minus, _}) -> $-;
tok2string({plus, _}) -> $+.

%%%-------------------------------------------------------------------
%%% Properties
%%%-------------------------------------------------------------------
prop_parse() ->
    ?FORALL(
       SymExpr, 'Value'(),
       begin
           JsonString = print(eqc_grammar:eval(SymExpr)),
           ?WHENFAIL(
              io:format("~nFailed string: '~s'~n", [JsonString]),
              try
                  cushion_json:json2erl(JsonString),
                  true
              catch
                  {bad_float, _F} ->
                      % XXX check that the exponent is grater than 309
                      true
              end)
       end).
