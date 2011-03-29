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
digit() -> terminal(digit, in_intervals([{$0, $9}])).
digit19() -> terminal(digit19, in_intervals([{$1, $9}])).
zero() -> terminal(zero).
minus() -> terminal(minus).
'$empty'() -> terminal(empty).
decimal_point() -> terminal(decimal_point).
escape() -> terminal(escape).
unescaped() -> terminal(unescaped, unescaped_gen()).
escaped() -> terminal(escaped, escaped_gen()).
exp() -> terminal(exp, eqc_gen:elements([$E, $e])).
quotation_mark() -> terminal(quotation_mark).
plus() -> terminal(plus).

terminal(Terminal) ->
    {Terminal, eqc_gen:nat()}.

terminal(Terminal, Value) ->
    {Terminal, Value, eqc_gen:nat()}.

%% TODO: Finish this, adding unicode characters from 5d
unescaped_gen() ->
    eqc_gen:frequency(
      [
       %! and white space
       {3, eqc_gen:choose(16#20, 16#21)},

       % #$%&'()*+,-./:;<=>?@[ numbers and big letters
       {5, eqc_gen:choose(16#23, 16#5b)},

       % ]^_`{|}~ and small letters
       {5, eqc_gen:choose(16#5d, 16#7e)},

       % Rest of the unicode range
       {1, utf8_list()}]).

%% Generate unicode characters beyond 127 already translated to utf-8, since
%% ktuo won't handle unicode characters
%%
utf8_list() ->
    ?LET(
       U,
       ?SUCHTHAT(C, cushion_tests_gen:unicode_char(), C > 16#7e),

       % Tricky way to convert an unicode character to a list representing it in
       % utf-8
       binary_to_list(cushion_util:unicode_to_binary([U]))).

escaped_gen() ->
    eqc_gen:oneof(
      [eqc_gen:elements([$", $\\, $/, $b, $f, $n, $r, $t]), u_encoded()]).

%% TODO: Add characters from u10000
u_encoded() ->
    ?LET(
       C,
       ?SUCHTHAT(U , cushion_tests_gen:unicode_char(), U < 16#1000),
       io_lib:format("u~4.16.0B", [C])).


in_intervals(Intervals) ->
    eqc_gen:oneof([eqc_gen:elements(lists:seq(A, B)) || {A, B} <- Intervals]).

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
tok2string({quotation_mark, _}) -> $";
tok2string({decimal_point, _}) -> $.;
tok2string({escape, _}) -> $\\;
tok2string({unescaped, C, _}) -> C;
tok2string({escaped, C, _}) -> C;
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
              io:format("~nFailed string: '~ts'~n", [JsonString]),
              begin
                  cushion_json:json2erl(JsonString),
                  true
              end)
       end).
