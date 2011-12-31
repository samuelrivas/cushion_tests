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

-export([prop_parse/0, generate_static_suite/0, test_static_suite/0,
         show_static_suite/0]).

%% Functions to manually explore symbol values generation
-export([json_value/0, json_number/0, json_string/0, json_object/0,
         json_array/0]).

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

json_object() ->
    ?LET(Tree, 'Object'(), lists:flatten(print(eqc_grammar:eval(Tree)))).

json_array() ->
    ?LET(Tree, 'Array'(), lists:flatten(print(eqc_grammar:eval(Tree)))).

%%%-------------------------------------------------------------------
%%% Terminal generators
%%%-------------------------------------------------------------------
false() -> false.
true() -> true.
null() -> null.
digit() -> {digit, in_intervals([{$0, $9}])}.
digit19() -> {digit19, in_intervals([{$1, $9}])}.
zero() -> zero.
minus() -> minus.
'$empty'() -> empty.
decimal_point() -> decimal_point.
escape() -> escape.
unescaped() -> {unescaped, unescaped_gen()}.
escaped() -> {escaped, escaped_gen()}.
exp() -> {exp, eqc_gen:elements([$E, $e])}.
quotation_mark() -> quotation_mark.
plus() -> plus.
left_curl() -> left_curl.
right_curl() -> right_curl.
left_bracket() -> left_bracket.
right_bracket() -> right_bracket.
colon() -> colon.
comma() -> comma.

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

%% Generate unicode characters beyond 127 already translated to utf-8
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

tok2string(empty) -> [];
tok2string(false) -> "false";
tok2string(true) -> "true";
tok2string(null) -> "null";
tok2string({string, S}) -> [$", S, $"];
tok2string({digit, N}) -> N;
tok2string({digit19, N}) -> N;
tok2string(zero) -> $0;
tok2string(quotation_mark) -> $";
tok2string(decimal_point) -> $.;
tok2string(escape) -> $\\;
tok2string({unescaped, C}) -> C;
tok2string({escaped, C}) -> C;
tok2string({exp, Exp}) -> Exp;
tok2string(minus) -> $-;
tok2string(plus) -> $+;
tok2string(left_curl) -> ${;
tok2string(right_curl) -> $};
tok2string(left_bracket) -> $[;
tok2string(right_bracket) -> $];
tok2string(colon) -> $:;
tok2string(comma) -> $,.

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

%%%-------------------------------------------------------------------
%%% Static suites
%%%-------------------------------------------------------------------
generate_static_suite() ->
    Suite = cushion_tests:generate_qc_suite(eqc:numtests(1000, prop_parse())),
    eqc_suite:write(static_suite_file(), Suite).

%% XXX This method uses implementation details of QuickCheck, it might stop
%% working in future releases of it
show_static_suite() ->
    {feature_based, Cases} =
        binary_to_term(
          cushion_util:untuple(file:read_file(static_suite_file()))),
    TokenLists = [eqc_grammar:eval(Case) || {_Lines, [Case]} <- Cases],
    io:format(
      "Next terms can be translated to Erlang without raising exceptions:~n"),
    lists:foreach(
      fun(Tokens) -> io:format(" * ~s~n", [print(Tokens)]) end,
      TokenLists).

test_static_suite() ->
    true.

%%%-------------------------------------------------------------------
%%% Internals
%%%-------------------------------------------------------------------

static_suite_file() ->
    filename:join(code:priv_dir(cushion_tests), "json_decode_prop_parse.suite").
