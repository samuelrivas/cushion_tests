%%%-------------------------------------------------------------------
%%% @author Samuel Rivas <samuelrivas@gmail.com>
%%% @copyright (C) 2010, Samuel Rivas
%%% @doc QuickCheck tests for some cushion_util functions
%%%
%%% @end
%%% Created : 27 Dec 2010 by Samuel Rivas <samuelrivas@gmail.com>
%%%-------------------------------------------------------------------
-module(cushion_util_tests).

-include_lib("eqc/include/eqc.hrl").

-export([prop_unicode_roundtrip/0, generate_static_suite/0, test_static_suite/0,
         show_static_suite/0]).

prop_unicode_roundtrip() ->
    ?FORALL(
       L, eqc_gen:list(cushion_tests_gen:unicode_char()),
       eqc:equals(
         L, cushion_util:binary_to_unicode(cushion_util:unicode_to_binary(L)))).

%%%-------------------------------------------------------------------
%%% Static suites
%%%-------------------------------------------------------------------

%% XXX code coverage is not a good feature for this test unless we cover-compile
%% stdlib. We just generate a rando
generate_static_suite() ->
    Suite = eqc_suite:random(eqc:numtests(1000, prop_unicode_roundtrip())),
    eqc_suite:write(static_suite_file(), Suite).

%% XXX This method uses implementation details of QuickCheck, it might stop
%% working in future releases of it
show_static_suite() ->
    {random, Cases} =
        binary_to_term(
          cushion_util:untuple(file:read_file(static_suite_file()))),
    io:format(
      "Next codepoints pass a roundtrip test "
      "binary_to_unicode/unicode_to_binary:~n"),
    lists:foreach(
      fun([Case]) -> io:format(" * ~500p~n", [Case]) end,
      Cases).

test_static_suite() ->
    eqc_suite:run(prop_unicode_roundtrip(), static_suite_file()).

%%%-------------------------------------------------------------------
%%% Internals
%%%-------------------------------------------------------------------

static_suite_file() ->
    filename:join(
      code:priv_dir(cushion_tests), "cushion_util_prop_roundtrip.suite").
