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

-export([prop_unicode_roundtrip/0]).

prop_unicode_roundtrip() ->
    ?FORALL(
       L, eqc_gen:list(cushion_tests_gen:unicode_char()),
       eqc:equals(
         L, cushion_util:binary_to_unicode(cushion_util:unicode_to_binary(L)))).

