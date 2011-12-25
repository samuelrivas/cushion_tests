%%%-------------------------------------------------------------------
%%% @author Samuel Rivas <samuelrivas@gmail.com>
%%% @copyright (C) 2010, Samuel Rivas
%%% @doc Generic generators for QuickCheck tests
%%%
%%% @end
%%% Created : 27 Dec 2010 by Samuel Rivas <samuelrivas@gmail.com>
%%%-------------------------------------------------------------------
-module(cushion_tests_gen).

-export([unicode_char/0]).

%%--------------------------------------------------------------------
%% @doc Generates an unicode character
%%
%% This generator distributes character generation equally in several groups to
%% avoid biasing the generation too much to the long tail of unicode characters
%% after the most common characters and the most relevant 7-bit characters from
%% 0 to 127. See the code for more info.
%%
%% Lower values are weighted more than larger values
%%
%% @end
%%--------------------------------------------------------------------
unicode_char() ->
    eqc_gen:frequency(
      [{5, eqc_gen:choose(0, 31)}, % Some non printable characters here
       {5, eqc_gen:choose(32, 126)}, % "normal" characters
       {3, eqc_gen:choose(127, 255)},
       {1, eqc_gen:choose(255, 16#d7ff)},  % jump over reserved 0xD800-0xDFFF
       {1, eqc_gen:choose(16#e000, 16#fffd)}, % jump over 0xfffe and 0xffff
       {1, eqc_gen:choose(16#10000, 16#10ffff)} % remaining range of unicode
      ]).
