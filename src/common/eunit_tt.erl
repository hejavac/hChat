
-module(eunit_tt).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

a_test() ->
    [1] = lists:reverse([]).