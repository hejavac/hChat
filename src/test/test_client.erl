
-module(test_client).

-compile(export_all).

start_client() ->
    {ok, S} = gen_tcp:connect({127,0,0,1}, 2222, [{packet, 0}]),
    AccountBin = hs_pt:pack_string(<<"1">>),
    PasswordBin = hs_pt:pack_string(<<"1">>),
    Bin = hs_pt:pack(10000, <<AccountBin/binary, PasswordBin/binary>>),
    gen_tcp:send(S, Bin),
    {ok, S}.