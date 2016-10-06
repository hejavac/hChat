
-module(hs_pt_100).

-compile(export_all).

-export([unpack/2, pack/2]).

unpack(10000, <<Bin/binary>>) ->
    {AccountName, Bin1} = ps_pt:unpack_string(Bin),
    {Password, _} = ps_pt:unpack_string(Bin1),
    {ok, login, [AccountName, Password]};

unpack(_Cmd, _Bin) ->
    {ok, []}.

pack(10000, [IsLogin]) ->
    {ok, ps_pt:pack(10000, <<IsLogin:8>>)};

pack(_Cmd, _Data) ->
    {ok, ps_pt:pack(0, <<>>)}.
