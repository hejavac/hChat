
-module(hs_pt_100).

-compile(export_all).

-export([read/2, write/2]).

read(10000, <<Bin/binary>>) ->
	{AccountName, Bin1} = protocol:read_string(Bin),
	{Password, _} = protocol:read_string(Bin1),
	{ok, login, [AccountName, Password]};

read(_Cmd, _Bin) ->
	{ok, []}.

write(10000, [IsLogin]) ->
	{ok, protocol:pack(10000, <<IsLogin:8>>)};

write(_Cmd, _Data) ->
	{ok, protocol:pack(0, <<>>)}.