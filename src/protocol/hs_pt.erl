
-module(hs_pt).

-export([send/2]).

-export([unpack_string/1, pack/2]).

-include("hs_pt.hrl").

send(S, Bin) when is_port(S) ->
    gen_tcp:send(S, Bin);
send(_S, _Bin) ->
    skip.

%%读取字符串
unpack_string(<<Len:16, Bin1/binary>>) ->
    case Bin1 of
        <<Str:Len/binary-unit:8, Rest/binary>> ->
            {binary_to_list(Str), Rest};
        _R1 ->
            {[], <<>>}
    end;
unpack_string(_) ->
    {[], <<>>}.

pack(Cmd, Data) ->
    L = byte_size(Data) + ?HEADER_LENGTH,
    <<L:16, Cmd:16, Data/binary>>.