
-module(protocol).
-export([read_string/1, pack/2]).

-include("protocol.hrl").

%%读取字符串
read_string(<<Len:16, Bin1/binary>>) ->
    case Bin1 of
        <<Str:Len/binary-unit:8, Rest/binary>> ->
            {binary_to_list(Str), Rest};
        _R1 ->
            {[],<<>>}
    end;
read_string(_) ->
    {[],<<>>}.

pack(Cmd, Data) ->
    L = byte_size(Data) + ?HEADER_LENGTH,
    <<L:16, Cmd:16, Data/binary>>.