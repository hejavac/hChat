
-module(hs_chat_101).

-export([handle/3]).

-include("hs_role.hrl").

handle(10100, HsRole, []) ->
    #hs_role{id = RoleId, socket = Socket} = HsRole,
    hs_chat:enter_chat(RoleId, Socket);

handle(10101, HsRole, [Msg]) ->
    #hs_role{id = RoleId, nickname = Nickname} = HsRole,
    hs_chat:send_to_all(RoleId, Nickname, Msg);

handle(_Cmd, _HsRole, _Data) ->
    skip.