
-module(mod_role).

-behaviour(gen_server).
-export([start/0, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([]).

%%开始
start() ->
    gen_server:start(?MODULE, [], []).

%%停止本游戏进程
stop(Pid) when is_pid(Pid) ->
    gen_server:cast(Pid, stop).

init([]) ->
    process_flag(priority, max),
    {ok, none}.

%%停止游戏进程
handle_cast(stop, Status) ->
    {stop, normal, Status};

handle_cast(_Event, Status) ->
    {noreply, Status}.

%%处理socket协议
%%cmd：命令号
%%data：协议体
handle_call({'SOCKET_EVENT', Cmd, Bin}, _From, Status) ->
    case handle_protocol(Cmd, Status, Bin) of
        {ok, NewStatus} ->
            
            {reply, ok, NewStatus};
        _R ->
            {reply, ok, Status}
    end;

handle_call(_Event, _From, Status) ->
    {reply, ok, Status}.

handle_info(_Info, Status) ->
    {noreply, Status}.

%%游戏进程死掉修改状态
terminate(_Reason, Status) ->
    %%玩家下线，如有队伍，则离开队伍
    pp_team:handle(24005, Status, offline),
    spawn(fun() -> mod_login:logout(Status) end),
    ok.

code_change(_oldvsn, Status, _extra) ->
    {ok, Status}.

%% 路由
%%cmd:命令号
%%Socket:socket id
%%data:消息体
handle_protocol(Cmd, Status, Bin) ->
    %%取前面二位区分功能类型
    [H1, H2, _, _, _] = integer_to_list(Cmd),
    case [H1, H2] of
        %%游戏基础功能处理
        "11" -> pp_chat:handle(Cmd, Status, Bin);
        %%错误处理
        _ ->
            {error, "Routing failure"}
    end.