
-module(hs_tcp_listener).
 
-behaviour(gen_server).

%% External API
-export([start_link/1]).
 
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {socket, ref}).

start_link(Port) ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [Port], []).

init([Port]) ->
    Opts = [binary, {packet, 0}, {reuseaddr, true},
            {keepalive, true}, {backlog, 30}, {active, false}],
    io:format("~n M:~p L:~p Port:~p ~n", [?MODULE, ?LINE, Port]),
    case gen_tcp:listen(Port, Opts) of
        {ok, LSocket} ->
            {ok, Ref} = prim_inet:async_accept(LSocket, -1),
            io:format("~n M:~p L:~p LSocket:~p Ref:~p ~n", [?MODULE, ?LINE, LSocket, Ref]),
            {ok, #state{socket = LSocket, ref = Ref}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call(Request, _From, State) ->
    {noreply, {unknown_call, Request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({inet_async, LSocket, Ref, {ok, Socket}}, 
        #state{socket=LSocket, ref=Ref} = State) ->
    try
        case set_sockopt(LSocket, Socket) of
            ok -> ok;
            {error, Reason} -> exit({set_sockopt, Reason})
        end,
        {ok, Pid} = hs_tcp_client_sup:start_client(),
        % 把Sock绑定到Pid中
        gen_tcp:controlling_process(Socket, Pid),
        hs_tcp_client:set_socket(Pid, Socket),
        % 监听下一个
        case prim_inet:async_accept(LSocket, -1) of
            {ok, NewRef} -> ok;
            {error, NewRef} -> exit({async_accept, inet:format_error(NewRef)})
        end,
        {noreply, State#state{ref=NewRef}}
    catch exit:Why ->
        error_logger:error_msg("Error in async accept: ~p.\n", [Why]),
        {stop, Why, State}
    end;

handle_info({inet_async, LSock, Ref, Error}, #state{socket=LSock, ref=Ref} = State) ->
    error_logger:error_msg("Error in socket ref: ~p.\n", [Error]),
    {stop, Error, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% 设置Scoket属性
set_sockopt(LSocket, Socket) ->
    {ok, Mod} = inet_db:lookup_socket(LSocket),
    % Mod = inet_tcp
    true = inet_db:register_socket(Socket, Mod),
    case prim_inet:getopts(LSocket, [active, nodelay, keepalive, delay_send, priority, tos]) of
        {ok, Opts} ->
            case prim_inet:setopts(Socket, Opts) of
                ok -> ok;
                Error -> gen_tcp:close(Socket), Error
            end;
        Error ->
            gen_tcp:close(Socket), Error
    end.

%% 知识点:
%% process_flag(trap_exit, true)变成系统进程,当系统进程收到错误信号时,该信号会被转换成{'EXIT', Pid, Why}
%% 不可捕捉的退出信号:exit(Pid, kill)

%% 监视:erlang:monitor(process,Item)->Ref,监视是单向的,如果A监视B而B挂了,就会向A发送{'DOWN',Ref,process,Pid,Why}
% on_exit(Pid, Fun) ->
%     spawn(fun() ->
%         Ref = monitor(process, Pid),
%         receive
%             {'DOWN', Ref, process, Pid, Why} ->
%                 Fun(Why)
%         end
%     end).

%% prim_inet:async_accept(Socket, -1)函数解析
%% ACCEPT(insock() [,Timeout] ) -> {ok,insock()} | {error, Reason}
%%
%% accept incoming connection on listen socket 
%% if timeout is given:
%%       timeout < 0  -> infinity
%%                 0  -> immediate accept (poll)
%%               > 0  -> wait for timeout ms for accept if no accept then 
%%                       return {error, timeout}
%%
%% ASYNC_ACCEPT(insock(), Timeout)

%% gen_tcp Opts
%% binary : Received Packet is delivered as a binary.
%% reuseaddr : 重启Erlang Socket服务时，gen_tcp:listen/2返回{error, eaddrinuse}错误，是因为上次关闭Socket服务后，TCP 套接字仍处于TIME_WAIT状态，等待数分钟，TIME_WAIT 状态退出之后可以正常启动。如需立即启动，可以在调用gen_tcp:listen/2时将reuseaddr选项设置为true，声明可重用端
%% keepalive : Enables/disables periodic transmission on a connected socket when no other data is exchanged. If the other end does not respond, the connection is considered broken and an error message is sent to the controlling process. Defaults to disabled.
% SocketOpts = [  
%    binary,   
%    {packet, 0}, %%{packet, 0}表示erlang系统会吧TCP数据原封不动地直接传送给应用程序  
%    {reuseaddr, true},  %%允许本地重复使用端口号  
%    {nodelay, true},  %%意味着很少的数据也会被马上被发送出去   
%    {delay_send, true},  %%如果打开了delay_send，每个port会维护一个发送队列，数据不是立即发送，而是存到发送队列里，等socket可写的时候再发送，相当于是ERTS自己实现的组包机制  
%    {active, false}, %%注意如果socket客户端断开后，其port不会关闭，而{active,true}与{active,once}则会关闭port  
%    {backlog, 1024}, %%缓冲区的长度  
%    {exit_on_close, false}, %%设置为flase，那么在socket被close之后还能将缓冲区中的数据发送出去  
%    {send_timeout, 15000} %%设置一个时间去等待操作系统发送数据，如果底层在这个时间段后还没发出数据，那么就会返回{error,timeout}  
%   ]  