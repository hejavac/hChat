
-module(hs_tcp_client).

-behaviour(gen_fsm).

-export([start_link/0, set_socket/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
    	handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% FSM STates
-export([
		wait_for_socket/2
		, wait_for_account/2
		, wait_for_data/2
	]).

%% 登录标志位
-define(LOGIN, 1).

-define(TCP_TIMEOUT, 1000). 		% 解析协议超时时间
-define(HEART_TIMEOUT, 60000). 		% 心跳包超时时间
-define(HEART_TIMEOUT_TIME, 0). 	% 心跳包超时次数
-define(HEADER_LENGTH, 4). 			% 消息头长度

-record(state, {
        socket
        , addr
        , pid
        , login
        , account_name
        , ref
    }).

start_link() ->
    gen_fsm:start_link(?MODULE, [], []).
 
set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
    gen_fsm:send_event(Pid, {socket_ready, Socket}).

init([]) ->
    process_flag(trap_exit, true),
    {ok, wait_for_socket, #state{}}.

handle_event(Event, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.

handle_sync_event(Event, _From, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.

handle_info({inet_async, Socket, Ref, {ok, Binary}}, StateName, #state{socket=Socket} = StateData) ->
    ?MODULE:StateName({inet_async, Socket, Ref, {ok, Binary}}, StateData);

handle_info({inet_async, Socket, Ref, {error, timeout}}, _StateName,
            #state{socket=Socket, ref=Ref} = StateData) ->
    error_logger:info_msg("~p Client ~p disconnected.\n", [self(), Ref]),
    {stop, normal, StateData};

handle_info(_Info, StateName, State) ->
    {noreply, StateName, State}.

terminate(_Reason, _StateName, #state{socket=Socket}) ->
    (catch gen_tcp:close(Socket)),
    ok.
 
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%% --------------------------- 状态机函数 ------------------------------

%% 等待Socket
wait_for_socket({socket_ready, Socket}, State) when is_port(Socket) ->
    Ref = async_recv(Socket, ?HEADER_LENGTH, ?HEART_TIMEOUT),
    inet:setopts(Socket, [{packet, 0}, binary]),
    {ok, {IP, _Port}} = inet:peername(Socket),
    {next_state, wait_for_account, State#state{socket=Socket, addr=IP, ref = Ref}};
wait_for_socket(Other, State) ->
    error_logger:error_msg("State: 'WAIT_FOR_SOCKET'. Unexpected message: ~p\n", [Other]),
    {next_state, wait_for_socket, State}.

%% 登录
wait_for_account({inet_async, Socket, Ref, {ok, <<Len:16, Cmd:16>>}}, #state{ref = Ref, socket = Socket} = State) ->
	BodyLen = Len - ?HEADER_LENGTH,
	io:format("~n M:~p L:~p ~n", [?MODULE, ?LINE]),
	case BodyLen > 0 of
		true ->
			Ref1 = async_recv(Socket, BodyLen, ?TCP_TIMEOUT),
			receive
				{inet_async, Socket, Ref1, {ok, Binary}} ->
					case read_protocol(Cmd, Binary) of
						{ok, login, Data} ->
							case pp_account:hanlde(10000, [], Data) of
								true ->
									io:format("~n M:~p L:~p ~n", [?MODULE, ?LINE]),
									[AccountName, _Password] = Data,
									NewState = State#state{login = ?LOGIN, account_name = AccountName},
									Ref = async_recv(Socket, ?HEADER_LENGTH, ?HEART_TIMEOUT),
									{next_state, wait_for_data, NewState};
								false ->
									login_lost(Socket, State, 0, "login fail")
							end;
						Other ->
							login_lost(Socket, State, 0, Other)
					end;
				Other ->
					login_lost(Socket, State, 0, Other)
			end;
		false ->
			login_lost(Socket, State, 0, "login fail")
	end;
wait_for_account({inet_async, Socket, Ref, {error, timeout}}, #state{ref = Ref, socket = Socket} = State) ->
	login_lost(Socket, State, 0, "login timeout"),
	{stop, normal, State};
wait_for_account(Other, #state{socket = Socket} = State) ->
	login_lost(Socket, State, 0, Other),
	{stop, normal, State}.

wait_for_data({inet_async, Socket, Ref, {ok, <<Len:16, Cmd:16>>}}, #state{ref = Ref, socket = Socket} = State) ->
	BodyLen = Len - ?HEADER_LENGTH,
    case BodyLen > 0 of
        true ->
            Ref1 = async_recv(Socket, BodyLen, ?TCP_TIMEOUT),
            receive
               {inet_async, Socket, Ref1, {ok, Binary}} ->
                    case read_protocol(Cmd, Binary) of
                        %%这里是处理游戏逻辑
                        {ok, Data} ->
                            case catch gen:call(State#state.pid, '$gen_call', {'SOCKET_EVENT', Cmd, Data}) of
                                {ok,_Res} ->
                                    async_recv(Socket, ?HEADER_LENGTH, ?HEART_TIMEOUT);
                                {'EXIT',Reason} ->
                                    do_lost(Socket, State, Cmd, Reason)
                            end;
                        Other ->
                            do_lost(Socket, State, Cmd, Other)
                    end;
                 Other ->
                    do_lost(Socket, State, Cmd, Other)
            end;
        false ->
            case read_protocol(Cmd, <<>>) of
                %%这里是处理游戏逻辑
                {ok, Data} ->
                    case catch gen:call(State#state.pid, '$gen_call', {'SOCKET_EVENT', Cmd, Data}, 3000) of
                        {ok,_Res} ->
                            async_recv(Socket, ?HEADER_LENGTH, ?HEART_TIMEOUT);
                        {'EXIT',Reason} ->
                            do_lost(Socket, State, Cmd, Reason)
                    end;
                Other ->
                    do_lost(Socket, State, Cmd, Other)
            end
    end,
    {next_state, wait_for_data, State};
wait_for_data({inet_async, Socket, Ref, {error, timeout}}, #state{ref = Ref, socket = Socket} = State) ->
	do_lost(Socket, State, 0, "login timeout"),
	{stop, normal, State};
wait_for_data(Other, #state{socket = Socket} = State) ->
	do_lost(Socket, State, 0, Other),
	{stop, normal, State}.

%% ------------------------ 内部函数 ----------------------------------

%% 接受信息
async_recv(Sock, Length, Timeout) when is_port(Sock) ->
    case prim_inet:async_recv(Sock, Length, Timeout) of
        {error, Reason} -> throw({Reason});
        {ok, Res}       -> Res;
        Res             -> Res
    end.

read_protocol(Cmd, Binary) ->
    [H1, H2, _, _, _] = integer_to_list(Cmd),
    Module = list_to_atom("protocol_"++[H1,H2]),
    Module:read(Cmd, Binary).

login_lost(Socket, _State, _Cmd, Reason) ->
    gen_tcp:close(Socket),
    exit({unexpected_message, Reason}).

do_lost(Socket, _State, _Cmd, Reason) ->
    % mod_login:logout(State#client.player),
    gen_tcp:close(Socket),
    exit({unexpected_message, Reason}).