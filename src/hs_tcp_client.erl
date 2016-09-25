
-module(hs_tcp_client).

-behaviour(gen_fsm).

-export([stark_link/0, set_socket/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
    	handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% FSM STates
-export([
		wait_for_socket/2
		, wait_for_data/2
	]).

%%
-define(LOGIN, 1).

-record(state, {
        socket
        , addr
        , pid
        , login
        , ref
    }).

start_link() ->
    gen_fsm:start_link(?MODULE, [], []).
 
set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
    gen_fsm:send_event(Pid, {socket_ready, Socket}).

init([]) ->
    process_flag(trap_exit, true),
    {ok, wait_for_socket, #state{}}.

wait_for_socket({socket_ready, Socket}, State) when is_port(Socket) ->
    Ref = async_recv(Socket, ?HEADER_LENGTH, ?HEART_TIMEOUT),
    inet:setopts(Socket, [{packet, 0}, binary]),
    {ok, {IP, _Port}} = inet:peername(Socket),
    {next_state, wait_for_account, State#state{socket=Socket, addr=IP, ref = Ref}};
wait_for_socket(Other, State) ->
    error_logger:error_msg("State: 'WAIT_FOR_SOCKET'. Unexpected message: ~p\n", [Other]),
    %% Allow to receive async messages
    {next_state, wait_for_socket, State}.

wait_for_account({inet_async, Socket, Ref, {ok, <<Len:16, Cmd:16>>}}, #state{ref = Ref, socket = Socket}) ->


wait_for_data({data, Data}, #state{socket=S} = State) ->
    ok = gen_tcp:send(S, Data),
    {next_state, wait_for_data, State, ?TIMEOUT};
 
wait_for_data(timeout, State) ->
    error_logger:error_msg("~p Client connection timeout - closing.\n", [self()]),
    {stop, normal, State};

handle_event(Event, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.

handle_sync_event(Event, _From, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.

handle_info({inet_async, Socket, Ref, {ok, Binary}}, StateName, #state{socket=Socket} = StateData) ->
    
    ?MODULE:StateName({inet_async, Socket, Ref, {ok, Binary}}, StateData);

handle_info({inet_async, Socket, Ref, {error,timeout}}, _StateName,
            #state{socket=Socket, addr=Addr} = StateData) ->
    error_logger:info_msg("~p Client ~p disconnected.\n", [self(), Addr]),
    {stop, normal, StateData};

%% 接受信息
async_recv(Sock, Length, Timeout) when is_port(Sock) ->
    case prim_inet:async_recv(Sock, Length, Timeout) of
        {error, Reason} -> throw({Reason});
        {ok, Res}       -> Res;
        Res             -> Res
    end.
