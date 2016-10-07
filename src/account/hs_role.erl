
-module(hs_role).

-behaviour(gen_server).

-export([start_link/0, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([
        set_hs_role/2
        , socket_protocol/3
        , socket_protocol/4
    ]).

set_hs_role(Pid, HsRole) ->
    gen_server:cast(Pid, {'set_hs_role', HsRole}).

%% 暂时不用,用gen:call能获得更多信息
socket_protocol(Pid, Cmd, Data) ->
    gen_server:call(Pid, {'socket_protocol', Cmd, Data}).

socket_protocol(Pid, Cmd, Data, Timeout) ->
    gen_server:call(Pid, {'socket_protocol', Cmd, Data}, Timeout).

start_link() ->
    gen_server:start(?MODULE, [], []).

stop(Pid) ->
    gen_server:cast(Pid, stop).

init([]) ->
    process_flag(priority, max),
    io:format("~n M:~p L:~p init ~n", [?MODULE, ?LINE]),
    {ok, none}.

handle_cast({'set_hs_role', HsRole}, _Status) ->
    {stop, normal, HsRole};

handle_cast(stop, Status) ->
    {stop, normal, Status};

handle_cast(_Event, Status) ->
    {noreply, Status}.

handle_call({'socket_protocol', Cmd, Bin}, _From, Status) ->
    case handle_socket_protocol(Cmd, Status, Bin) of
        {ok, NewStatus} ->
            {reply, ok, NewStatus};
        _R ->
            {reply, ok, Status}
    end;

handle_call(_Event, _From, Status) ->
    {reply, ok, Status}.

handle_info(_Info, Status) ->
    {noreply, Status}.

terminate(_Reason, Status) ->
    spawn(fun() -> hs_account:logout(Status) end),
    ok.

code_change(_oldvsn, Status, _extra) ->
    {ok, Status}.

handle_socket_protocol(Cmd, Status, Bin) ->
    [H1, H2, H3, _, _] = integer_to_list(Cmd),
    Module = get_handle_scoket_protocol_module([H1, H2, H3]),
    case is_atom(Module) of
        true -> Module:handle(Cmd, Status, Bin);
        false -> Module
    end.

get_handle_scoket_protocol_module("101") ->  hs_chat_101;
get_handle_scoket_protocol_module(_) ->  {error, "handle_socket_protocol failure"}.