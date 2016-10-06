%% 聊天

-module(hs_chat).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    io:format("~n M:~p L:~p hs_chat init ~n", [?MODULE, ?LINE]),
    {ok, []}.

handle_cast(_R , State) ->
    {noreply, State}.

handle_call(_R , _FROM, State) ->
    {reply, ok, State}.

handle_info(_Reason, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra)->
    {ok, State}.
