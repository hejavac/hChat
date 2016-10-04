
-module(hs_tcp_listener_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Port) ->
    supervisor:start_link(hs_tcp_listener_sup, [Port]).

init([Port]) ->
    {ok, 
        {
            {one_for_one, 1, 60},
            [
            % TCP Listener supervisor
            {   hs_tcp_listener, 
                {hs_tcp_listener, start_link, [Port]},
                permanent,
                infinity,
                supervisor, 
                [hs_tcp_listener_sup]
            }
            ]
        }
    }.