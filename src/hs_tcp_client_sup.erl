
-module(hs_tcp_client_sup).

-behaviour(supervisor).

-export([start_client/0, start_link/0]).
-export([init/1]).

start_client() ->
    supervisor:start_child(hs_tcp_client_sup, []).

start_link() ->
    supervisor:start_link({local, hs_tcp_client_sup}, ?MODULE, []).

init([]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, 5, 60},
            [
            % TCP Client
            {   
                hs_tcp_client,
                {hs_tcp_client, start_link, []},
                temporary,
                2000,
                worker,
                [hs_tcp_client]
            }
            ]
        }
    }.