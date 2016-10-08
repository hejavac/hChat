
-module(test_client).

-compile(export_all).

start_client(Account, Password) ->
    {ok, Socket} = gen_tcp:connect({127,0,0,1}, 2222, [{packet, 0}]),
    AccountBin = hs_pt:pack_string(Account),
    PasswordBin = hs_pt:pack_string(Password),
    Bin = hs_pt:pack(10000, <<AccountBin/binary, PasswordBin/binary>>),
    gen_tcp:send(Socket, Bin),
    Pid = spawn(fun() -> recv_msg(Socket) end),
    gen_tcp:controlling_process(Socket, Pid),
    {ok, Socket}.

recv_msg(Socket) ->  
    receive  
        {tcp, Socket, Binary} ->
            % Msg = binary_to_term(Bin),  
            io:format("~n M:~p L:~p Received Binary: ~p~n", [?MODULE, ?LINE, Binary]),  
            recv_msg(Socket);
        Other ->
            io:format("~n M:~p L:~p Received Other: ~p~n", [?MODULE, ?LINE, Other]),
            recv_msg(Socket)
    end. 

unpack() -> ok.