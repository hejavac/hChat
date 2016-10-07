
-module(hs_account).

-export([
		login/3
		, logout/1
        , test_login/0
	]).

-include("hs_account.hrl").
-include("hs_role.hrl").

login(AccountName, Password, Socket) -> 
    io:format("~n M:~p L:~p AccountName:~p Password:~p ~n", [?MODULE, ?LINE, AccountName, Password]),
    Sql = io_lib:format(?sql_role_select, [AccountName, Password]),
    case db:fetch_one(Sql) of
        fail -> {false, no_this_account_name};
        Id -> 
            {ok, Pid} = hs_role:start_link(),
            do_login(Id, Pid, Socket)
    end.

do_login(Id, Pid, Socket) ->
    RoleSql = io_lib:format(?sql_role_base_data_select, [Id]),
    [NickName] = db:fetch_row(RoleSql),
    HsRole = #hs_role{
        id = Id
        , nickname = NickName
        , socket = Socket
        , pid = Pid
    },
    hs_role:set_hs_role(Pid, HsRole),
    {ok, Pid}.

logout(_Role) ->
	skip.

test_login() ->
    Sql = io_lib:format(?sql_role_select, ["1", "1"]),
    Result = db:fetch_row(Sql),
    io:format("~n M:~p L:~p Result:~p ~n", [?MODULE, ?LINE, Result]),
    ok.