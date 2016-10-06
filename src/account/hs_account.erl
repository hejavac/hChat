
-module(hs_account).

-export([
		login/3
		, logout/1
        , test_login/0
	]).

-include("hs_account.hrl").

login(_AccountName, _Password, _Socket) -> ok.
	

logout(_Role) ->
	skip.

test_login() ->
    Sql = io_lib:format(?sql_role_select, ["1", "1"]),
    Result = db:fetch_row(Sql),
    io:format("~n M:~p L:~p Result:~p ~n", [?MODULE, ?LINE, Result]),
    ok.