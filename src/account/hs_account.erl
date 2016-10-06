
-module(hs_account).

-export([
		login/3
		, logout/1
	]).

login(_AccountName, _Password, _Socket) ->
	ok.

logout(_Role) ->
	skip.