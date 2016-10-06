%% 数据库操作

-module(db).
-compile(export_all).

-include("hs_init.hrl").

%% mysql:fetch/2 -> {updated, #mysql_result{}} | {data, #mysql_result{}}

%% mysql数据库连接初始化
init() ->
    mysql:start_link(?DB, ?DB_HOST, ?DB_PORT, ?DB_USER, ?DB_PASS, ?DB_NAME, fun(_, _, _, _) -> ok end, ?DB_ENCODE),
    mysql:connect(?DB, ?DB_HOST, ?DB_PORT, ?DB_USER, ?DB_PASS, ?DB_NAME, ?DB_ENCODE, true),
    ok.

%% 执行并返还条数
fetch_do(Sql) -> 
	case mysql:fetch(?DB, Sql) of
        {updated, {_, _, _, R, _, _, _, _}} -> R;
        Error -> erlang:error({mysql_error, Sql, Error})
    end.

%% 查找行数
fetch_row(Sql) -> 
	case mysql:fetch(?DB, Sql) of
        {data, {_, _, [], _, _, _, _, _}} -> [];
        {data, {_, _, [R], _, _, _, _, _}} -> R;
        Error -> erlang:error({mysql_error, Sql, Error})
    end.

%% 查找所有
fetch_all(Sql) -> 
	case mysql:fetch(?DB, Sql) of
        {data, {_, _, R, _, _, _, _, _}} -> R;
        Error -> erlang:error({mysql_error, Sql, Error})
    end.

%% 事务管理
transaction(F) -> 
	case mysql:transaction(?DB, F) of
        {atomic, R} -> R;
        {updated, {_, _, _, R, _, _, _, _}} -> R;
        Error -> erlang:error({mysql_error, Error})
    end.