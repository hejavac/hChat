

%% 登录
-define(sql_role_select, <<"select * from role where account_name = '~s' and password = '~s' limit 1">>).