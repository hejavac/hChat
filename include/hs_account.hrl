

%% 登录
-define(sql_role_select, <<"select id from role where account_name = '~s' and password = '~s' limit 1">>).

%% 基础信息获取
-define(sql_role_base_data_select, <<"select nick_name from role where id = ~p">>).