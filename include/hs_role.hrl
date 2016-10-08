%% 玩家相关record


%% 玩家record
-record(hs_role, {
        id = 0                  % 角色id
        , nickname = ""         % 昵称
        , pid = 0               % 进程pid
        , socket = 0            % socket
        , login_flag = 0        % 登录标志 0:没有登录 1:登录
    }).
