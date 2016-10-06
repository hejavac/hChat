
%% 配置读取

-module(config).

-export([
        get_port/0
    ]).

-define(DEF_PORT, 2222).

%% 获得端口
get_port() -> 
    application:get_env(hs, port, ?DEF_PORT).
