
-module(hs_app).

-behaviour(application).

%% Application and Supervisor callbacks
-export([start/0, start/2, stop/1, init/1]).

start() ->
	io:format("~n M:~p L:~p hs_app~n", [?MODULE, ?LINE]),
    application:start(hs),
    ok.

%%----------------------------------------------------------------------
%% Application behaviour callbacks
%%----------------------------------------------------------------------
start(_Type, _Args) ->
    % 数据库初始化
    db:init(),
    hs_account:test_login(),
    ListenPort = config:get_port(),
    io:format("~n M:~p L:~p ListenPort:~p ~n", [?MODULE, ?LINE, ListenPort]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ListenPort]).
 
stop(_S) ->
    ok.

%%----------------------------------------------------------------------
%% Supervisor behaviour callbacks
%%----------------------------------------------------------------------
init([Port]) ->
	io:format("~n M:~p L:~p Port:~p ~n", [?MODULE, ?LINE, Port]),
    {ok,
        {
            {one_for_one, 5, 10},
            [
            % TCP Listener supervisor
            {   hs_tcp_listener_sup, 
                {hs_tcp_listener_sup, start_link, [Port]},
                permanent,
                infinity,
                supervisor, 
                [hs_tcp_listener_sup]
            },
            % Client instance supervisor
            {   hs_tcp_client_sup,
                {hs_tcp_client_sup, start_link, []},
                permanent,
                infinity,
                supervisor,
                [hs_tcp_client_sup]
            },
            % 聊天进程
            {   hs_chat,
                {hs_chat, start_link, []},
                permanent,
                infinity,
                worker,
                [hs_chat]
            }
            ]
        }
    }.


%% 知识点
% http://erldoc.com/doc/otp-design-principles/supervisor.html
% {Id, StartFunc, Restart, Shutdown, Type, Modules}
%     Id = term()
%     StartFunc = {M, F, A}
%         M = F = atom()
%         A = [term()]
%     Restart = permanent | transient | temporary
%     Shutdown = brutal_kill | integer() >=0 | infinity
%     Type = worker | supervisor
%     Modules = [Module] | dynamic
%         Module = atom()

% Id 是督程内部用于标识子进程规范的名称。
% StartFunc 定义了用于启动子进程的很难书调用。它是一个模块.函数.参数的元组，与 apply(M, F, A) 用的一样。
% Restart 定义了一个被终止的子进程要在何时被重启：
% permanent 子进程总会被重启。
% temporary 子进程从不会被重启。
% transient 子进程只有当其被异常终止时才会被重启，即，退出理由不是 normal 。
% Shutdown 定义了一个子进程应如何被终止。
% brutal_kill 表示子进程应使用 exit(Child, kill) 进行无条件终止。
% 一个整数超时值表示督程先通过调用 exit(Child, shutdown) 告诉子进程要终止了，然后等待其返回退出信号。如果在指定的事件内没有接受到任何退出信号，那么使用 exit(Child, kill) 无条件终止子进程。
% 如果子进程是另外一个督程，那么应该设置为 infinity 以给予子树足够的时间关闭。
% Type 指定子进程是督程还是佣程。
% Modules 应该为只有一个元素的列表 [Module]，其中 Module 是回调模块的名称，如果子进程是督程、gen_server或者gen_fsm。如果子进程是一个gen_event，那么 Modules 应为 dynamic 。 在升级和降级过程中发布处理器将用到这个信息，参见 发布处理 。