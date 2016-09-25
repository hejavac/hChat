%% 配置
{
    application, hs,
    [
        {description, "Chat"},
        {vsn, "1.0"},
        {id, "tcp_server"},
        {modules,      [hs_app]},
        {registered,   [hs_app]},
        {applications, [kernel, stdlib]},
        %%
        %% mod: Specify the module name to start the application, plus args
        %%
        {mod, {hs_app, []}},
        {env, []}
    ]
}.

%% http://blog.csdn.net/mochinoname/article/details/6614504 启动参数
%% 比较完整的资源文件：
%% {application,test,                      % 名称
%%    [{description,"Test application"},   % 描述
%%     {vsn, "1.0.0"},                     % 版本
%%     {id, Id},                           % id 同 erl -id ID
%%     {modules, [test_app,test_sup]},     % 所有模块，systools用来生成script/tar文件
%%     {maxP, Num},                        % 最大进程数
%%     {maxT, Time},                       % 运行时间 单位毫秒
%%     {registered, [test_app]},           % 指定名称，systools用来解决名字冲突
%%     {included_applictions, []},         % 指定子app，加载但不启动
%%     {mod, {test_app,[]}},               % 启动模块，[]为参数Args test_app:start(normal, Args)
%%     {env, []},                          % 配置env，可以使用application:get_env获取
%%     {applications,[kernel,stdlib]}]}.   % 依赖项，启动app前，将会首先启动的app