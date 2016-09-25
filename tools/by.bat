goto start
:: =======编译完：=========
::
:: 需要手动输入以下三个中一个
:: 无监控树：chat_server:start_link().
:: 监控树：chat_server_sup:start_link().
:: 行为启动，有带监控树：chat_server_app:start([], []).
:start

cd ../

erl -noshell -s make all -s init stop

erl -pa ./ebin
