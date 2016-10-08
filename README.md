# hChat
Chat

practice 

参考
http://mryufeng.iteye.com/blog/366761

http://erlangcentral.org/wiki/index.php/Building_a_Non-blocking_TCP_server_using_OTP_principles

http://www.cnblogs.com/bicowang/p/3976129.html

client:
{ok,S} = gen_tcp:connect({127,0,0,1},2222,[{packet,2}]).
gen_tcp:send(S, <<4:16, 10000:16, >>).