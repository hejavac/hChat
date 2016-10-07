# hChat
Chat

practice 

client:
{ok,S} = gen_tcp:connect({127,0,0,1},2222,[{packet,2}]).
gen_tcp:send(S, <<4:16, 10000:16, >>).