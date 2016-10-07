goto start

:start

cd ..

cd config

erl -boot start_sasl -config hs -pa ../ebin

pause