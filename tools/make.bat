cd ../
::-eval "case make:files([\"src/mmake.erl\"], [{outdir, \"ebin\"}]) of error -> halt(1); _ -> ok end"
erl -eval "case mmake:all(10) of up_to_date -> halt(0); error -> halt(1) end."
pause