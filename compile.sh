cd data;
dune clean;
dune build;
#cat test1.nick | ~/NickLang/_build/default/bin/./main.exe
#cat ../scripts/test3.tide |  ~/TideScript/_build/default/bin/./main.exe
#cat ../examples/test1.tide |  dune exec tidescript
#cat ../examples/test2.tide |  dune exec tidescript
#cat ../examples/test2.tide | OCAMLRUNPARAM=b  dune exec tidescript
cat ../examples/test3.tide | OCAMLRUNPARAM=b  dune exec tidescript
