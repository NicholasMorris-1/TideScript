cd data;
dune clean;
dune build;
cat ../examples/test1.tide |  dune exec tidescript
#cat ../examples/test2.tide |  dune exec tidescript
#cat ../examples/test2.tide |   dune exec tidescript
#cat ../examples/test3.tide |   dune exec tidescript
cat ../examples/test4.tide |   dune exec tidescript
