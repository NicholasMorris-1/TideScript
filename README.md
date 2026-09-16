# TideScript: A Domain Specific Language for Peptide Chemistry

This is a prototype compiler for  *TideScript* , to accompany the 2025 SPLASH Onward! conference proceedings [paper](https://dl.acm.org/doi/10.1145/3759429.3762627). 

Currently the compiler just prints out the state of the program, we are working on incorporating back-ends for execution on real hardware.

## Installation 

Ensure you have [OCaml installed](https://ocaml.org/docs/installing-ocaml) and it's main package manager, [Opam](https://opam.ocaml.org/doc/Install.html).  

Clone the repo 

``` shell
git clone https://github.com/NicholasMorris-1/TideScript
cd TideScript
```

Install the dependecies neccesarry 

``` shell

opam init
opam switch create TideScript 4.14.2
opam install dune menhir ounit2
eval $(opam env)
``` 
You can replace \<TideScript\> with whatever description you would like, and 4.14.2 with a different compiler version if you so chose.
### Nix users 
The root directory provides a Nix shell containing TideScript, BigraphER, and
their native build dependencies. Enter it from the repository root with:

``` shell
nix-shell
```

This makes both commands available:

``` shell
tidescript --help
bigrapher --help
```

To build either package independently, run:

``` shell
nix-build -A tideScript
nix-build -A bigrapher
```

The resulting packages are placed in the Nix store, with a `result` symlink
created in the repository for each `nix-build` command.

## Running Scripts


You may need to run ```eval $(opam env)``` each new session depending on how you installed/initialised opam. 

Example scripts are found in the examples directory, these are the same scripts presented in the paper. From the root directory you can run ```bash compile.sh``` to test out examples, and you can comment out lines acordingly. Feel free to create your own scripts! 

Or you can run (from the data/ directory) 

```
dune build
cat ../examples/<example>.tide | OCAMLRUNPARAM=b  dune exec tidescript
``` 
replacing \<example\> with your desired script.



