opam init
eval $(opam env)
eval `opam config env`
opam switch
opam install -y ocaml-makefile merlin tuareg ocp-indent
opam user-setup install