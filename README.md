# Installation sur mac

## INSTALLATION
```
brew install opam
opam init
opam install eliom Js_of_ocaml
opam install ocsipersist-pgsql
opam install cstruct


echo "export DYLD_LIBRARY_PATH=/Users/corozco/.opam/default/lib/stublibs:$DYLD_LIBRARY_PATH" >> ~/.zshrc
```

## EXECUTION
```
make test.byte
```

## URL
localhost:8080