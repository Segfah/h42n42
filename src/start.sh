#!/bin/bash

# Función para ejecutar make test.byte
run_make() {
    cd /app/src && eval $(opam env) && make test.byte
}

# Ejecuta make test.byte inicialmente
run_make


