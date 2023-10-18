#!/bin/bash

# Function to clean containers and images
clean_docker() {
    docker stop $(docker ps -aq)
    docker rm $(docker ps -aq)
}

# Function to clean containers and images
clean_full_docker() {
    docker stop $(docker ps -aq)
    docker rm $(docker ps -aq)
    docker rmi $(docker images -q)
    docker builder prune

}

# Función para conectarse vía SSH
connect_ssh() {
    ssh root@localhost -p 2222
}

connect_ssh() {
    ssh root@localhost -p 2222
}

# Function to build and run the container
build_and_run() {
    docker build -t h42n42 .
    docker run --rm --name h42n42 -d -v $(pwd):/app -p 8080:8080 -p 2222:22 h42n42
}

# Función para recargar (ejecutar make test.byte dentro del contenedor)
reload() {
    CONTAINER_ID=$(docker ps -q --filter ancestor=h42n42) # Obtiene el ID del contenedor basado en la imagen h42n42
    docker exec -it $CONTAINER_ID /bin/sh -c "/app/src/start.sh"
}

# Check the provided argument
case "$1" in
    clean)
        clean_docker
        ;;
    fclean)
        clean_full_docker
        ;;
    ssh)
        connect_ssh
        ;;
    reload)
        reload
        ;;
    create)
        build_and_run
        ;;
    *)
        echo "Usage:"
        echo "./console.sh create  - Builds and runs the container."
        echo "./console.sh reload  - RE make test.byte."
        echo "./console.sh ssh     - Connects to the container via SSH on port 2222."
        echo "./console.sh clean   - Stops and removes all containers."
        echo "./console.sh fclean   - Stops and removes all containers and deletes all images."
        ;;
esac
