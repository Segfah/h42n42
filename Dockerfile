FROM ubuntu:20.04

# Evitamos preguntas durante la instalación de paquetes
ENV DEBIAN_FRONTEND=noninteractive

# Instalamos las dependencias necesarias
RUN apt-get update && apt-get install -y \
    curl \
    gcc \
    git \
    m4 \
    make \
    unzip \
    bubblewrap \
    openssh-server \
    libgdbm-dev \
    pkg-config \
    libsqlite3-dev \
    zlib1g-dev \
    libssl-dev \
    libpcre3-dev \
    libgmp-dev \
    inotify-tools \
    && rm -rf /var/lib/apt/lists/*

# Instalamos OPAM usando el script proporcionado
RUN echo "/usr/local/bin" | bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"


# Inicializamos OPAM y configuramos el entorno
RUN opam init -y --disable-sandboxing
RUN eval $(opam env)

# Instalamos las dependencias de OCaml
RUN opam install -y eliom Js_of_ocaml ocsipersist-pgsql cstruct

# Configuramos el servidor SSH
RUN mkdir /var/run/sshd && \
    echo 'root:h42n42' | chpasswd && \
    sed -i 's/#PermitRootLogin prohibit-password/PermitRootLogin yes/' /etc/ssh/sshd_config

# Exponemos el puerto 8080 para el sitio web y el 22 para SSH
EXPOSE 8080 22

# Establecemos el directorio de trabajo
WORKDIR /app

# Copia el script al contenedor
COPY src/start.sh /start.sh

# Establece el script como el comando por defecto a ejecutar cuando se inicie el contenedor
#CMD ["/start.sh"]


CMD ["/usr/sbin/sshd", "-D"]