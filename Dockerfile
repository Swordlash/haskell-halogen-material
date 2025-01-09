FROM ubuntu:22.04
ENV DEBIAN_FRONTEND=noninteractive
SHELL ["/bin/bash", "-c"]

RUN apt-get update && apt-get install -y \
    build-essential \
    curl \
    libffi-dev \
    libffi8 \
    libgmp-dev \
    libgmp10 \
    libncurses-dev \
    libncurses5 \
    libtinfo5 \
    python3 \
    cmake \
    git \
    unzip \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

RUN git clone https://github.com/emscripten-core/emsdk.git && \
    cd emsdk && \
    ./emsdk install latest && \
    ./emsdk activate latest && \
    source "/emsdk/emsdk_env.sh"

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | bash -s -- -y

ENV PATH="/root/.ghcup/bin:$PATH"

RUN emconfigure ghcup install ghc --set javascript-unknown-ghcjs-ghc-9.12.1

RUN emcc --version && \
    ghcup --version && \
    ghc --version