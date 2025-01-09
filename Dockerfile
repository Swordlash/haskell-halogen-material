# Use an official Ubuntu base image
FROM ubuntu:22.04

# Set environment variables to prevent interactive prompts
ENV DEBIAN_FRONTEND=noninteractive

# Update and install required packages
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

# Install the latest version of Emscripten
RUN curl -L https://emscripten.org/emsdk.zip -o emsdk.zip && \
    unzip emsdk.zip && \
    rm emsdk.zip && \
    cd emsdk && \
    ./emsdk install latest && \
    ./emsdk activate latest && \
    echo "source /emsdk/emsdk_env.sh" >> /root/.bashrc

# Add Emscripten to PATH
ENV PATH="/emsdk/upstream/emscripten:/emsdk:/emsdk/upstream/bin:$PATH"

# Install GHCup
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | bash -s -- -y

# Set GHCup binary path
ENV PATH="/root/.ghcup/bin:$PATH"

# Install and configure GHCJS
RUN source /root/.bashrc && \
    emconfigure ghcup install ghc --set javascript-unknown-ghcjs-ghc-9.12.1

# Verify installations
RUN emcc --version && \
    ghcup --version && \
    ghc --version