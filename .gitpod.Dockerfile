FROM gitpod/workspace-base

# Install dependencies
RUN sudo apt-get install -y build-essential curl libffi-dev libffi7 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5

# ghcup is a replacement for the haskell platform. It manages the development env easily. 
# We use the official instalation script
RUN sudo curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# Add ghcup to path and add the folder stack installs the software
ENV PATH=${PATH}:${HOME}/.ghcup/bin

# Set up the environment. 
RUN ghcup install ghc 8.10.7
RUN ghcup set ghc 8.10.7
RUN ghcup install hls
RUN ghcup install stack
RUN ghcup install cabal

# change stack configuration to use system installed ghc.
# By default, stack tool will download its own version of the compiler,
# Setting up this configuration will avoid downloading haskell compiler twice.
RUN stack config set install-ghc --global false
RUN stack config set system-ghc --global true 
