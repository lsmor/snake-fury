FROM gitpod/workspace-base

# Use ghc version 9.2.5
ENV GHC_VERSION=9.2.5

# Env variables:
#   - Adds ghcup binary folde to path
#   - set ghcup instalation script to non-interactive
#   - set ghcup instalation script to minimal instalation. That is: just install ghcup and nothing else
ENV PATH=${PATH}:${HOME}/.ghcup/bin
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=true
ENV BOOTSTRAP_HASKELL_MINIMAL=true

# Install dependencies
RUN sudo apt-get install -y build-essential curl libffi-dev libffi7 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5

# ghcup is a replacement for the haskell platform. It manages the development env easily. 
# We use the official instalation script
RUN sudo curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# Set up the environment. 
RUN ghcup install ghc ${GHC_VERSION} --set
RUN ghcup install hls
RUN ghcup install stack
RUN ghcup install cabal

# change stack configuration to use system installed ghc.
# By default, stack tool will download its own version of the compiler,
# Setting up this configuration will avoid downloading haskell compiler twice.
RUN stack config set install-ghc --global false
RUN stack config set system-ghc --global true 

# Finally, set the yaml file to the desired GHC_VERSION. This should be done
# after stack config set ..., otherwise stack fails because it interprets that stack-ghc-xxx.yaml
# should be the global configuration file, instead of project specific.
ENV STACK_YAML=stack-ghc-${GHC_VERSION}.yaml