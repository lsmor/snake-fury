FROM gitpod/workspace-base

RUN sudo curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
RUN ghcup install --set ghc 8.10.7
RUN ghcup install hls
RUN ghcup install stack
RUN ghcup install cabal
