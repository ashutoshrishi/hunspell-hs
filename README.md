# hunspell-hs

[![Haskell Stack Project CI](https://github.com/ashutoshrishi/hunspell-hs/actions/workflows/ci.yaml/badge.svg)](https://github.com/ashutoshrishi/hunspell-hs/actions/workflows/ci.yaml)

Haskell bindings for Hunspell with thread-safety in mind. The goal was
to be able to serve hunspell as an API in a webserver.

Takes inspiration from the Golang bindings:
https://github.com/akhenakh/hunspellgo

Please refer to the module description for `Language.Hunspell` on
Hackage for more information.

# Installation

This package needs to find the `libhunspell` shared library on your
system.

On Linux (use your package manager to install `libhunspell-dev`):

    sudo apt install libhunspell-dev

On Macos:

    brew install hunspell
