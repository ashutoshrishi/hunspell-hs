# hunspell-hs

[![Build Status](https://travis-ci.com/ashutoshrishi/hunspell-hs.svg?branch=master)](https://travis-ci.com/ashutoshrishi/hunspell-hs)

Haskell bindings for Hunspell with thread-safety in mind. The goal was
to be able to use hunspell as an API in other project.

Takes inspiration from the Golang bindings: https://github.com/akhenakh/hunspellgo

# Usage

This package needs to find the `libhunspell` shared library on your system.

On Linux (use your package manager to install `libhunspell-dev`):

    sudo apt install libhunspell-dev

On Macos:

    brew install hunspell
