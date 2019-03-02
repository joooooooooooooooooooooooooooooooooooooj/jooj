#!/bin/sh

nix-shell --run "cabal --enable-nix v1-repl lib:jooj"
