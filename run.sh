#!/bin/bash -efu

stack exec obfuscate-exe -- "$1" | stack runhaskell
