# haskell-code-obfuscator

haskell-code-obfuscator is an utlity that makes haskell programs completely 
unreadable for humans.

## Usage

```
Usage: obfuscate-exe FILE [--seed INT] [--ghc-options GHC_OPTIONS | (-w|--working-dir WDIR)]

Available options:
   --seed INT                Seed for random name generation
   --ghc-options GHC_OPTIONS GHC options to compare
   -w, --working-dir WDIR    Path to a project directory
   -h, --help                Show this help text
```

## Examples

* Obfuscate a file: 

```
stack exec -- obfuscate-exe FILE
```

* Obfuscate a file with options:
```
stack exec -- obfuscate-exe --ghc-options="-package PACKAGE" FILE
```

* Obfuscate a file in a stack project:
```
stack exec -- obfuscate-exe -w PROJECT_DIR PATH
```

Note: need to have `hie.yaml` file in the main project directory.
