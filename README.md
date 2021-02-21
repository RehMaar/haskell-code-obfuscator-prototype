# haskell-code-obfuscator

haskell-code-obfuscator is an utlity that makes haskell programs completely 
unreadable for humans.

## Usage

```
Usage: obfuscate-exe FILE [--seed INT] [--ghc-options GHC_OPTIONS | (-w|--working-dir WDIR)]

Available options:
   --seed INT                Seed for random name generation
   --ghc-options GHC_OPTIONS GHC options to compile a module with
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

## Renaming 

1. Local symbols renaming
2. Imported symbols renaming
3. Not imported record selectors renaming

## Supported structural transformations

1. Strings and chars are transformated into function applications.
   ```
   -- Source
   testString = "Hello!"
   -- Transformed
   testString  = map toEnum [72, 101, 108, 108, 111, 33]
   ```
   Note: now we don't check if `Prelude.map` is hidden and redefined.
2. Basic do-notation into lambda form.
  ```
  TODO
  ```
3. Add parens: ``1 + 2`` into `(1 + 2)`.
4. Transform operator into applications: `1 + 2` into `(+) 1 2`.
5. Transform `if` into `case`: `if True then 1 else 2` into `case True of { True -> 1; False -> 2}`.
6. Transform a lambda with multiple arguments into lambdas with one argument: `\a b c ->` into `\a -> \b -> \c -> `.

## TODO: Transformations

* Inlining.
* Number literals into hex.
* Guards into case. Note: symantic problems with this transformation.
* Function arguments to lambda. Note: only when `guards into case` enabled. 
* Function arguments to lambda. Or when there are no guards.
* Insert randomly generated definitions for distraction
* Eta-expansion (`f` into `\x -> f x`).
* Nice place for ideas: https://tigress.wtf/transformations.html

## TODO: De-obfuscation

## TODO: User-friendly things

1. Add options to disable some transformations
2. Tests
3. Better projects support
4. ...
