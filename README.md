# haskell-code-obfuscator

haskell-code-obfuscator is an utlity that makes haskell programs completely 
unreadable for humans.

## Usage

```
Usage: obfuscate-exe FILE [--seed INT]
                     [--ghc-options GHC_OPTIONS | (-w|--working-dir WDIR)]
                     [--all | [--rename | --rename-all] [--no-basic] [--strings]]
                                            
Available options:
  --seed INT                Seed for random generation
  --ghc-options GHC_OPTIONS GHC options to compile the given module
  -w,--working-dir WDIR     Path to a project directory
  --all                     Apply all transformations
  --rename                  Rename local variables
  --rename-all              Rename local variables and imported symbols
  --no-basic                Do not apply basic structural transformations
  --strings                 Apply string transformation
  -h,--help                 Show this help text
```

* Obfuscate a file: 

```
stack exec -- obfuscate-exe FILE --all
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

## Examples

Original:
```haskell
module Main where

testIf1 p x = if p then x else 10

main = do
  putStrLn $ show $ testIf1 True 1
  putStrLn $ show $ testIf1 False undefined
```

Result:
```haskell
module Main where {testIf1 stv xumiZi = case stv of { True -> xumiZi; False -> 10 }; 
main = (((>>=) ((((($) (putStrLn)) (((($) (show)) (testIf1 True 1))))))) 
((\ (_) -> ((($) (putStrLn)) (((($) (show)) (testIf1 False undefined)))))));}
```

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
   Note: maybe there are better ways to obfuscate strings.
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

## TODO: Transform programs to bottles:

It's possible to transform haskell programs to pictures like that. [Like in perl](https://metacpan.org/dist/Acme-EyeDrops/view/lib/Acme/EyeDrops.pm).

Original:
```haskell
module Main where

testIf1 p x = if p then x else 10

main = do
  putStrLn $ show $ testIf1 True 1
  putStrLn $ show $ testIf1 False undefined
```

Result:
```haskell
module
Main ( 
main ) 
where 
{ 
    zJSj
    gwww
    wwww
    = if 
    gwww
    then 
    wwww 
    else 
  10; main 
 = do { aaa
$ show $ zJSj
True 1; aaa $
show $ zJSj f
undefined };f
= False;  aaa
= putStrLn; x
= (); y = ();
xxxxxxxxxxx =
(); xxxxxxxxx
= xxxxxxxxxxx
; xxxxxxx = 1
; q = xxxxxxx
; qwe = 123;}
```

## TODO: De-obfuscation

## TODO: User-friendly things

0. Transform expressions without modules
1. Add options to disable some transformations
2. Tests
3. Better projects support
4. ...
