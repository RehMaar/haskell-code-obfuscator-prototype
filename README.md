# haskell-code-obfuscator

haskell-code-obfuscator is an utlity that makes haskell programs completely 
unreadable for humans.

TODO:
- [x] Obfuscate identifiers.
- [*] Obfuscate code structure
- [ ] Good tests.
- [ ] Good project description.
- [ ] Support GHC options.
- [ ] Support stack and cabal projects.
  * [x] Support extraction of module dependencies from project file.
        * Note: need `hie.yaml` file (from Hie-Bios project)
  * [ ] Obfuscate the whole project.


Obfuscation types:
- [x] Do-notation -> lambdas
- [x] Operators -> application.
- [x] Lambda with several arguments -> lambdas with one arguments.
- [x] If-notation -> case-notation
- [x] Add parens.
- [x] Strings and chars -> mess of code.
- [ ] more?
