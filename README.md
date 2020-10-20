# haskell-code-obfuscator

haskell-code-obfuscator is an utlity that makes haskell programs completely 
unreadable for humans.

TODO:
- [x] Obfuscate identifiers.
- [x] Obfuscate code structure
   - [x] Support basic expressions and declarations.
   - [ ] Support more expressions and declarations.
- [ ] Use exactPrint for printings in one-line
   - [ ] Change annotations when changing AST.
   - [ ] Change annotations (and AST?) for one-line printings.
- [ ] Good tests.
- [ ] Good project description.
- [ ] Support GHC options as command line options.
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
