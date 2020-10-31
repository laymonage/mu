# mu

Simple untyped (and soon simply typed) lambda calculus interpreter written in Haskell.

Features:
* Bound and free variables
* Abstractions (functions)
* Function application
* Aliases to name expressions so they don't have to be typed out over and over again
* "Evaluation" through alpha conversion and beta reduction

## Table of Contents

- [Table of Contents](#table-of-contents)
- [Grammar](#grammar)
- [Predefined aliases](#predefined-aliases)
- [Examples](#examples)
- [Building & Running](#building--running)
- [License](#license)

## Grammar

```
input       ::= expr (";" expr)*
expr        ::= alias | application
alias       ::= ALIASIDENT ":=" application
application ::= (term)* | "(" application ")"
term        ::= variable | abstraction
variable    ::= VARIDENT | ALIASIDENT
abstraction ::= ("\" | "λ") VARIDENT "." application

VARIDENT    ::= _any single lowercase letter_
ALIASIDENT  ::= _any series of alpha numeric characters_ or + or *
```

Whitespace after any token is ignored.
Comments can be started with `--`.

## Predefined aliases

A set of predefined aliases is available in the environment, which includes the following aliases.

```
S     := λw.λy.λx.(y (w y x))
+     := S
*     := λx.λy.λz.(x (y z))
0     := λs.λz.z
1     := S 0
2     := S 1
3     := S 2
4     := S 3
5     := S 4
6     := S 5
7     := S 6
8     := S 7
9     := S 8
T     := λx.λy.x
F     := λx.λy.y
True  := T
False := F
not   := λp.p F T
and   := λp.λq.p q F
or    := λp.λq.p T q
Z     := λx.x F not F
```

## Examples

```
> ID := \x.x -- The identity function.
\x.x
> ID a
a
> AND := \p.\q.p q p -- Boolean and.
\p.\q.p q p
> TRUE := \x.\y.x -- Boolean true.
\x.\y.x
> FALSE := \x.\y.y -- Boolean false.
\x.\y.y
> AND TRUE FALSE ; AND TRUE TRUE
\x.\y.y ; \x.\y.x
```

## Building & Running

The project can be easily built using Cabal (install via `ghcup` on Linux) or Stack:
```
$ cabal build
```
and can also be run using
```
$ cabal run
```

This will launch a REPL in which Lambda Calculus expressions can be typed and evaluated.

The program REPL can be installed by using
```
$ cabal install
```
This way it can be executed from anywhere by just invoking the `mu` command.

## License

Licensed under the [MIT License](./LICENSE).
