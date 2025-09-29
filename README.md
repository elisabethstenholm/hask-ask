# Hask ask

Hask ask is a small auction plattform written in Haskell.

The program is built with Cabal and packaged with Nix. You can either install
all the necessary dependencies to build the Cabal package, or you can just
build it with Nix. Building/developing the package with Nix installs all
necessary dependencies in a location that will not pollute your global
installations.

A simple way to install Nix is to use the [Determinate Systems
installer](https://determinate.systems/nix-installer/). (It is easy to uninstall
it with the Determinate installer, if you decide you don't like it.)

## Building and running the program

If you want to play around with the source code, open a nix dev shell
with all the required tools by running
```
$ nix develop
```

From there, you can for instance launch VSCode
```
$ code .
```
If you have the [Haskell
extension](https://marketplace.visualstudio.com/items?itemName=haskell.haskell)
installed, make sure that the settings specify that it should look for ghc in
the path. Then the Haskell language server should work.

The program can then be built (from inside the nix shell) with
```
$ cabal run
```

This will start a server that serves the webpage from `localhost:8080`.

Or you can build the program with
```
$ cabal build
```