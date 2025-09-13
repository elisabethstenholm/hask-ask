# Hask ask

Hask ask is a small auction plattform written in Haskell.

## Running the program

The program is built with Cabal and packaged with Nix. You can either install
all the necessary dependencies to build the Cabal package, or you can just
build it with Nix.

In order to run the program simply do
```
$ nix run
```

In order to build the program, do
```
$ nix build
```
The binary is placed in ` ./result/bin/hask-ask`.

## Development

If you want to play around with the source code, open a nix dev shell
with all the required tools by running
```
$ nix develop
```