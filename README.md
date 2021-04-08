# Description
J'arrange is a very simple language that allows you to rearrange JSON data very
easily.
A program consists of a list of pattern matched rewrite rules which are then
applied to all JSON data piped into stdin.
# Example
Given the following rules
``` json
{"hello" : "world"} -> "Hello, World!"
[x, x] -> x
{"a" : a, "b" : b} -> [a, b]
x -> {"obj" : x}
```
and the following input
``` json
3.141
{"hello" : "world"}
[10, 10]
[10, 20]
{"b" : false, "a" : true}
```
we get the following output.
``` json
{"obj" : 3.141}
"Hello, world!"
10
{"obj" : [10, 20]}
[true, false]
```
# Install
## From Source
+ Install [cabal](https://www.haskell.org/cabal/) if you haven't already.
+ Clone this repository.
+ Run `cabal install`.
+ As long as the cabal bin directory (usually `~/.cabal/bin`) is on your path you can test the install by running <br>
`jarrange test.jr < test.json`.

# TODO
+ JSON pretty printer.
+ Clean up code and make it literate.
+ Add matching on parts of strings and assembling strings
