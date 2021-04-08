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
