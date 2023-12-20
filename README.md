# GraBlink
Based on [BlinkFill](https://dl.acm.org/doi/10.14778/2977797.2977807), GraBlink implements 3 different synthesis methods for
comparative purposes.

### The DSL
This is extremely similar to the one from BlinkFill. The only modifications we made are to add arithmetic expressions.
```
e  := Concat(f1, f2, ...)
f  := Substr(p1, p2)
    | ConstStr(s)
    | NumToStr(b)
b  := ConstNum(n)
    | Strstr(p1, p2)
    | b + b
    | ...
p  := ConstPos(n)
    | (t, k, d)
d  := Start
    | End
```
All of these should be fairly straightforward, except for `(t, k, d)`. This position notation refers to the `k`th match of the regular expression `t`. `d` refers to whether the position represents the first character of that match, or the last. The full list of regular expressions that are considered by the synthesizer can be seen in `inputdatagraph.rs` in the `TOKENS` map.

### Program Specification
GraBlink operates over simple string operations (i.e. concatenation and substrings) and some trivial arithmetic operations. It will attempt to synthesize the smallest program in the dsl that takes a given input column to a given output column.  
An example of such a specification can be seen below:
```
First Name;FN
Last Name;LN
One Two;OT
Three Four;TF
Five Six;FS
```
This will yield a program similar to `Concat(SubStr(0,1),SubStr( (CAPS,2,Start), (CAPS,2,End) ))`.  

### Running it
In order to synthesize a program, all you have to do is pass in the specification as a command line argument, and the synthesis method (e.g. `./grablink --vsa input.txt`).

The different ways of running it are as follows:
| Flag | Description |
|------|-------------|
| `--enum` | Runs synthesis using basic enumeration of the program space, no optimizations |
| `--vsa` | Uses graph intersections to massively narrow down the program space before generating programs. Does not attempt arithmetic expressions.
| `--vsan` | The same as `--vsa`, but will attempt to synthesize arithmetic expressions using a recursive enumerator |
| `--egraph` | Uses enumeration with position equivalence reduction and optimizes using egraphs |
| `--time` | Runs all 3 synthesis methods and times them |
| `--timen` | Same as `--time`, but runs the VSA approach with the arithmetic enumerator |

### Tests
We tested all three synthesis methods on a variety of benchmarks that we yoinked from [Duet](https://github.com/wslee/duet)