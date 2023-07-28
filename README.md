# lua-haskell-interpreter

## Run interpretor

*Limitations*
* Input in interactive shell does not support proper cursor movement codes.
  It is recommended to directly copy complete statement into the shell.
* Only single line statement input is supported. Use `do S;S1 end` to sequence multiple
  statements together.
* Non-integer number literals / scientific notations are not supported.

```bash
stack ghci
```

```haskell
> :load Main
*Main> main
Lua> print("Hello World")
```

## Basic operations

```lua
a = {}
a[20] = "great"
print(a[20])
k = 20
print(a[k])
```

## Sequence and Control Structure

```lua
do a=20;b=40;op="+" end

if a > b then v=a else v=b end

if op == "+" then r = a + b elseif op == "-" then r = a - b else r = -1 end

for i=1,10 do print(i) end

for i=10,1,-3 do print(i) end

do v=0; for i=1,10,1 do v = v+i end; print(v) end
```

## Functions

```lua
function tdouble(v) return 2*v end

print(tdouble(13))

function tmulti(a,b) return a*b end

print(tmulti(13,14))

function tcircum(r) do pi=314;return 2*pi*r/100 end end

print(tcircum(100))
```

## OOP Style with MetaTable

```lua

Square = {}
function Square:area() return self.x * self.x end
t = {}
t["x"] = 10
t["__metatable"] = Square
print(t:area())

function Square:new() do o={};o["__metatable"]=self; return o end end
o = Square:new()
o["x"] = 12
print(o:area())
```

## TODO: Citations & Credits
