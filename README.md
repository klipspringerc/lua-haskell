# lua-haskell-interpreter

A simple interactive lua interpreter implemented in Haskell

*Supported Features*
* Primitive types
* Variables
* Tables
* Control Structures
* Functions
* Metatables & Metamethods

*Limitations*
* Input in interactive shell does not support proper cursor movement codes.
  It is recommended to directly copy complete statement into the shell.
* Only single line statement input is supported. Use `do S;S1 end` to sequence multiple
  statements together.
* Float number literals / scientific notations are not supported.
* Combination of features could not work, e.g. non-tail `break` statement in sequenced statements inside loops.
* Lua implicit table key deduction for array-like syntax is not implemented.

## Run interpretor

```bash
stack build
stack exec main
```

Or in GHCi

```haskell
> :load Main
*Main> main
```

```lua
Lua> print("Hello World")
Lua> quit
```

## Basic operations

```lua
a = {}
a[20] = "great"
print(a[20])
k = 20
print(a[k])

do t = {[1] = 99, ["x"]="abc", [#"key"]=true}; print (t) end
```

## Sequence and Control Structure

```lua
do a=20;b=40;op="+" end

if a > b then print(a) else print(b) end

if op == "+" then return a + b elseif op == "-" then return a - b else return -1 end

for i=10,1,-3 do print(i) end

do v=0; for i=1,10 do v = v+i end; print(v) end

v = 1

while v do do v=v*2; if v > 5 then break end end end
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

## OOP Style

`Square` inherits methods from `Shape` while overridding the `name` method

```lua
Shape = {}
function Shape:family() return "Shape" end
function Shape:name() return "Shape" end
function Shape:new() do o={};o["__metatable"]=self; return o end end

Square = Shape:new()
function Square:area() return self.x * self.x end
function Square:name() return "Square" end

function Square:new() do o={};o["__metatable"]=self; return o end end
o = Square:new()
o["x"] = 16
print(o:area())
print(o:name())
print(o:family())
```

MetaMethod with operator `+` and `-`

```lua
Point = {}
function Point:square() return self.x^2 + self.y^2 end
function Point:add(p) do o={};o["__metatable"]=self["__metatable"];o["x"]=self.x+p.x;o["y"]=self.y+p.y;return o end end
function Point:sub(p) do o={};o["__metatable"]=self["__metatable"];o["x"]=self.x-p.x;o["y"]=self.y-p.y;return o end end
Point["__add"] = Point.add
Point["__sub"] = Point.sub
function Point:new(x,y) do o={};o["__metatable"]=self;o["x"]=x;o["y"]=y;return o end end

do a = Point:new(3,4); b = Point:new(1,1) end
c = a + b
c.x
c:square()
c = a - b
c.x
```

## Citations & References

UIUC CS421 MP4 & MP5 [github](https://github.com/uiuc-cs421/cs421-mp5)
- Code structure
- Parser usage
- Unit tests

[Programming in Lua](https://www.lua.org/pil/16.2.html)
- Lua features and code examples

Lua-Interpretor [github](https://github.com/luliu8/Lua-Interpreter-implemented-with-Haskell)
- Lua binary operators
- Unit tests