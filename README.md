# lua-haskell-interpreter

## Run interpretor

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

do a=20;b=40;return a*b end
```

## Functions

```lua
function tdouble(v) return 2*v end
print(tdouble(13))

function tmulti(a,b) return a*b end
print(tmulti(13,14))

function tcircum(r) do pi=314;return 2*pi*r/100 end end
print(tcircum(10))
```

## TODO: Citations