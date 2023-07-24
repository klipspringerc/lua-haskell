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
```

## Functions

```lua
function tdouble(v) return 2*v end
print(tdouble(13))

function tmulti(a,b) return a*b end
print(tmulti(13,14))
```

## TODO: Citations