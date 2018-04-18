# Indexed List Literals

This is an incredibly simple library, which makes writing lists where the length is known at compile time a little bit nicer.

If you write a function with the signature
```haskell
vector :: ILL input length output => input -> Vector length output
```
then
```haskell
v :: Vector 3 Int
v = vector (1,2,3)

x :: Vector 0 Double
x = vector $ ZeroTuple @Double

y :: Vector 1 Double
y = vector (Only 1)

z :: Vector 2 String
z = vector ("Hello", "World")
```

If want matrix literals you can write a function
```haskell
matrix :: (ILL row width ty, ILL matrix height row) => matrix -> Matrix width height ty
```
then
```haskell
a :: Matrix 0 0 Bool
a = matrix $ ZeroTuple @(ZeroTuple Bool)

b :: Matrix 1 2 String
b = matrix $ Only ("Hello","World")

c :: Matrix 4 5 Double
c = matrix ((1,2,3,0,0)
           ,(4,5,6,0,0)
           ,(7,8,9,0,0)
           ,(0,0,0,0,0))
```
The full code is in test//Docs.hs

This only supports literals of length up to 20, though that can be easily extended using the code generator in src//Data//IndexedListLiterals.hs
