# Indexed Container Literals

This is an incredibly simple library, which makes writing lists where the length is known at compile time a little bit nicer.

If you write a function with the signature
```haskell
vector :: ICL input length output => input -> Vector length output
```
then
```haskell
vector ZeroTuple @Long :: Vector 0 Long
vector (1,2,3) :: Vector 3 Int
vector (OneTuple 1) :: Vector 1 Double
vector ("Hello", "World") :: Vector 2 String
```
