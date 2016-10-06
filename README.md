
Tutorial for libroman
=====================

**libroman** lets you use roman numerals in Haskell. They support basic math and
conversion to and from strings.

To start using **libroman** you have to import the Data.Roman module

```haskell
 module MyModule where
 import Data.Roman
```


Type Class Roman
----------------

The Module `Data.Roman` exports the type class **`Roman`**. Which has a single
method `fromRoman` to convert from a `Roman` value to any other `Integral` value.

There are two data types that implement this type class:

- `RomanSymbol`: represents the symbols that make up roman numerals
- `RomanNumeral`: represents the combination of these symbols.

The Data Type `RomanSymbol` is a simple ADT that has the following Constructors:
`Nulla | I | V | X | L | C | D | M` where `Nulla` represents zero.

`RomanNumeral` is a type synonym for lists of `RomanSymbol`s ( as in
`[RomanSymbol]` ). This data type can be used to represent any combination
of RomanSymbol ( even if it does not follow the classical rules ), and by
extension the numeric values they represent.

For the rest of this tutorial we will be working with `RomanNumeral`


Data Type RomanNumeral
----------------------

Besides the aforementioned instance for `Roman`, `RomanNumeral` is also a
member of a whole host of other type classes.

- `Eq`
- `Ord`
- `Enum`
- `Show`
- `Read`
- `Num`
- `Real`
- `Integral`

Because a numeric literal (eg. `12`) can be thought of as syntactic sugar for `fromInteger n` the
easiest way to declare a RomanNumeral value is like this:

``` haskell
  a = 12 :: RomanNumeral

  b :: RomanNumeral
  b = 273
```
Or in ghci like this:

```haskell
λ>  let myRoman = 45 :: RomanNumeral
λ>  myRoman
    XLV
λ>  234 :: RomanNumeral
    CCXXXIV
λ>  [X,I,V]
    XIV
```

you want the number 4 represented as IIII, you could just do this.

``` haskell
λ> let c = [I, I, I, I]
```

## Converting to Strings

`RomanNumeral` implements `Read` and `Show` to convert to and from `String`

`read` is case insensitive.

```haskell
  l = show k          -- "XII"
  m = read "XXXII"    :: RomanNumeral
  n = read "nulla"    :: RomanNumeral
```


Mathy Stuff with RomanNumeral
-----------------------------

Once you have some Values you can get to work. The type class `Num` enables you
to do basic math:

``` haskell
  d = a + b       -- CCLXXXV
  e = b - c       -- CCLXVI
  f = e * d
```

Check the result of f by yourself: `ghci> f`

You will see that roman numerals are not really suited for big numbers.
Try to keep your math under 10'000 or prepare for lots or `M`s

Roman numerals can't represent fractional values so you can't use `(\)`, but
you can use the methods of the `Integral` type class to do Integer division.

``` haskell
  g = div f d     -- CCLXV
```

Just as with any other enumerable value, you can declare ranges:

``` haskell
  r  :: [RomanNumeral]
  r   = [1..12]
  r'  = [a..b]
  r'' = [[C, X, I]..[C, C]]
```

**Beware:** The Romans didn't have a concept of negative numbers, so
`RomanNumeral` can't represent negative numbers. This means that the
function `negate` will cause an *Underflow Exception*. Same if you
subtract a larger RomanNumeral from a smaller one, or perform any other
operation that would create a negative value.

*I don't know how to solve this problem ( apart from dependent typing :P ),
as simply throwing an exception and crashing everything ruins part of the
type-checked goodness we have in haskell, but returning the wrong result
because of this seems even worse somehow.*


``` haskell
  baloney  = negate e          -- This throws an error if evaluated!

```

Try to guess which of the next two thows an Exception, and which one doesn't

``` haskell
  hogwash  = a - b + b
  salami   = a + b - b
```

*In a nutshell: Mathematically you can use `RomanNumeral` in basically the
same way as any other unsigned integral value (`Word` for instance), but 
instead of wrapping you get an Exception*


Comparison of RomanNumerals
---------------------------

And of course you can compare two `RomanNumeral`s with
`(>=)`, `(>)`, `(==)`, `(<)` and `(<=)`

**Beware:** `(==)` has a little quirk stemming from the fact that `RomanNumeral`
enables you to represent the same numerical value in multiple ways.
`(==)` will only return
`True` if the representation is equal, not just the numerical value.

``` haskell
λ>  let rep1 = [I, I, I, I]		-- clock style
λ>  let rep2 = [I, V]             -- normal style
λ>  let rep3 :: RomanNumeral
λ>  let rep3 = 4

λ>  rep1 == rep2 
    False
λ>  rep2 == rep3   
    True
```


Numerical Type Converting
-------------------------

`Integral` also has a method `toInteger` which can be used to coerce your Roman
values to `Integer`s. However the function `fromIntegral` from the Prelude is
a lot more general, as you can convert from any `Integral` value to any other
`Integral` value, so you can use it to both convert to and from RomanNumeral.

``` haskell
λ>  let thisIsAnInteger =  12   -- Numeric literals default to Integer
λ>  let thisIsARoman    = (44   :: RomanNumeral)
λ>  toInteger thisIsARoman
	44
λ>  fromIntegral thisIsARoman :: Word -- You can convert to any Integral type
	44
λ>  fromIntegral thisIsAnInteger
	12
λ>  fromIntegral thisIsAnInteger :: RomanNumeral
	XII
```

libroman is pretty good at interpreting the 'correct' numeric value for
`RomanNumeral`s which don't follow the classic rules of writing roman
numerals, for instance double subtraction as in XIIX, or repeating the same character more than 3 times as in IIII.

``` haskell
λ>  let weird = [X, I, I, X]
λ>  let clock = [I, I, I, I]
λ>	fromIntegral weird
	18
λ>	fromIntegral clock
	4
```

