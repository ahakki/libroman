<!--
Haskell Files should declare a module

>   module Data.Roman.Tutorial where
>   import Data.Roman

-->

Tutorial for libroman
=====================

This Tutorial is a literate Haskell file so if you want to play along with the
Tutorial while we do this you can simply boot up ghci and load this module with
`ghci> :l Data.Roman.Tutorial`


Type Class Roman
----------------

The Module `Data.Roman` exports the type class **Roman**. Which has a single
method `fromRoman` to convert from a `Roman` value to any `Integral` value.

There are two data types that implement this type class: `RomanSymbol` for
single roman numerals (as in the symbols), and `RomanNumeral` which is used to
represent the combination of these symbols.

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

`RomanNumeral` is a

Because a numeric literal `a` is syntactic sugar for `fromInteger a` the
easiest way to declare a RomanNumeral value is like this:

>   a = 12 :: RomanNumeral
>
>   b :: RomanNumeral
>   b = 273

You can also create RomanNumerals manually using List Syntax. If, for instance,
you want the number 4 represented as IIII, you could just do this.

>   c = [I, I, I, I]


Mathy Stuff with RomanNumeral
-----------------------------

Once you have some Values you can get to work. The type class `Num` enables you
to do basic math:

>   d = a + b       -- CCLXXXV
>   e = b - c       -- CCLXVI
>   f = e * d

Check the result of f by yourself: `ghci> f`

You will see that roman numerals are not really suited for big numbers.
Try to keep your math under 10'000 or prepare for lots or `M`s

Roman numerals can't represent fractional values so you can't use `(\)`, but
you can use the methods of the `Integral` type class to do Integer division
and so on.

>   g = div f d     -- CCLXV

Just as with any other enumerable value, you can declare ranges:

>   r  :: [RomanNumeral]
>   r   = [1..12]
>   r'  = [a..b]
>   r'' = [[C, X, I]..[C, C]]

And of course you can compare two `RomanNumeral`s with
`(>=)`, `(>)`, `(==)`, `(<)` and `(<=)`

**Beware:** The Romans didn't have a concept of negative numbers, so
`RomanNumeral` can't represent negative numbers. This means that the
function `negate` will cause an *Underflow Exception*. Same if you
subtract a larger RomanNumeral from a smaller one, or perform any other
operation that would create a negative value.

*I don't know how to solve this problem ( apart from dependent typing :P ),
as simply throwing an exception and crashing everything ruins part of the
type-checked goodness we have in haskell, but returning the wrong result
because of this seems even worse somehow.*


>   baloney  = negate e          -- This throws an error if evaluated!
>
>-- Try to guess which of the next two
>-- thows an Exception, and which one doesn't
>
>   hogwash  = a - b + b
>   salami   = a + b - b

*In a nutshell: Mathematically you can use `RomanNumeral` in basically the
same way as any other unsigned integral value (`Word` for instance)*


Numerical Type Converting
-------------------------

`Integral` also has a method `toInteger` which can be used to coerce your Roman
values to `Integer`s. However the function `fromIntegral` from the Prelude is
a lot more general, as you can convert from any `Integral` value to any other
`Integral` value, so you can use it to both convert to and from RomanNumeral.

>   thisIsAnInteger = 12    -- Numeric literals default to Integer
>   thisIsARoman    = 44    :: RomanNumeral
>
>   h = toInteger thisIsARoman
>
>   i = fromIntegral thisIsARoman
>
>   j = fromIntegral thisIsAnInteger
>
>   k :: RomanNumeral
>   k = fromIntegral thisIsAnInteger

libroman is pretty good at interpreting the 'correct' numeric value for
`RomanNumeral`s which don't follow the classic rules of writing roman
numerals, for instance double subtraction as in XIIX, or repeating the same character more than 3 times as in IIII.

>   weird = [X, I, I, X]
>   clock = [I, I, I, I]

If you check these values in ghci ( using `fromIntegral` ) you will see that
they are interpreted as `18` and `4` which is the most sensible interpretation
(imho)

You can probably guess the types of `h`, `i` and `j`, but if you are not sure
you can check in ghci with `:t`


Converting to Strings
---------------------

`RomanNumeral` implements `Read` and `Show` to convert to and from `String`

`read` is case insensitive.

>   l = show k          -- "XII"
>   m = read "XXXII"    :: RomanNumeral
>   n = read "nulla"    :: RomanNumeral
