# Property based testing

##

Apply functions to randomly generated inputs and ensuring that desirable properties hold.

## Example

##

```haskell
-- Reverse is involutive
propReverse :: Property
propReverse =
  property $ do



```

##

```haskell
-- Reverse is involutive
propReverse :: Property
propReverse =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha


```

##

```haskell
-- Reverse is involutive
propReverse :: Property
propReverse =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    reverse (reverse xs) === xs
```

## Libraries

- QuickCheck
- Hedgehog

::: notes
QuickCheck  
 - Koen Claessen and John Hughes released QuickCheck in 1999
 - Household name for property based testing

Hedgehog  
 - New kid on the block
 - Jacob Stanley released it in 2017
 - Jacob gave a great talk on Hedgehog and why he wrote it at Lambda Jam in 2017 (link in the references)
 - Short version: integrated shrinking, and no type classes
:::

