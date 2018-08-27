# Property based testing

## {
  data-background-image="images/hedgehog.png"
  data-background-size="80%"
}

::: notes
- Might have heard of QuickCheck
- New kid on the block
- Jacob Stanley released it in 2017
- Jacob gave a great talk on Hedgehog and why he wrote it at Lambda Jam in 2017 (link in the references)
:::

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

## Shrinking

##

```
[ 'w','K','E','y','P','z','m','o','i','N','G','I','X','X'
, 'v','l','q','X','n'
]
```

##

```haskell
['a','b']
```



