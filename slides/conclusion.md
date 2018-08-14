# Conclusion

## State machine testing

Very powerful technique to test stateful systems

## `NixOps`

- Referentially transparent specification of a deployment
- Deploy in a few commands

## `DMap`

- Heterogenous mapping of keys to values that preserves type safety
- Good for more "dynamic" data formats
- Instances for GADTs can get a bit tricky and repetitive
- Template haskell can remove some of the boilerplate

## `servant`

- `servant` allows us to define part of an API and get client functions for free
- `servant`'s design allows us to extend it as necessary

## Final thought

If you can't write your app in Haskell, at least test it in Haskell.

