# `DMap`

## API

##

```json
{
  "date": "1900-01-01T12:00:00",
  "date_gmt": "1900-01-01T12:00:00",
  "modified": "1900-01-01T12:00:00",
  "modified_gmt": "1900-01-01T12:00:00",
  "password": "",
  "slug": "a",
  "status": "publish",
  "type": "post",
  "link": "http://192.168.56.101/1900/01/01/a/",
  "title": "foo",
  "content": "bar",
  "excerpt": "baz",
  "author": 1,
  "featured_media": 0,
  "comment_status": "open",
  "ping_status": "open",
  "sticky": false,
  "template": "",
  "format": "standard",
  "meta": [],
  "categories": [],
  "tags": []
}
```

##

```json
{
  "title": "A post"
}
```

## Maybe baby

```haskell
data Post =
  Post
  { _postDate :: Maybe LocalTime
  , _postDateGmt :: Maybe LocalTime
  , _postPassword :: Maybe Text
  -- ...
  }
```

::: notes
- First thought might be to use `Maybe` everywhere
- This is clunky and doesn't feel like a good fit given we have to deal with all fields all the time.
:::

## A type for each occassion?

::: notes
- Could have multiple types depending on which fields are used
- Good if only a few combinations
- WordPress has a huge number of possible combinations and a poorly specified API
:::

## `DMap`

::: notes
- type safe heterogenous map --- value type depends on key
- colleagues had some success using `DMap` from the `dependent-map` package to wrangle JSON objects
- decided to have a go, and also had success
:::

##

```haskell
aPost :: DMap PostKey Identity
aPost =
  fromList [ PostTitle  :=> Identity "Hello Compose"
           , PostStatus :=> Identity Publish
           , PostAuthor :=> Identity 1
           ]
```

##

```haskell
aPost :: Applicative f => DMap PostKey f
aPost =
  fromList [ PostTitle ==> "Hello Compose"
           , PostStatus ==> Publish
           , PostAuthor ==> 1
           ]
```

##

```haskell
data DMap (key :: v -> *) (f :: k -> *)
```

::: notes
- `key` is a type constructor for our keys
- `f` is a type constructor applied to each value type
:::

##

```haskell
data PostKey a where
  PostTitle  :: PostKey Text
  PostStatus :: PostKey Status
  PostAuthor :: PostKey Author
  -- ...
  -- more constructors
```

##

```haskell
class Eq a => Ord a where
  compare :: a -> a -> Ordering
  ...

```

```haskell
class GEq f => GCompare (key :: v -> *) where
  gcompare :: key a -> key b -> GOrdering a b 
```

::: notes
- Need a way to compare keys to be used as a map
- `Ord` doesn't type check
- `GCompare` is the answer
:::

##

```haskell
deriveGEq ''PostKey
deriveGCompare ''PostKey
 
```

##

```haskell
deriveGEq ''PostKey
deriveGCompare ''PostKey
deriveGShow ''PostKey
```

::: notes
Same story with showing keys
:::

## What about values?

::: notes
- So far only have classes for keys
- value types are dependent on key types
- classes relating to values parameterised over keys to determine value type
:::

##

```haskell
class GShow tag => ShowTag (key :: k -> *) (f :: k -> *) where
  showTaggedPrec ::
    forall (v :: k). key v -> Int -> f v -> ShowS
```

::: notes
This says: given a key, which determines the type of the value `v`, return the showsPrec function for `f v`.

This is a common pattern for these instances of dependently typed GADTs.
:::

##

```haskell
instance ShowTag PostKey f where
  showTaggedPrec _ = showsPrec1
```

::: notes
It'd be really nice if we could write this. Unfortunately, `showsPrec1` has a `Show` constraint on
the value type --- the `a` in the `f a` we're showing. That constraint can't be satisfied because
GHC doesn't know what the value type is without knowing the constructor that's in play. GHC
won't inspect our GADT to look at all of the possible inhabitants of our value type and check the
constraint holds either.
:::

##

```haskell
instance ShowTag PostKey f where
  showTaggedPrec PostTitle = showsPrec1
  showTaggedPrec PostId = showsPrec1
  showTaggedPrec PostAuthor = showsPrec1
  -- ...
  -- regex replace your way to victory
```

##

```haskell
deriveShowTag :: Name -> DecsQ
deriveShowTag n = do
  keyType <- reify n
  let
    mkEq conName = clause [conP conName []] (normalB (varE 'showsPrec1)) []
    mkDecl = \case
      (GadtC [conName] _bangTypes _ty) -> mkEq conName
      _ -> fail "Can only deriveFromJSONViaKey with GADT constructors"
    decl = case keyType of
      TyConI (DataD _ctx _n _tyvars _kind cons _deriving) ->
        funD 'showTaggedPrec $ fmap mkDecl cons
      _ -> fail "Can only deriveFromJSONViaKey with a type constructor"
  f' <- varT <$> newName "f"
  let c = cxt [appT (conT ''Show1) f']
  pure <$> instanceD c (foldl appT (conT ''ShowTag) [conT n, f']) [decl]
```
