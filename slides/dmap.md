# `DMap`

##

::: {style="top: -20px; position: relative;"}
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
:::

##

```json
{
  "title": "A post"
}
```

::: notes
- Huge variation in how post can be specified
- Almost every field optional
:::

## `Maybe`?

##

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
- First thought might be to use `Maybe` everywhere.
- Doesn't scale.
- Must specify every field when we might only care about 1.
:::

## A type for each use?

::: notes
- Sometimes a good solution to these problems is to have a type for each scenario.
- Way too many combinations.
- Not sure what we'll get back from API early in process.
- Might be appropriate if small number of uses.
:::

## `DMap`


::: notes
- type safe heterogenous map --- type of each value may vary, and depends on the key
- colleagues had some success using `DMap` from the `dependent-map` package to wrangle JSON objects
- decided to have a go, and also had success
:::

##

```haskell
data Map k v
```

::: notes
key and value types same for every element
:::

##

```haskell
data DMap (key :: v -> *) (f :: k -> *)
```

::: notes
- `key` type takes a parameter that determines the type of its corresponding value
- `f` is a type constructor that wraps each value
:::

##

```haskell
data PostKey a where
  PostTitle  :: PostKey Text
  PostStatus :: PostKey Status
  PostAuthor :: PostKey Int
  ...
```

::: notes
- Using `GADT` syntax
- `PostKey` is the type.
- `a` is a type level variable that tracks the type of the value corresponding to each key.
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

::: notes
- sits between a record and `Data.Map`
- type of values changes with the key, like a record
- may have 0 or more key,value pairs, like a map
:::

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
class Eq a => Ord a where
  compare :: a -> a -> Ordering
  ...

```

::: notes
- Need a way to compare keys to be used as a map
:::

##

```haskell
instance Ord (PostKey a) where
  compare :: PostKey a -> PostKey a -> Ordering
  ...

```

::: notes
- `Ord` instance would mean we can only compare keys with the same value type.
:::

##

```haskell
class GEq f => GCompare (key :: v -> *) where
  gcompare :: key a -> key b -> GOrdering a b 
```

::: notes
- Notice that this compares things of type `key a` and `key b`.
- Constructor (i.e. set of keys) must match, but type of value can differ
:::

##

```haskell
deriveGEq ''PostKey
deriveGCompare ''PostKey
```

<!--
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
- e.g. if we want to `Show` or compare a `DMap`, we need to handle its values
:::

##

::: {style="width: 110%; position: relative; left: -20px;"}
```haskell
instance forall k (key :: k -> *) (f :: k -> *).
         ShowTag key f
         => Show (DMap key f)
```
:::

::: notes
- This is provided by `dependent-map`, but we need a `ShowTag` instance
:::

##

```haskell
instance Show1 f => ShowTag PostKey f where
  showTaggedPrec ::
    forall (a :: k).
    key a -> Int -> f a -> ShowS
 
```

::: notes
- Given a key, return the function required to define a `Show` instance for `f a`
- The equation for every constructor is the same
- Key is like a proxy --- only used to determine the type of the value we want to show
- Seems like we should be able to write this.
- GHC can't determine the constraint is met without a value
:::

##

```haskell
instance Show1 f => ShowTag PostKey f where
  showTaggedPrec ::
    forall (a :: k).
    key a -> Int -> f a -> ShowS
  showTaggedPrec _ = showsPrec1
```

::: notes
- Given a key, return the function required to define a `Show` instance for `f a`
- The equation for every constructor is the same
- Key is like a proxy --- only used to determine the type of the value we want to show
- Seems like we should be able to write this.
- GHC can't determine the constraint is met without a value
:::

##

```haskell
instance ShowTag PostKey f where
  showTaggedPrec PostTitle = showsPrec1
  showTaggedPrec PostId = showsPrec1
  showTaggedPrec PostAuthor = showsPrec1
  ...
```

##

::: {style="width: 125%; position: relative; left: -120px;"}
```haskell
deriveShowTag ::
  Name -> DecsQ
deriveShowTag n = do
  keyType <- reify n
  let
    mkEq conName = clause [conP conName []] (normalB (varE 'showsPrec1)) []
    mkDecl = \case
      (GadtC [conName] _bangTypes _ty) -> mkEq conName
      _ -> fail "Can only deriveShowTag with GADT constructors"
    decl = case keyType of
      TyConI (DataD _ctx _n _tyvars _kind cons _deriving) ->
        funD 'showTaggedPrec $ fmap mkDecl cons
      _ -> fail "Can only deriveShowTag with a type constructor"
  f' <- varT <$> newName "f"
  let c = cxt [appT (conT ''Show1) f']
  pure <$> instanceD c (foldl appT (conT ''ShowTag) [conT n, f']) [decl]
```
:::

##

::: {style="width: 125%; position: relative; left: -120px;"}
```haskell
class GEq tag => EqTag (tag :: k -> *) (f :: k -> *) where
  eqTagged :: forall (a :: k). tag a -> tag a -> f a -> f a -> Bool
  
  
  
  
  
 
```
:::

::: notes
Similar to `ShowTag`, `dependent-sum` also defines `EqTag`
:::

##

::: {style="width: 125%; position: relative; left: -120px;"}
```haskell
class GEq tag => EqTag (tag :: k -> *) (f :: k -> *) where
  eqTagged :: forall (a :: k). tag a -> tag a -> f a -> f a -> Bool
  
class FromJSONViaKey k f where
  parseJSONViaKey ::  k a -> Value -> Parser (f a)

class ToJSONViaKey k f where
  toJSONViaKey :: k a -> f a -> Value
```
:::

::: notes
- We'd also like Aeson instances
:::

##

::: {style="width: 125%; position: relative; left: -120px;"}
```haskell
deriveClassForGADT ::
  Name
  -> Name
  -> Name
  -> Name
  -> Name
  -> DecsQ
deriveClassForGADT klass ctx ty method f = do
  keyType <- reify ty
  let
    mkEq conName = clause [conP conName []] (normalB (varE f)) []
    mkDecl = \case
      (GadtC [conName] _bangTypes _ty) -> mkEq conName
      _ -> fail "Can only deriveFromJSONViaKey with GADT constructors"
    decl = case keyType of
      TyConI (DataD _ctx _n _tyvars _kind cons _deriving) ->
        funD method $ fmap mkDecl cons
      _ -> fail "Can only deriveFromJSONViaKey with a type constructor"
  f' <- varT <$> newName "f"
  let c = cxt [appT (conT ctx) f']
  pure <$> instanceD c (foldl appT (conT klass) [conT ty, f']) [decl]
```
:::

::: notes
`EqTag` isn't quite the same shape, but our Aeson instances and `ShowTag` are all exactly the same
shape, so we can pull out the common bits.
:::

##

```haskell
deriveFromJSONViaKey n =
  deriveClassForGADT ''FromJSONViaKey
                     ''FromJSON1
                     n
                     'parseJSONViaKey
                     'parseJSON1
```

-->
