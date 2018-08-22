# Putting it together

## State

##

```haskell
type Posts v = Map (Var Int v) PostMap

newtype WPState (v :: * -> *) =
  WPState
  { _posts :: Posts v
  }
  deriving (Eq, Show)
```

::: notes
- Start by looking at state
- So far only testing posts, so just a map of id to post data
:::

## Inputs

## { data-background-image="images/wp-failure-example-inputs.png"
    class="wp-example"
    }

##

```haskell
newtype CreatePost (v :: * -> *) =
  CreatePost PostMap
  deriving (Show)
  
  
  
  
  






 
```

##

```haskell
newtype CreatePost (v :: * -> *) =
  CreatePost PostMap
  deriving (Show)
  
data DeletePost (v :: * -> *) =
  DeletePost (Var Int v) (Maybe Bool)
  deriving (Show)
  
  
  
  
  


 
```

##

```haskell
newtype CreatePost (v :: * -> *) =
  CreatePost PostMap
  deriving (Show)
  
data DeletePost (v :: * -> *) =
  DeletePost (Var Int v) (Maybe Bool)
  deriving (Show)
  
data UpdatePost (v :: * -> *) =
  UpdatePost (Var Int v) PostMap
  deriving (Show)
  
  
  
  
```

##

```haskell
newtype CreatePost (v :: * -> *) =
  CreatePost PostMap
  deriving (Show)
  
data DeletePost (v :: * -> *) =
  DeletePost (Var Int v) (Maybe Bool)
  deriving (Show)
  
data UpdatePost (v :: * -> *) =
  UpdatePost (Var Int v) PostMap
  deriving (Show)
  
newtype GetPost (v :: * -> *) =
  GetPost (Var Int v)
  deriving (Show)
```

##

```haskell
genPost ::
  ( MonadGen n
  , HasPostMaps state
  )
  => LocalTime
  -> state (v :: * -> *)
  -> n PostMap
genPost now s = do
  content <- genAlpha 1 500
  excerpt' <- T.take <$> Gen.int (Range.linear 1 (T.length content - 1)) <*> pure content
  status <- Gen.filter (/= Trash) Gen.enumBounded
  let
    genDate =
      if status == Future
      then Gen.filter (not . withinADay now) genLocalTime
      else genLocalTime
    excerpt = bool content excerpt' (T.null excerpt')
    genSlug = Gen.filter (not . existsPostWithSlug s) . fmap mkSlug $ genAlpha 1 300
    gensI = [
        PostDateGmt :=> genDate
      , PostSlug :=> genSlug
      , PostStatus :=> pure status
      , PostTitle :=> mkCreateR <$> genAlpha 1 30
      , PostContent :=> pure (mkCreatePR content)
      , PostAuthor :=> pure (Author 1)
      , PostExcerpt :=> pure (mkCreatePR excerpt)
      ]
  DM.traverseWithKey (const (fmap pure)) $ DM.fromList gensI
```

::: notes
 - If future, ensure date is not within a day 
   + WP auto publishes at given timestamp -- don't want it to change
 - No empty slugs --- WP defaults them
   + can't check equality without implementing slug logic
:::

##

```haskell
genPost ::
  ( MonadGen n
  , HasPostMaps state
  )
  => LocalTime
  -> state (v :: * -> *)
  -> n PostMap
genPost now s = do
  content <- genAlpha 1 500
  excerpt' <- T.take <$> Gen.int (Range.linear 1 (T.length content - 1)) <*> pure content
  status <- Gen.filter (/= Trash) Gen.enumBounded
  let
    genDate =
      if status == Future
      then Gen.filter (not . withinADay now) genLocalTime
      else genLocalTime
    excerpt = bool content excerpt' (T.null excerpt')
    genSlug = Gen.filter (not . existsPostWithSlug s) . fmap mkSlug $ genAlpha 1 300
    gensI = [
        PostDateGmt :=> genDate
      , PostSlug :=> genSlug
      , PostStatus :=> pure status
      , PostTitle :=> mkCreateR <$> genAlpha 1 30
      , PostContent :=> pure (mkCreatePR content)
      , PostAuthor :=> pure (Author 1)
      , PostExcerpt :=> pure (mkCreatePR excerpt)
      ]
  DM.traverseWithKey (const (fmap pure)) $ DM.fromList gensI
```

##

```haskell
genPost ::
  ( MonadGen n
  , HasPostMaps state
  )
  => LocalTime
  -> state (v :: * -> *)
  -> n PostMap
genPost now s = do
  content <- genAlpha 1 500
  excerpt' <- T.take <$> Gen.int (Range.linear 1 (T.length content - 1)) <*> pure content
  status <- Gen.filter (/= Trash) Gen.enumBounded
  let
    genDate =
      if status == Future
      then Gen.filter (not . withinADay now) genLocalTime
      else genLocalTime
    excerpt = bool content excerpt' (T.null excerpt')
    genSlug = Gen.filter (not . existsPostWithSlug s) . fmap mkSlug $ genAlpha 1 300
    gensI = [
        PostDateGmt :=> genDate
      , PostSlug :=> genSlug
      , PostStatus :=> pure status
      , PostTitle :=> mkCreateR <$> genAlpha 1 30
      , PostContent :=> pure (mkCreatePR content)
      , PostAuthor :=> pure (Author 1)
      , PostExcerpt :=> pure (mkCreatePR excerpt)
      ]
  DM.traverseWithKey (const (fmap pure)) $ DM.fromList gensI
```

##

```haskell
genPost ::
  ( MonadGen n
  , HasPostMaps state
  )
  => LocalTime
  -> state (v :: * -> *)
  -> n PostMap
genPost now s = do
  content <- genAlpha 1 500
  excerpt' <- T.take <$> Gen.int (Range.linear 1 (T.length content - 1)) <*> pure content
  status <- Gen.filter (/= Trash) Gen.enumBounded
  let
    genDate =
      if status == Future
      then Gen.filter (not . withinADay now) genLocalTime
      else genLocalTime
    excerpt = bool content excerpt' (T.null excerpt')
    genSlug = Gen.filter (not . existsPostWithSlug s) . fmap mkSlug $ genAlpha 1 300
    gensI = [
        PostDateGmt :=> genDate
      , PostSlug :=> genSlug
      , PostStatus :=> pure status
      , PostTitle :=> mkCreateR <$> genAlpha 1 30
      , PostContent :=> pure (mkCreatePR content)
      , PostAuthor :=> pure (Author 1)
      , PostExcerpt :=> pure (mkCreatePR excerpt)
      ]
  DM.traverseWithKey (const (fmap pure)) $ DM.fromList gensI
```

##

```haskell
genPost ::
  ( MonadGen n
  , HasPostMaps state
  )
  => LocalTime
  -> state (v :: * -> *)
  -> n PostMap
genPost now s = do
  content <- genAlpha 1 500
  excerpt' <- T.take <$> Gen.int (Range.linear 1 (T.length content - 1)) <*> pure content
  status <- Gen.filter (/= Trash) Gen.enumBounded
  let
    genDate =
      if status == Future
      then Gen.filter (not . withinADay now) genLocalTime
      else genLocalTime
    excerpt = bool content excerpt' (T.null excerpt')
    genSlug = Gen.filter (not . existsPostWithSlug s) . fmap mkSlug $ genAlpha 1 300
    gensI = [
        PostDateGmt :=> genDate
      , PostSlug :=> genSlug
      , PostStatus :=> pure status
      , PostTitle :=> mkCreateR <$> genAlpha 1 30
      , PostContent :=> pure (mkCreatePR content)
      , PostAuthor :=> pure (Author 1)
      , PostExcerpt :=> pure (mkCreatePR excerpt)
      ]
  DM.traverseWithKey (const (fmap pure)) $ DM.fromList gensI
```

##

```haskell
genPost ::
  ( MonadGen n
  , HasPostMaps state
  )
  => LocalTime
  -> state (v :: * -> *)
  -> n PostMap
genPost now s = do
  content <- genAlpha 1 500
  excerpt' <- T.take <$> Gen.int (Range.linear 1 (T.length content - 1)) <*> pure content
  status <- Gen.filter (/= Trash) Gen.enumBounded
  let
    genDate =
      if status == Future
      then Gen.filter (not . withinADay now) genLocalTime
      else genLocalTime
    excerpt = bool content excerpt' (T.null excerpt')
    genSlug = Gen.filter (not . existsPostWithSlug s) . fmap mkSlug $ genAlpha 1 300
    gensI = [
        PostDateGmt :=> genDate
      , PostSlug :=> genSlug
      , PostStatus :=> pure status
      , PostTitle :=> mkCreateR <$> genAlpha 1 30
      , PostContent :=> pure (mkCreatePR content)
      , PostAuthor :=> pure (Author 1)
      , PostExcerpt :=> pure (mkCreatePR excerpt)
      ]
  DM.traverseWithKey (const (fmap pure)) $ DM.fromList gensI
```

##

```haskell
genPost ::
  ( MonadGen n
  , HasPostMaps state
  )
  => LocalTime
  -> state (v :: * -> *)
  -> n PostMap
genPost now s = do
  content <- genAlpha 1 500
  excerpt' <- T.take <$> Gen.int (Range.linear 1 (T.length content - 1)) <*> pure content
  status <- Gen.filter (/= Trash) Gen.enumBounded
  let
    genDate =
      if status == Future
      then Gen.filter (not . withinADay now) genLocalTime
      else genLocalTime
    excerpt = bool content excerpt' (T.null excerpt')
    genSlug = Gen.filter (not . existsPostWithSlug s) . fmap mkSlug $ genAlpha 1 300
    gensI = [
        PostDateGmt :=> genDate
      , PostSlug :=> genSlug
      , PostStatus :=> pure status
      , PostTitle :=> mkCreateR <$> genAlpha 1 30
      , PostContent :=> pure (mkCreatePR content)
      , PostAuthor :=> pure (Author 1)
      , PostExcerpt :=> pure (mkCreatePR excerpt)
      ]
  DM.traverseWithKey (const (fmap pure)) $ DM.fromList gensI
```

##

```haskell
genPost ::
  ( MonadGen n
  , HasPostMaps state
  )
  => LocalTime
  -> state (v :: * -> *)
  -> n PostMap
genPost now s = do
  content <- genAlpha 1 500
  excerpt' <- T.take <$> Gen.int (Range.linear 1 (T.length content - 1)) <*> pure content
  status <- Gen.filter (/= Trash) Gen.enumBounded
  let
    genDate =
      if status == Future
      then Gen.filter (not . withinADay now) genLocalTime
      else genLocalTime
    excerpt = bool content excerpt' (T.null excerpt')
    genSlug = Gen.filter (not . existsPostWithSlug s) . fmap mkSlug $ genAlpha 1 300
    gensI = [
        PostDateGmt :=> genDate
      , PostSlug :=> genSlug
      , PostStatus :=> pure status
      , PostTitle :=> mkCreateR <$> genAlpha 1 30
      , PostContent :=> pure (mkCreatePR content)
      , PostAuthor :=> pure (Author 1)
      , PostExcerpt :=> pure (mkCreatePR excerpt)
      ]
  DM.traverseWithKey (const (fmap pure)) $ DM.fromList gensI
```

