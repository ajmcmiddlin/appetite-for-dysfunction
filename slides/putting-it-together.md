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
  status <- Gen.enumBounded
  let
    genDate =
      if status == Future
      then Gen.filter (not . withinADay now) genLocalTime
      else genLocalTime
    excerpt = bool content excerpt' (T.null excerpt')
    genSlug = Gen.filter (not . existsPostWithSlug s) . fmap mkSlug $ genAlpha 1 300
    gensI = [
        PostDateGmt :=> genDate
        -- We don't want empty slugs because then WordPress defaults them and we can't be
        -- certain about when things should be equal without implementing their defaulting logic.
      , PostSlug :=> genSlug
      , PostStatus :=> Gen.filter (/= Trash) Gen.enumBounded
     -- , PostPassword
      , PostTitle :=> mkCreateR <$> genAlpha 1 30
      , PostContent :=> pure (mkCreatePR content)
      -- TODO: author should come from state. Start state has user with ID = 1.
      , PostAuthor :=> pure (Author 1)
      , PostExcerpt :=> pure (mkCreatePR excerpt)
     -- , PostFeaturedMedia
     -- , PostCommentStatus
     -- , PostPingStatus
     -- , PostFormat
     -- , PostMeta
     -- , PostSticky
     -- , PostTemplate
     -- , PostCategories
     -- , PostTags
      ]
    -- gensV = [
    --   ]
  DM.traverseWithKey (const (fmap pure)) $ DM.fromList gensI
```

::: notes
 - If future, ensure date is not within a day 
   + WP auto publishes at given timestamp -- don't want it to change
:::

