# Putting it together

## State

##

<pre><code class="haskell" data-trim data-noescape>
type Posts v = Map (Var Int v) StatePost

<span class="fragment">newtype WPState (v :: * -> *) =
  WPState
  { _posts :: Posts v
  }
  deriving (Eq, Show)</span>
</code></pre>

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

## Creating posts

##

```haskell
cCreatePost env@Env{..} =
  let








  in



 
```

##

```haskell
cCreatePost env@Env{..} =
  let
    gen = Just . fmap CreatePost . genPost







  in



 
```

##

```haskell
cCreatePost env@Env{..} =
  let
    gen = Just . fmap CreatePost . genPost
    exe (CreatePost pm) = do
      annotateShow pm
      annotateShow $ encode pm




  in



 
```

##

```haskell
cCreatePost env@Env{..} =
  let
    gen = Just . fmap CreatePost . genPost
    exe (CreatePost pm) = do
      annotateShow pm
      annotateShow $ encode pm
      let create =
        fmap (runIdentity . (DM.! PostId))
             (createPost (auth env) pm)

  in



 
```

##

```haskell
cCreatePost env@Env{..} =
  let
    gen = Just . fmap CreatePost . genPost
    exe (CreatePost pm) = do
      annotateShow pm
      annotateShow $ encode pm
      let create =
        fmap (runIdentity . (DM.! PostId))
             (createPost (auth env) pm)
      evalEither =<< liftIO (runClientM create servantClient)
  in



 
```

##

```haskell
cCreatePost env@Env{..} =
  let
    gen = Just . fmap CreatePost . genPost
    exe (CreatePost pm) = do
      annotateShow pm
      annotateShow $ encode pm
      let create =
        fmap (runIdentity . (DM.! PostId))
             (createPost (auth env) pm)
      evalEither =<< liftIO (runClientM create servantClient)
  in
    Command gen exe [
      Update $ \s (CreatePost p) o ->
        posts . at o ?~ StatePost p $ s
    ]
```

## Updating posts

##

```haskell
cUpdatePost env@Env{..} =
  let
    gen s =
      (flip UpdatePost <$> genPost s <*>) <$> genId s
    exe (UpdatePost pId pm) = do
      annotateShow pm
      annotateShow $ encode pm
      let update =
        fmap (runIdentity . (DM.! PostId))
             (updatePost (auth env) (concrete pId) pm)
      evalEither =<< liftIO (runClientM update servantClient)
  in
    Command gen exe [
      Require $ \s (UpdatePost varId _) ->
        s ^. posts . at varId & not . null
    , Update $ \s (UpdatePost pId p) _ ->
        posts . at pId ?~ StatePost p $ s
    ]
```

##

```haskell













      Require $ \s (UpdatePost varId _) ->
        s ^. posts . at varId & not . null


 
```

## Getting posts

##

```haskell
cGetPost now env@Env{..} =
  let
    gen s =
      (fmap . fmap) GetPost $ genId s
    exe (GetPost varId) = do
      let
        get = getPost (auth env) (concrete varId)
      evalEither =<< liftIO (runClientM get servantClient)
  in
    Command gen exe [
      Require $ \s (GetPost varId) ->
        s ^. posts . at varId & not . null
    , Ensure $ \_so sn (GetPost varId) p -> do
        stateMap <- eval $ (sn ^. posts) M.! varId
        annotateShow stateMap
        annotateShow p
        postsEq now stateMap p
    ]
```

##

```haskell












      Ensure $ \_so sn (GetPost varId) p -> do
        stateMap <- eval $ (sn ^. posts) M.! varId
        annotateShow stateMap
        annotateShow p
        postsEq now stateMap p
     
```

