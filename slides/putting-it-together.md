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
cCreatePost now env@Env{..} =
  let








  in



 
```

##

```haskell
cCreatePost now env@Env{..} =
  let
    gen = Just . fmap CreatePost . genPost now







  in



 
```

##

```haskell
cCreatePost now env@Env{..} =
  let
    gen = Just . fmap CreatePost . genPost now
    exe (CreatePost pm) = do
      annotateShow pm
      annotateShow $ encode pm




  in



 
```

##

```haskell
cCreatePost now env@Env{..} =
  let
    gen = Just . fmap CreatePost . genPost now
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
cCreatePost now env@Env{..} =
  let
    gen = Just . fmap CreatePost . genPost now
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
cCreatePost now env@Env{..} =
  let
    gen = Just . fmap CreatePost . genPost now
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
        posts . at o ?~ genToStatePost now p $ s
    ]
```
