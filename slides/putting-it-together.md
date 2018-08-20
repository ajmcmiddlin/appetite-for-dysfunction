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

