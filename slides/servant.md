# `servant`

##

```haskell
API -> ClientFunctions
```

::: notes
- `servant` is an excellent library for web APIs
- client functions speak HTTP --- server doesn't have to be in Haskell
- we deal in Haskell data types
:::

##

<img src="images/wordpress-api.png" alt="wordpress REST API for posts" style="width: 70%"/>

##

```haskell
type Posts =
  "posts" :> (
  List
  :<|> BasicAuth "wordpress" () :> List

  :<|> BasicAuth "wordpress" () :> ReqBody '[JSON] PostMap :>
       Post '[JSON] PostMap

  :<|> BasicAuth "wordpress" () :> Capture "id" Int :>
       Get '[JSON] PostMap

  :<|> BasicAuth "wordpress" () :> Capture "id" Int :>
       QueryParam "force" NoForceDelete :>
       Delete '[JSON] DeletedPost

  :<|> BasicAuth "wordpress" () :> Capture "id" Int :>
       QueryParam' "force" ForceDelete :>
       Delete '[JSON] DeletedPost
  )

type List = QueryParamMap ListPostsKey Identity :>
            Get '[JSON] [PostMap]
```

##

```haskell
(     listPosts :<|> listPostsAuth
 :<|> getPost
 :<|> createPost
 :<|> deletePost :<|> deletePostForce ) =
   client postsAPI
```

## Query parameters

##

::: {.smaller-code}
```haskell
data ListPostsKey a where
  ListPostsContext           :: ListPostsKey Context
  ListPostsPage              :: ListPostsKey Int
  ListPostsPerPage           :: ListPostsKey Int
  ListPostsSearch            :: ListPostsKey Text
  ListPostsAfter             :: ListPostsKey LocalTime
  ListPostsAuthor            :: ListPostsKey Author
  ListPostsAuthorExclude     :: ListPostsKey (NonEmpty Author)
  ListPostsBefore            :: ListPostsKey LocalTime
  ListPostsExclude           :: ListPostsKey (NonEmpty Int)
  ListPostsInclude           :: ListPostsKey (NonEmpty Int)
  ListPostsOffset            :: ListPostsKey Int
  ListPostsOrder             :: ListPostsKey Order
  ListPostsSlug              :: ListPostsKey (NonEmpty Text)
  ListPostsStatus            :: ListPostsKey Status
  ListPostsCategories        :: ListPostsKey (NonEmpty Text)
  ListPostsCategoriesExclude :: ListPostsKey (NonEmpty Text)
  ListPostsTags              :: ListPostsKey (NonEmpty Text)
  ListPostsTagsExclude       :: ListPostsKey (NonEmpty Text)
  ListPostsSticky            :: ListPostsKey Sticky
```
:::

::: notes
- These are all the criteria we can specify to filter posts when listing
- 19 constructors makes for 19 factorial combinations of parameters specified
- `DMap` to the rescue again
:::

##

```haskell
type List = QueryParamMap ListPostsKey Identity :>
            Get '[JSON] [PostMap]
```

::: notes
- Look back at part of our API.
- WP API requires `GET` with query parameters.
- `QueryParamMap` allows us to specify query parameters as a `DMap`.
- `QueryParamMap` doesn't come with `servant`. I defined it.
- `servant`'s query parameter handling results in a `Maybe` value for each parameter
- `servant`'s use of type classes allow us to easily extend its capabilities.
:::

<!--
##

```haskell
class HasClient (api :: k) where

  type family Client (api :: k) :: *

  clientWithRoute ::
    Data.Proxy.Proxy api
    -> Servant.Common.Req.Req
    -> Client api
```

::: notes
- `servant` defines this class
- Handles updating of requests for each API combinator
:::

##

```haskell
data QueryParam (sym :: Symbol) a

instance (KnownSymbol sym, ToHttpApiData a, HasClient api)
      => HasClient (QueryParam sym a :> api) where

  type Client (QueryParam sym a :> api) =
    Maybe a -> Client api
```

::: notes
- `QueryParam` type has no constructors
- instance definitions pattern match the combinator and the rest of the API
- `Client` shows that each `QueryParam` results in a `Maybe` parameter
- Without `QueryParamMap` would have to translate `DMap` to function call with
  19 parameters.
:::

-->

##

```haskell
data QueryParamMap (key :: * -> *) (f :: * -> *)

instance (ToQueryParamKeyValues key f, HasClient api)
      => HasClient (QueryParamMap key f :> api) where

  type Client (QueryParamMap key f :> api) =
    DMap key f -> Client api

  clientWithRoute Proxy req dm =
    let
      addPair (k, v) = appendToQueryString k (Just v)
      f ka fa req' =
        foldr addPair req' $ toQueryParamKeyValues ka fa
    in
      clientWithRoute (Proxy :: Proxy api) $
        DM.foldrWithKey f req dm
```

##

```haskell





  type Client (QueryParamMap key f :> api) =
    DMap key f -> Client api








 
```

