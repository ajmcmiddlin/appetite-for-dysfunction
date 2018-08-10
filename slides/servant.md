# `servant`

##

```haskell
API -> ClientFunctions
```

::: notes
- `servant` not just for servers
- server doesn't need to be written in Haskell
- client functions aren't magic --- translate API to HTTP requests
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
listPosts :: ListPostsMap -> ClientM [PostMap]
listPostsAuth :: BasicAuthData -> ListPostsMap -> ClientM [PostMap]
createPost :: BasicAuthData -> PostMap -> ClientM PostMap
getPost :: BasicAuthData -> Int -> ClientM PostMap
deletePost :: BasicAuthData -> Int -> Maybe NoForceDelete -> ClientM DeletedPost
deletePostForce :: BasicAuthData -> Int -> ForceDelete -> ClientM DeletedPost

(     listPosts :<|> listPostsAuth :<|> createPost :<|> getPost
 :<|> deletePost :<|> deletePostForce ) =
  client postsAPI
```

## `QueryParamMap`?

##

```haskell
type List = QueryParamMap ListPostsKey Identity :>
            Get '[JSON] [PostMap]
```

##

```haskell
            QueryParamMap ListPostsKey Identity
            
```

##



