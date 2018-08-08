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
aPost :: Applicative f => DMap PostKey f
aPost =
  fromList [ PostTitle ==> "Hello Compose"
           , PostStatus ==> Publish
           , PostAuthor ==> 1
           ]
```

##

```haskell
```

