---
title: Appetite for dysfunction transcript
---

## Introduction

Hi, my name is Andrew McMiddlin. I'm the programmer formerly known as Andrew McCluskey, and I work
at Data61's Queensland Functional Programming Lab.

A heads up before we begin. This talk is not a deep dive into state machine testing. It's more of an
experience report and high level overview of some functional programming tools and techniques that I
found helpful while doing state machine testing. So don't freak out if you don't come away with a
deep understanding of these topics. My goal is to just make you aware of these techniques and give
you a sense of how they can be used.

Today I'm going to tell you all about how I used functional programming tools to test WordPress;
the blogging and content management system built on PHP and MySQL. Before I get into that though, I
want to give you all a little context on how and why I got here.

To start with, let's take a very quick look at property based testing. Property based testing is a
method of testing whereby inputs to a function are generated randomly, and certain invariants are
checked against the outputs produced by the function being tested. This is in contrast to
traditional unit, or case based, testing, where specific inputs are chosen by the programmer and the
function's output compared to an expected value.

In the Haskell world, this approach was brought to life by Claessen and Hughes in 2000 with
[QuickCheck](http://www.cse.chalmers.se/~rjmh/QuickCheck/). However, I tend to use a relative
newcomer to the property testing world called `hedgehog`. I won't go into the differences between
the two, but if you're interested, `hedgehog`'s author (Jacob Stanley) gave an excellent talk at
Lambda Jam in 2017 that answers this question and serves as an introduction to the library. The talk
is called _Gens and Roses: Appetite for reduction_. This talk's name is a riff on that talk's title.

In `hedgehog`, a property test to ensure that reversing a list twice is the same as doing nothing
looks like this:

```haskell
-- Reverse is involutive
propReverse :: Property
propReverse =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    reverse (reverse xs) === xs
```

As we can see, this code is first generating some random data --- a list of characters with a length
between 0 and 100 --- and then ensuring that the randomly generated list does not violate the stated
property.

Property based testing is a very powerful technique, as our tests are no longer limited to the
examples that we can think of and can be bothered writing. We can now express our tests in terms of
every possible value our function might get, and let a machine execute as many cases as we can be
bothered waiting for. It also forces us to think about the software we're writing in terms of every
possible scenario, rather than focusing on specific examples.

## State machine testing

While very powerful, and very well suited to testing referentially transparent functions, property
based testing in this form is less well equipped to test the higher level properties of stateful
systems. We might write our web application as a referentially transparent expression, but there are
a whole slew of properties that involve the system's state and interactions with the outside world
when the software is running. Just as Claessen and Hughes brought property based testing to Haskell
in 2000 with QuickCheck, they then went on to deliver a solution to this problem in their 2002 paper
on monadic QuickCheck. Again following in QuickCheck's footsteps, `hedgehog` has an implementation
of property based state machine testing that greatly simplifies testing stateful software.

In short, this approach comprises the following steps:

1. Model the application's state with a Haskell data type.
2. Model the actions that might alter the application state.
3. Randomly generate a list of actions with randomly generated input data.
4. Run these actions against the application.
5. Update the model state with the expected changes.
6. Ensure that the model always agrees with the application.

I gave a talk earlier this year at Lambda Jam that aimed to introduce the concept of state machine
testing and how to do it in `hedgehog`. In the talk I covered how `hedgehog`'s state machine testing
facilities work, and provided examples that test part of a Haskell application. This talk is a
follow up in response to questions I received after that talk. Most notably: is it possible to use
hedgehog to test software _not_ written in Haskell. The answer to this question is that it most
definitely can, and to really prove the point, I decided to test WordPress. Using state machine
testing with WordPress uncovered a few interesting challenges that I didn't encounter testing a
Haskell application, so really the focus of this talk is the tools and techniques I used to test
WordPress and not so much the details of the tests.

## WordPress

This is what WordPress say about themselves.

> WordPress is open source software you can use to create a beautiful website, blog, or app.

> WordPress is both free and priceless at the same time.

> 31% of the web uses WordPress, from hobby blogs to the biggest news sites online.

> Over 60 million people have chosen WordPress to power the place on the web they call “home” — join the family.

In short, WordPress is blogging, or web content management software. Given its prevalence, I'm
guessing most people here have at least heard of it. I actually have a couple of old WordPress blogs
lying around, and my assessment of WordPress isn't quite as positive as their marketing material's.
Let's just say that I will be very glad to find the time to move them to Hakyll.

WordPress was started in 2003 as a fork from another project called b2 that was started in 2001. It
is currently 2018, which means that, including its time as b2, the project has been active for about
17 years. It's written in PHP and MySQL, and has a large number of plugins and themes available for
it. Given it's history and technical foundations, you might think that WordPress isn't the most
robust or easy to modify software, and if you did, I would say you are right. To be fair though, I
still find it surprising that it functions as well as it does.

## Let there be... WordPress

Step zero of testing existing software is being able to run the software. Being a paid up member of
the Church of Lambda, I'd like my deployments to be referentially transparent too please. To achieve
this goal I used NixOps. NixOps is the devops spinoff from Nix, where Nix is a purely functional
package manager that uses referentially transparent expressions to build software. As a result, I
have a referentially transparent expression representing everything I need to setup a working
WordPress installation. It gets even better. Nix has a huge library of expressions for all sorts of
software, including WordPress and its dependencies, which means that I had to do very little work.
Here's the code in its entirety.

```nix
{
network.description = "Wordpress";

wordpress =
  { config, pkgs, ... }:
  let
    wpPackage = pkgs.fetchFromGitHub {
      owner = "WordPress";
      repo = "WordPress";
      rev = "4.9.7";
      sha256 = "1kxwk7mqhi9n334i6yb9iyi9vbl07agbbv5fzfhrcx5d2v13h48r";
    };

    basicAuthPlugin = pkgs.stdenv.mkDerivation {
      name = "basic-auth-plugin";
      # Download the theme from the wordpress site
      src = pkgs.fetchurl {
        url = https://github.com/WP-API/Basic-Auth/archive/9e9d5267c7805c024f141d115b224cdee5a10008.zip;
        sha256 = "b7f4fe0e6064040eeec2d34c27296cc69c92ed015c8d4164cf86af002fde2ddd";
      };
      # We need unzip to build this package
      buildInputs = [ pkgs.unzip ];
      # Installing simply means copying all files to the output directory
      installPhase = "mkdir -p $out; cp -R * $out/";
    };

    twentySeventeen = pkgs.stdenv.mkDerivation {
      name = "theme-twenty-seventeen";
      # Download the theme from the wordpress site
      src = pkgs.fetchurl {
        url = https://downloads.wordpress.org/theme/twentyseventeen.1.6.zip;
        sha256 = "0cch9bvap4r0775f055mynbf0d6k8zrqyn2mdwkbn6rr12hn526b";
      };
      # We need unzip to build this package
      buildInputs = [ pkgs.unzip ];
      # Installing simply means copying all files to the output directory
      installPhase = "mkdir -p $out; cp -R * $out/";
    };
  in
  {
    services.mysql = {
      enable = true;
      package = pkgs.mysql;
      initialScript = ./init.sql;
    };

    services.httpd = {
      enable = true;
      logPerVirtualHost = true;
      adminAddr="andrew@qfpl.io";
      extraModules = [
        { name = "php7"; path = "${pkgs.php}/modules/libphp7.so"; }
      ];

      virtualHosts = [
        {
          hostName = "wordpress";
          extraSubservices =
            [
              {
                serviceType = "wordpress";
                dbPassword = "wordpress";
                wordpressUploads = "/data/uploads";
                languages = [ "en_GB" ];
                package = wpPackage;
                plugins = [ basicAuthPlugin ];
                themes = [ twentySeventeen ];
              }
            ];
        }
      ];
    };

    # HTTP, HTTPS, MySQL
    networking.firewall.allowedTCPPorts = [ 80 443 3306 ];
  };
}
```

Now that I've specified the WordPress environment, I need to describe how to deploy it. For my local
tests, I'm using VirtualBox, which is supported out of the box by NixOps, such that I don't need to
specify much.

```nix
{
  wordpress =
    { config, pkgs, ... }:
    { deployment.targetEnv = "virtualbox";
      deployment.virtualbox.memorySize = 4096; # megabytes
      deployment.virtualbox.vcpu = 4; # number of cpus
      deployment.virtualbox.headless = true;
    };
}
```

Once my specifications are in place I can deploy the environment for testing.

```
$ nixops create ./wp.nix ./wp-vbox.nix -d wp
$ nixops deploy -d wp
```

The output from the deployment tells me the IP address, but I can also find it with the following.

```
$ nixops info -d wp
```

I can also SSH into the machine if necessary like so, where `wordpress` is the name of the machine
in the deployment that I want to SSH into. There might be multiple machines in a deployment, so
specifying the machine is necessary.

```
$ nixops ssh -d wp wordpress
```

## The API

Now that we have a deployment of WordPress to test, let's investigate the API. Rather than trying to
drive the tests through a browser, we're going to use WordpPress's REST API. There is some
documentation for the API, however it doesn't help much beyond telling you the endpoints and most of
the top level fields in each object. Other than that, it's what you would expect: different
endpoints for different resources, HTTP methods determining the actions taken on a resource, and
JSON for the interchange format.

## Options, options everywhere

Here's an example of the JSON used to create a post.

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

Here's another example.

```json
{
  "title": "A post"
}
```

Here's an example of a post returned after creation.

```json
{
  "id": 14,
  "date": "2018-08-06T00:49:37",
  "date_gmt": "2018-08-06T00:49:37",
  "guid": {
    "rendered": "http://192.168.56.101/?p=14",
    "raw": "http://192.168.56.101/?p=14"
  },
  "modified": "2018-08-06T00:49:37",
  "modified_gmt": "2018-08-06T00:49:37",
  "password": "",
  "slug": "",
  "status": "draft",
  "type": "post",
  "link": "http://192.168.56.101/?p=14",
  "title": {
    "raw": "A post",
    "rendered": "A post"
  },
  "content": {
    "raw": "",
    "rendered": "",
    "protected": false
  },
  "excerpt": {
    "raw": "",
    "rendered": "",
    "protected": false
  },
  "author": 1,
  "featured_media": 0,
  "comment_status": "open",
  "ping_status": "open",
  "sticky": false,
  "template": "",
  "format": "standard",
  "meta": [],
  "categories": [],
  "tags": [],
  "_links": {
    "self": [
      {
        "href": "http://192.168.56.101/wp-json/wp/v2/posts/14"
      }
    ],
    "collection": [
      {
        "href": "http://192.168.56.101/wp-json/wp/v2/posts"
      }
    ],
    "about": [
      {
        "href": "http://192.168.56.101/wp-json/wp/v2/types/post"
      }
    ],
    "author": [
      {
        "embeddable": true,
        "href": "http://192.168.56.101/wp-json/wp/v2/users/1"
      }
    ],
    "replies": [
      {
        "embeddable": true,
        "href": "http://192.168.56.101/wp-json/wp/v2/comments?post=14"
      }
    ],
    "version-history": [
      {
        "href": "http://192.168.56.101/wp-json/wp/v2/posts/14/revisions"
      }
    ],
    "wp:attachment": [
      {
        "href": "http://192.168.56.101/wp-json/wp/v2/media?parent=14"
      }
    ],
    "wp:term": [
      {
        "taxonomy": "category",
        "embeddable": true,
        "href": "http://192.168.56.101/wp-json/wp/v2/categories?post=14"
      },
      {
        "taxonomy": "post_tag",
        "embeddable": true,
        "href": "http://192.168.56.101/wp-json/wp/v2/tags?post=14"
      }
    ],
    "curies": [
      {
        "name": "wp",
        "href": "https://api.w.org/{rel}",
        "templated": true
      }
    ]
  }
}
```

Obviously there's a broad range of acceptable JSON formats for a post. Almost every field is
optional such that it doesn't appear at all. Given WordPress's use of PHP and Javascript, the
dynamic nature of its data isn't a surprise, but it does make things a little more interesting when
dealing with them from Haskell.

The first solution that probably comes to mind is to use a record and make every optional field a
`Maybe`. This is certainly an option, however it gets cumbersome pretty quickly. The issue with this
solution is that you're often dealing with only a few fields, but must account for all of them at
all times --- even if it's just setting them all to `Nothing`.

Building on this idea, one might decide to use multiple post types, probably with a sprinkling of
language extensions to avoid some of the boilerplate of moving between them. However, without types
or a well specified API, and given all the possible combinations, this didn't seem like it would be
fun. I'd essentially have to define the type via trial and error with a slow feedback loop.

Luckily, there's a better option. Just before I encountered this problem, a couple of my colleagues
were spruiking dependent maps for dealing with dynamic JSON objects, so I decided to give them a go.
As we'll see, they provide a good trade-off for this problem. We maintain the type safety that is a
hard requirement for me, while having a more flexible structure for our data that fits the problem
well.

## `dependent-map`

`dependent-map` is a Haskell package that provides a map data structure where each value's type is
dependent on the key's value. It's somewhere between a record and `Data.Map`. Like a record, the set
of keys is fixed and known ahead of time, and the type of each value corresponds to the key's name.
However, unlike a record and more like `Data.Map`, the map may contain zero or more of the keys.

Let's take a look at an example. First, we define our key type. This specifies the constructor for
each possible key, as well as the type of its corresponding value. Here we have 3 keys (`PostTitle`,
`PostId`, and `PostStatus`). Each of them maps to values of type `Text`, `Int`, and `Status`
respectively. You'll also notice we're using GADTs, allowing the type parameter `a` to be determined
by the constructor used.

```haskell
data PostKey a where
  PostTitle  :: PostKey Text
  PostId     :: PostKey Int
  PostStatus :: PostKey Status
```

As is common for map types, we need a way to compare and order keys. Normally this would result in a
constraint that requires keys to have an instance of the `Ord` type class.

```haskell
class Eq a => Ord a where
  compare :: a -> a -> Ordering
  ...
```

However, we can't write an `Ord` instance for our key type because we need to compare values that
share a type constructor but aren't necessarily of the same type. To enable this comparison,
`dependent-map` uses the class below.

```haskell
class GEq f => GCompare (f :: k -> *) where
  gcompare :: f a -> f b -> GOrdering a b 
```

Writing these instances by hand would be cumbersome, so thankfully the `dependent-sum-template`
package provides some template Haskell to produce them for us. The generated `GEq` instance does
what you'd expect and returns equal only when the two constructors are the same. The `GCompare`
instance is also unremarkable in that it simply declares keys to be in the order they appear in the
data type definition.

```haskell
deriveGEq ''PostKey
deriveGCompare ''PostKey
```

Now that we have a key type and the necessary instances for it, we can create a map using `fromList`
or `insert` values into `empty`. You'll notice that our map type takes a type constructor --- in
this case `Identity` --- as an argument. This type constructor wraps each value stored in the map.

```haskell
(==>) :: Applicative f => tag a -> a -> DSum tag f

aPost, aPost' :: DMap PostKey Identity

aPost =
  fromList [PostTitle ==> "Hello Compose", PostStatus ==> Publish]

aPost' =
  insert PostTitle (Identity "Hello again") empty
```

**TODO: TALK ABOUT ADDITIONAL INSTANCES WE NEED**

## servant

The final piece in this puzzle is using servant to specify the API and provide a high level client
to query it. To start with, let's look at the endpoints we're testing. Given WordPress is about
producing content, the posts API seemed like an interesting and logical place to start. Here are
some of the things we can do.


Endpoint          Method    Description
-----------       --------- --------------
/posts            `GET`     Lists posts

/posts            `POST`    Create a post

/posts/&lt;id&gt; `GET`     Get a post

/posts/&lt;id&gt; `DELETE`  Delete a post

From this we get the following `servant` API type.

```haskell
type Posts =
  "posts" :>
  (    List
  :<|> BasicAuth "wordpress" () :> List
  :<|> BasicAuth "wordpress" () :> ReqBody '[JSON] PostMap :> Post '[JSON] PostMap
  :<|> BasicAuth "wordpress" () :> Capture "id" Int :> Get '[JSON] PostMap
  :<|> BasicAuth "wordpress" () :> Capture "id" Int :> QueryParam "force" NoForceDelete :> Delete '[JSON] DeletedPost
  :<|> BasicAuth "wordpress" () :> Capture "id" Int :> QueryParam' "force" ForceDelete :> Delete '[JSON] DeletedPost
  )

type PostMap = DMap PostKey Identity
type List = QueryParamMap ListPostsKey Identity :> Get '[JSON] [PostMap]
```

From this type, servant can produce client functions for us. This saves us having to deal explicitly
with `HTTP` libraries and formulating the correct request values by hand.

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

For those already familiar with servant, you might have noticed that the `List` type used in our API
definition makes use of a media type called `QueryParamMap`, which is not defined in `servant`. This
is a new media type I defined that allows the use of `DMap`s to specify query parameters. You see,
WordPress allows you to filter the posts listed when you `GET` the posts endpoint. It's nice to be
able to filter on all of the possible combinations of fields that a post specifies, and WordPress
allows you to do this. However, the way that WordPress handles this filtering presents a problem for
the intrepid Haskell programmer.

Filtering is done when using the `GET` HTTP method, which, according to the HTTP/1.1 RFC, doesn't
have any semantics for a request body and should therefore ignore it when producing a response. As a
result, query parameters must be used to specify filter parameters. Out of the box, `servant`
handles query parameters by adding arguments of type `Maybe a` to the client function. In our case,
that would result in a function with over 20 arguments. This presents the same issues we've already
covered when talking about a type for posts --- we don't want to have to deal with tens of values
when we only care about a couple.

Given this is largely the same problem we've already solved with `dependent-map`, we can just roll
out the same solution. There's one more piece of the puzzle in this case though. `servant` doesn't
know how to turn a `DMap` into a list of query parameters. However, `servant` _does_ allow us to
extend its capabilities by instancing the relevant type classes. Thanks `servant`!

```haskell
data QueryParamMap (key :: * -> *) (f :: * -> *)

instance (ToQueryParamKeyValues key f, HasClient api) => HasClient (QueryParamMap key f :> api) where
  type Client (QueryParamMap key f :> api) = DMap key f -> Client api

  clientWithRoute Proxy req dm =
    let
      addPair (k, v) = appendToQueryString k (Just v)
      f ka fa req' = foldr addPair req' $ toQueryParamKeyValues ka fa
    in
      clientWithRoute (Proxy :: Proxy api) $ DM.foldrWithKey f req dm
```

The code above gives us two things: a type constructor to use in `servant` APIs that signals the use
of a `DMap` for query parameters, and an instance for `HasClient` that tells `servant` how to
deal with `QueryParamMap`s in clients. That is, what is the type of the client function, and how
should the `DMap` given as an argument modify the request being made.

