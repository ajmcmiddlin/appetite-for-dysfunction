# Props for WordPress

## Abstract

Property based state machine testing is an unreasonably effective technique for finding bugs and providing minimal test cases to reproduce those bugs. The Haskell library `hedgehog` provides excellent support for this style of testing. While `hedgehog` is a Haskell library, it is by no means limited to testing Haskell code. In fact, coupled with some other libraries and tools, it can be used to great effect when testing almost any software. You don't have to take my word for it though. In this talk I will provide a tour of the tools and techniques I've actually used to test some decidedly not-functional software: WordPress.

WordPress, for anyone who didn't setup a blog in the 2000s, is a blogging platform implemented using PHP and MySQL. It's REST API is under specified and makes extensive use of the dynamic nature of JSON -- with fields in each object appearing and disappearing across request types. At first this might not appear to be a good fit for decidedly typed, functional language. Appearances can be deceiving. Haskell's rich ecosystem gives us:

 - `hedgehog` to perform state machine testing, including concurrency tests.
 - `servant` to generate client request functions given a specification of the REST API.
 - `dependent-map` to model the highly dynamic JSON objects.

## Bio

Andrew is a functional programming engineer in the Queensland Functional Programming Lab at Data61. Earlier in his career I worked across a range of domains using conventional imperative programming languages, growing more and more frustrated with the difficulties of reasoning in a stateful and often untyped world. At some point he encountered Clojure and the ideas of immutability and referential transparency, which quickly changed the way he thought about solving problems. From there he discovered the benefits of a good type system and has been focused on using Haskell to solve problems ever since.

