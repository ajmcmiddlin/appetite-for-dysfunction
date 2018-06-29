# Mad props for WordPress

## Abstract

Property based testing is an unreasonably effective technique for finding bugs and providing minimal test cases to reproduce those bugs. It achieves this by randomly generating inputs, providing those inputs to the software under test, and ensuring that the outputs uphold some desirable properties. Property based _state machine_ testing extends this technique to more easily test stateful software, using state machines to model the application while ensuring that the model and application continue to agree.

The Haskell library `hedgehog` provides excellent support for both of these styles of testing. Although `hedgehog` is a Haskell library, its usefulness is by no means limited to testing Haskell code. In fact, coupled with some other libraries and tools, it can be used to great effect when testing any software. Even something like... WordPress.

WordPress, for anyone who hasn't investigated setting up a blog in the last 15 years, is a blogging platform implemented using PHP and MySQL. WordPress is not written in Haskell or anything resembling a functional programming language, its REST API is under specified, and it makes extensive use of JSON objects with fields that may or may not appear depending on the context in which the object is used. At first this might not appear to be a good candidate for testing with a typed, functional language. Appearances can be deceiving. Haskell and its ecosystem are well equipped to tackle this problem, bringing with them all of their usual benefits. You don't have to take my word for it either -- I've written the code.

In this talk I will provide a tour of the tools and techniques I've employed to test WordPress. The main players are:

 - `hedgehog` to perform state machine testing, including concurrency tests.
 - `servant` to generate client request functions given a specification of the REST API.
 - `dependent-map` to model the dynamic JSON objects.
 
The final ingredient in this functional, testing cocktail is Nix. I will briefly show how NixOps and Nixpkgs provide a means to specify a test environment in a succinct and declarative manner, and then deploy that specification to a virtual machine for testing.

Attendees should leave the talk with a high-level understanding of state machine testing and the tools and techniques that can be employed to test complex APIs not written in Haskell. All source code will be made publicly available under an open source licence after the talk.

## Bio

Andrew is a functional programming engineer in the Queensland Functional Programming Lab at Data61. Earlier in his career he worked across a range of domains using conventional imperative programming languages, growing more and more frustrated with the difficulties of reasoning in a stateful and often untyped world. At some point he encountered Clojure and the ideas of immutability and referential transparency, which quickly changed the way he thought about solving problems. From there he discovered the benefits of a good type system and has been focused on using Haskell to solve problems ever since.

