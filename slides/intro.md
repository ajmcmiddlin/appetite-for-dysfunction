# Setup

- What and why.
- Quick overvie of state machine testing + example output.
- `nixops` to get a WordPress instance.
- `servant` to specify the WordPress API.
- `dependent-map` to deal with dynamic JSON without sacrificing type safety.
- Take away
   + Some good tools that might not be well known
   + Writing these tests really shake out how an API works
   + You really feel the pain of a poor (by my definition) API when you work with it in Haskell.

## What and why?

::: notes
 - Gave a talk on how state machine testing works with the Haskell library `hedgehog` at Lambda Jam
 - Technical overview of how it works and how to write tests
 - Questions: can it test stuff not written in Haskell?
 - Wanted to answer the question with code and make it challenging
    + One thing to say that it can, and another to show that it can
 - Chose WordPress because it's well known, and very much not functional
 - This talk is not so much about the details of state machine testing
    + More an experience report on the problems I solved to be able to test WordPress
 - Finally, these slides will be on qfpl.io soon, and they include links to all the code at the end.
:::

##

- Property based testing (refresher)
- State machine testing (high level)
- WordPress
- NixOps for functional deployments
- `dependent-map` for messy JSON formats
- Template haskell to scrap some boilerplate
- `servant` to talk web

