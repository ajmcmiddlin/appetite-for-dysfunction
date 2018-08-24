# Intro

## What and why?

::: notes
 - Property based state machine testing using the Haskell library hedgehog
 - Using the technique to test software not written in Haskell
 - Technical talk earlier this year at Lambda Jam
 - Question: can it test stuff not written in Haskell?
 - Knew it could, but wanted to write the code
 - Interested in the use case of testing big, legacy project that's not even functional
 - WordPress
 - Talk has 3 related goals
   + Intro to state machine testing
   + Show that even if you can't write your app in Haskell, you can get value out of using it to test
   + Show some techniques to test non-haskell/non-FP projects
:::

## Outline

- WordPress
- Property based testing
- State machine testing
- NixOps
- `dependent-map`
- `servant` to talk web
- Putting it together

