# Appetite for dysfunction

This talk aims to do three things:

 1. Introduce state machine testing.
 2. Demonstrate that Haskell and [hedgehog](https://github.com/hedgehogqa/haskell-hedgehog) can be
    used to test applications that aren't written in Haskell or use functional programming.
 3. Introduce some tools and techniques that make testing non-Haskell applications easier.
 
WordPress is used to demonstrate points 2 and 3.

## Versions

The version you most likely want is the one given at [C◦mp◦se ::
Melbourne](http://www.composeconference.org/) in 2018. It's tagged as
[compose](https://github.com/qfpl/appetite-for-dysfunction/tree/compose).

An earlier version was given at [BFPG](https://www.meetup.com/Brisbane-Functional-Programming-Group).
This is considered a rough draft, but may be found at tag
[bfpg](https://github.com/qfpl/appetite-for-dysfunction/tree/bfpg).

## Building

To build the talk, run `nix-build` in the project root. To view the build, open `result/index.html` in your browser.
