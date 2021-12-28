# Haskell's snake challenge for monads.

**This is a work in progress**. The idea of this repo is to provide a learning path for Haskell via writing a simple but complex enough program: The Snake Game in the Console. The approach is the following:

The same game is written twice. First, it is written using no monads (read below), all game logic is done with pure functions. Then, the code is refactored to use monads. The idea of this approach is to light away the darkness around monads: The challenger has the opportunity to see with its own eyes how monads lead to cleaner and more ergonomic code.

Exercises are tagged with a different version each `v0.1-exercise`, `v0.1-solution`, `v0.2-exercise`, etc... The challenger tries to solve them in their own way and then checks its own solution with the proposed one.

### note about not using monads
> By that I mean, not using do notation nor functor/applicative/monads combinators like `liftA2`, `fmap`, `>>=`, etc...
> 
> Obviously, The IO and the asynchronous part of the code is provided and the challenger is not expected to solve it.

## What this repo is *NOT*
This is not a Haskell tutorial. The challenger is expected to know (at least a little) basic Haskell: syntax, recursion, algebraic data types and records. The challenger should take care about finding learning resources for those parts of the code he/she doesn't understand. Of course, it isn't mandatory to know them perfectly, and the challenge is intended to be difficult if you are a beginner... otherwise, I wouldn't be a challenge isn't it?

## Contributions
The current state is not good for contributions, since I am still defining the overall structure of the challenge.

## Current status
The repo at HEAD executes the snake game, just write `stack run -- <height> <width> <speed microsecs>`, for example `stack run -- 40 60 200000` runs the snake game in the console with a 40x60 board and every step in the game happens every fifth of a second.
