# Haskell's snake challenge.

**This is a work in progress**. The idea of this repo is to provide a learning path for Haskell via writing a simple but complex enough program: The Snake Game in the Console. The approach is the following:

The same game is written twice. First, it is written using no monads (read below), all game logic is done with pure functions. Then, the code is refactored to use monads. The idea of this approach is to light away the darkness around monads: The challenger has the opportunity to see with its own eyes how monads lead to cleaner and more ergonomic code.

Exercises ~~are~~ will be tagged with a different version each `v0.1-exercise`, `v0.1-solution`, `v0.2-exercise`, etc... The challenger tries to solve them in their own way and then checks its own solution with the proposed one.

for running the game, checkout the latest aviable tag and compile. Example: `git checkout v1.4.0-improving-performance & stack run -- 40 60 200000` runs the snake game in the console with a 40x60 board and every step in the game happens every fifth of a second. (Notice that stack will download the compiler and libraries, etc... If you have the same version installed in your system, it'll be quick, otherwise go and prepare a coffee)

### note about not using monads
> By that I mean, not using do notation nor functor/applicative/monads combinators like `liftA2`, `fmap`, `>>=`, etc...
> 
> Obviously, The IO and the asynchronous part of the code is provided and the challenger is not expected to solve it.


## Current status
At the moment there are no exercises prepared, but if you are interested there are some tags already prepared to help my future me to design the challenge. run `git tag -l` to see a list of all the tags available

**v0.XX are the versions implemented with only pure functions**
- v0.1 is the minimum viable product. Essentialy a very inefficient version of snake but enough to spit the board to the console. 
- v0.2, v0.3 and v0.4 are little refactoring of v0.1 to make it more efficient (essentially, getting rid of `String` in favour of `Builder`, because you shouldn't use `String` ever!)
- v0.5 is just a side note. Uses Pattern Synonyms to achive better performance.

**v1.XX are the versions using monads**
- v1.1 uses the State monad to refactor the game logic step, leading to clearer version of the same logic (arguably, snake is simple enough to use just pure functions, but you are here to learn monads!). 
- v1.2, v1.3, v1.4 uses the so called [three layer of Haskell pattern](https://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html) to ~~overengineer~~ make the application not dependent on the concrete implementation, but to abstractions. 
- v1.5 is the same as v0.5. If you have reached here, you might be interested in cheking it out!

## What this repo is *NOT*
This is not a Haskell tutorial. The challenger is expected to know (at least a little) basic Haskell: syntax, recursion, algebraic data types and records. The challenger should take care about finding learning resources for those parts of the code he/she doesn't understand. Of course, it isn't mandatory to know them perfectly, and the challenge is intended to be difficult if you are a beginner... otherwise, I wouldn't be a challenge isn't it?

## Contributions
The current state is not good for contributions, since I am still defining the overall structure of the challenge.

