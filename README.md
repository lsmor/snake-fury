# snake-fury

[![Open in Gitpod](https://gitpod.io/button/open-in-gitpod.svg)](https://gitpod.io/#https://github.com/lsmor/snake-fury)

Welcome to snake-fury: the Haskell challenge for beginners. This challenge aims to provide a learning path for people willing to learn Haskell further than basic recursion exercises by implementing the snake game in Haskell. The pedagogical approach followed by snake-fury is based in two legs:

- snake-fury isn't a tutorial, but a challenge.
- snake-fury is focused on learn by refactoring, not by example.

The first legs means that you'll be asked to implement some functions/algorithms but it is expected that the challenger will be unable to implement them without some research on hackage documentation, blogs, youtube videos, etc... Guidelines will be given to help the challenger, nevetheless an important skill when learning Haskell is to be able to search, read and understand documentation which is (oftenly, but not always) more complex and less accesible than other programming languages. 

The second legs is even more interesting. Haskell is notoriously known by its difficulty and by the popularization of the holy triad: Functor - Applicative - Monad. The are plenty of tutorials showing examples, hundred of thousand of lines triying to make them accesible and newcomer friendly... But with all due respect, It seems like they all fail to explain: "Why monads? Why not other less mathematical abstraction? Why not classic OOP patterns?". The approach given by snake-fury, is to make the same application twice... it sounds crazy, but the idea goes like this: You'll implement da "pure" version of the snake game. Not monads, no functors, no bullsh\*t. Then you will refactor the core application logic using the state and reader monads. Then you'll be ask to abstract your code, to use `mtl` classes to make your code less dependant on the concrete implementation.


Below There is a dramatization of the Haskell learning curve. This challenge aims to be helpfull companion from the newby slope to the temple of oblivion... but be aware, nothing will safe you from the temptation of abandom. Hopefully, you'll be able to climb up to the temple and spread the lambdas  


![dramatization of Haskell's learning curve](./assets/Haskell_learning_curve.png)


## Current status

**This is a work in progress. The last commit is not reliable. If you want to compile, use one of the tags** . 

The idea of this repo is to provide a learning path for Haskell via writing a simple but complex enough program: The Snake Game in the Console. The approach is the following:

The same game is written twice. First, it is written using no monads (read below), all game logic is done with pure functions. Then, the code is refactored to use monads. The idea of this approach is to light away the darkness around monads: The challenger has the opportunity to see with its own eyes how monads lead to cleaner and more ergonomic code.

Exercises ~~are~~ will be tagged with a different version each `v0.1-exercise`, `v0.1-solution`, `v0.2-exercise`, etc... The challenger tries to solve them in their own way and then checks its own solution with the proposed one.

for running the game, checkout the latest aviable tag and compile. Example: `git checkout v1.4.0-improving-performance & stack run -- 40 60 200000` runs the snake game in the console with a 40x60 board and every step in the game happens every fifth of a second. (Notice that stack will download the compiler and libraries, etc... If you have the same version installed in your system, it'll be quick, otherwise go and prepare a coffee)

### note about not using monads
> By that I mean, not using do notation nor functor/applicative/monads combinators like `liftA2`, `fmap`, `>>=`, etc...
> 
> Obviously, The IO and the asynchronous part of the code is provided and the challenger is not expected to solve it.

At the moment there are no exercises prepared, but if you are interested there are some tags already prepared to help my future me to design the challenge. run `git tag -l` to see a list of all the tags available

**v0.XX are the versions implemented with only pure functions**
- v0.1 is the minimum viable product. Essentialy a very inefficient version of snake but enough to spit the board to the console. 
- v0.2, v0.3 and v0.4 are little refactoring of v0.1 to make it more efficient (essentially, getting rid of `String` in favour of `Builder`, because you shouldn't use `String` ever!)
- v0.5 is just a side note. Uses Pattern Synonyms to achive better performance.

**v1.XX are the versions using monads**
- v1.1 uses the State monad to refactor the game logic step, leading to clearer version of the same logic (arguably, snake is simple enough to use just pure functions, but you are here to learn monads!). 
- v1.2, v1.3, v1.4 uses the so called [three layer of Haskell pattern](https://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html) to ~~overengineer~~ make the application not dependent on the concrete implementation, but on abstractions. 
- v1.5 is the same as v0.5. If you have reached here, you might be interested in cheking it out!

This is how the game looks in the terminal:
![](./assets/snake-tui.gif)

**v2.XX are the versions using monads with SDL**
- whereas versions 0 and 1 render the snake game simply by flushing out a bytestring into the console, the version 2 uses sdl to provide a graphical interface to the game. The key point of this steps is realize how abstractions like `mtl` or `HasXXX` type classes lead to better code reusability.

This is how the game looks as a gui:
![](./assets/snake-gui.gif)

## What this repo is *NOT*
This is not a Haskell tutorial. The challenger is expected to know (at least a little) basic Haskell: syntax, recursion, algebraic data types and records. The challenger should take care about finding learning resources for those parts of the code he/she doesn't understand. Of course, it isn't mandatory to know them perfectly, and the challenge is intended to be difficult if you are a beginner... otherwise, I wouldn't be a challenge isn't it?



## Structure (TODO)


- You'll be asked to implement some functionality.


This challenge is divided in three parts:

- Part One 


## Arquitecture

The general arquitecture of the software is the following:
- There are three major components: 
  - An Event Queue: It keeps a queue of the following events to happen in the game based on the user keyboard input
  - A Game State: Is the logic state. It keeps track on the snake body, the current apple, the direction of movement, etc...
  - A Render State: Is the game board. Instead of building up the board from the GameState, we keep an array in memory and modify it as convenient
- Each compoment send a message to the next one in the following order: (user keyboard) -> EventQueue -> GameState -> RenderState -> (render device)
- we have two threads. 
    - The sencondary thread is continuously reading from users keyboard and pushing the key strokes into an asynchronous EventQueue
    - The main thread reads at steady time from the EventQueue, and based on what the user has pressed, it runs the game logic and prints the board in the console

Notice that two threads are necessary, since use can press keys faster than the game update. For example let say we run a frame each second and a half (normal speed in the snake game.), then a user is likely to press keys faster than that. If the key stroke are catch as the same speed the game runs, then many stroke will be lost. 

The following diagram helps to visualize

![Overview of the arquitecture](./assets/snake_arquitecture.png)


## Contributions
The current state is not good for contributions, since I am still defining the overall structure of the challenge.

