- [snake-fury](#snake-fury)
  - [Introduction](#introduction)
    - [note about not using monads](#note-about-not-using-monads)
  - [Start coding](#start-coding)
  - [Building and Running](#building-and-running)
  - [Arquitecture](#arquitecture)
  - [Solution.](#solution)
  - [Set up a development environment](#set-up-a-development-environment)
  - [Contributions](#contributions)
# snake-fury

[![Open in Gitpod](https://gitpod.io/button/open-in-gitpod.svg)](https://gitpod.io/#https://github.com/lsmor/snake-fury)

## Introduction

**Welcome to snake-fury**: the Haskell challenge for beginners. This challenge aims to provide a learning path for people willing to learn Haskell further than basic recursion exercises by implementing the snake game in Haskell. Snake-fury's pedagogical approach is based on two legs:

- snake-fury isn't a tutorial but a challenge.
- snake-fury is focused on learning by refactoring, not by example.

The first leg means that you'll be asked to implement some functions/algorithms. It is expected that the challenger will be unable to implement them without some research on Hackage's documentation, blogs, youtube videos, etc. There will be guidelines to help the challenger. Nevertheless, an important skill when learning Haskell is to be able to search, read and understand the documentation that is (often, but not always) more complex and less accessible than other programming languages. 

The second leg is even more interesting. Haskell is notoriously known for its difficulty and the popularization of the holy triad: Functor - Applicative - Monad. The are plenty of tutorials showing examples and hundreds of thousands of lines trying to make them accessible and newcomer-friendly... But with all due respect, It seems they all fail to explain: "Why monads? Why not other less mathematical abstraction? Why not classic OOP patterns?". The approach given by snake-fury is to make the same application twice... it sounds crazy, but the idea goes like this: You'll implement a "pure" version of the snake game: No monads, no functors, no abstractions [[see below](#markdown-header-note-about-not-using-monads)]. Then you will refactor the core application logic using the state and reader monads. Then you'll be asked to abstract your code and to use `mtl` classes to make your code less dependent on the concrete implementation.


Below there is a dramatization of Haskell's learning curve. This challenge aims to be a helpful companion from the newby slope to the temple of oblivion... but be aware, nothing will save you from the temptation of abandon. Hopefully, you'll be able to climb up to the temple and spread the lambdas  


![dramatization of Haskell's learning curve](./assets/Haskell_learning_curve.png)

### note about not using monads
> By that I mean, not using do notation nor functor/applicative/monads combinators like `liftA2`, `fmap`, `>>=`, etc...
> 
> Obviously, The IO and the asynchronous part of the code are provided and the challenger is not expected to solve it.

## Start coding

Be sure you have a haskell developement environment [up and running](#markdown-hearder-set-up-a-development-environment). If you don't want to install Haskell's toolchain yourself, you can use gitpod to quickly jump into an online environment. 

Clone the code and move to `snake-fury-exercise1` branch . 

```bash
git clone https://github.com/lsmor/snake-fury.git
git checkout snake-fury-exercise
```

I'd recommend to create a branch for your solution (ex: `git checkout -b my-solution`) but you can use the given one. You'll find the following folder structure:

```
app
 |- Main.hs            # Here is the entrypoint of your application. This is implemented for you
src
 |- EventQueue.hs      # Here is the EventQueue. This is implemented for you
 |- GameState.hs       # here will go the logic of the game. You have to complete this file
 |- RenderState.hs     # here will go the data structure for rendering the game. You have to complete this file
 |- Initialization.hs  # some utility functions. You don't need to touch this file.
exercises
 |- refactor-1.md
 |- refactor-2.md
 |- refactor-3.md
 |- refactor-4.md
 |- refactor-5b.md
```
Each file correspond to each component in the system (and some utilities to keep code simpler). Be sure you read the about the (arquitecture)[#markdown-header-arquitecture] to understand why the code is splitted this way. Within files `GameState.hs` and `RenderState.hs` you have the exercises statements as comments. Notice that you'll need to implement as many auxiliar functions as you need to make it work. If you feel stuck you can check the [solution](#markdown-header-solution) I've implemented. It is totally fine if you implement a different one. 

Once you fill those two files you'll have a minimum viable product. Now it is time to improve it!. In the `exercises` folder you have exercises for refactors. Follow them one by one as enumerated. One of the ideas is you feel the power of the type system when refactoring. 

- **refactor-1**: you will move from you mvp to a full snake game with all the functionality
- **refactor-2**: you will refactor the logic of the game to use the state monad. Hopefully you'll find code much easier to read.
- **refactor-3**: you will refactor the logic of the game to use the reader + state monad stack. Probably you'll find code _harder_ read.
- **refactor-4**: You will refactor the code to use `mtl` like constraints.
- **refactor-5**: Finally, you'll refactor the code to be fully abstracted over the monad stack you use.

## Building and Running

This project is prepare for both `cabal` and `stack`. At the moment the only major difference between this tools is the building plan. If you are a newcommer, you'll find `stack` slightly easier to use, but cabal is also fairly easy. Even if you are using `stack` this repository wont allow it to install `ghc`, you need to have it installed in your `PATH`. Check [this](#set-up-a-development-environment) section for more information.

Notice the `main` branch is experimental and can change (even into a non-compilable code), so don't use it to build the project. For stable branches, use `solution-xxx`. To run the executable, you have to build first with either `cabal` or `stack` using. 

```bash
git checkout solution-xxx

# cabal users
cabal build

# stack users
stack build
```

To run it, you have to pass the height, width and fps (consider playing in a range from 5 to 15 fps)

```bash
# cabal users. 7 fps is a normal speed for snake game
cabal run snake-fury -- 10 10 7

# stack users. 7 fps is a normal speed for snake game
stack run snake-fury -- 10 10 7
```

> note: At the moment the `main` branch doesn't use frames per second but microseconds as input parameter. Hence `cabal run snake-fury -- 10 10 100000` will run at 10 fps. In the exercise branch, the input parameter is correct. So if you want to build from `main` be aware of this change


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

## Solution. 

Exercises ~~are~~ will be tagged with a different version each `v0.1-exercise`, `v0.1-solution`, `v0.2-exercise`, etc... The challenger tries to solve them in their own way and then checks its own solution with the proposed one.

for running the game, checkout the latest aviable tag and compile. Example: `git checkout v1.4.0-improving-performance & stack run -- 40 60 200000` runs the snake game in the console with a 40x60 board and every step in the game happens every fifth of a second. (Notice that stack will download the compiler and libraries, etc... If you have the same version installed in your system, it'll be quick, otherwise go and prepare a coffee)


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


## Set up a development environment

I strongly recommend to use [ghcup](https://www.haskell.org/ghcup/) to manage your Haskell toolchain. Download it using the script given in the page. Once you got it, install `ghc`, `cabal` and `hls`

## Contributions
The current state is not good for contributions, since I am still defining the overall structure of the challenge.

