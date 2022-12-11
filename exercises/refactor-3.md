# Refactor 3: Monad Transformers

- [Refactor 3: Monad Transformers](#refactor-3-monad-transformers)
  - [Step 1: Monad Stacks](#step-1-monad-stacks)
    - [Task 1.1: Did you spot the subtle pattern?! That's the Reader Monad](#task-11-did-you-spot-the-subtle-pattern-thats-the-reader-monad)
    - [Task 1.2: Refactor `RenderState`](#task-12-refactor-renderstate)
  - [Step 2 (optional): The IO monad and async code](#step-2-optional-the-io-monad-and-async-code)
    - [Task 2.1](#task-21)
      - [template](#template)

In this refactor you'll work on the `GameState` and `RenderState` modules. From this point on, you'll feel you are overengineering your code. This is a difficult topic. I'd say that simple is better than complex, so the implementation as in refactor 2 is a good balance between complexity and simplicity. Nevertheless, In order to learn deeper about monads we need to move into the topic of monad transformers and `mtl` style. If you feel your code is getting less readable, keep in mind this kind of patterns are suited for more evolved software, so it may not fit well in this situation. It won't be that bad, though.

This refactor has two steps, which one is optional.

- Step 1: Introduce the `ReaderT` transformer and monad stacks in general
  - Taks 1.1: use the `ReaderT` transformer to refactor a common pattern
  - Taks 1.2: Apply the same stack to the `RenderState` module
- Step 2 (Optional): Work in the `IO` monad and asynchronous code.
  - Task 2.1: Re-implement your-self module EventQueue

## Step 1: Monad Stacks

As you can guess, `State` monad is just an example of monad. In Haskell we have monads for error handling (`Maybe` and `Either e`), for doing input/output (`IO`), for parallelism (`Eval`), for read-only / write-only / read-write state (resp. `Reader`, `Write`, `State`), etc...

The problem with monads comes when you want to compose them. Meaning, what happend if I want to do input/output and having a read-only environment, and a read-write state? I would need to use three different monads, but what type is that? As we've seen in the previous refactor we have `State` for managing the read-write state. If I want to do all those effects, does it exist a `IOReaderState` data type? There exist something very similar: monad transformers.

### Task 1.1: Did you spot the subtle pattern?! That's the Reader Monad

Did you notice that many functions in the `GameState` module uses the `BoardInfo` datatype? Of course!, the information about the size of the board is crucial all along the software. The `Reader` monad essentially abstract this pattern. When you have a function from some environment to some result `function :: env -> a`, that can be express as `Reader env a`. This is useful when you have multiple functions taking the same `env`. For example

Without `Reader` monad

```haskell
f :: env -> a
g :: env -> b
h :: env -> c

j :: env -> (a, b, c) 
j e = (f e, g e, h e)

```

With `Reader` monad

```haskell
f :: Reader env a
g :: Reader env b
h :: Reader env c

j :: Reader env (a, b, c) 
j = do
  a <- f
  b <- g
  c <- h
  pure (a, b, c)
```

You may think we haven't won anything. That's kind of true, but intention matters here. In the second example, it is very clear from the type signature that `env` is an important datatype in our software. It is used for configuration and as a read-only environment. Let's refactor `GameStep` to introduce the `ReaderT` transformer.

- Introduce the following `imports`
  - `import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask, runReader)`
  - `import Control.Monad.Trans.Class ( MonadTrans(lift) )`
- Change the `GameStep` to look like below. Try to understand what you are doing. (tip: why `ReaderT` but not `StateT`. Go to transformers library documentation and read more about them)
  - `type GameStep a = ReaderT BoardInfo (State GameState) a`
- Change all functions with signature `BoardInfo -> GameStep a` to `GameStep a`. Use function `ask` to get the read only env. These are:
  - `makeRandomPoint :: GameStep Point`
  - `newApple :: GameStep Point`
  - `extendSnake ::  Point -> GameStep DeltaBoard`
  - `displaceSnake :: Point -> GameStep DeltaBoard`
  - `step :: GameStep [Board.RenderMessage]`
- Notice that you need to use `lift` when using the `State` monad. (Why is that?)
- Modify function `move` to use `runReaderT`

### Task 1.2: Refactor `RenderState`

For the sake of practice, let's refactor `RenderState` module. Again, this feels like a total overengineering. Keep in mind this refactor will break something in the `main` module. This is done on purpose.

- Import the following:
  - `import Control.Monad.Trans.Reader (ReaderT (runReaderT), asks, ask)`
  - `import Control.Monad.Trans.State.Strict (State, put, get, runState, evalState)`
  - `import Control.Monad.Trans (lift)`
  - More import will be necessary, but you should find out yourself.
- Create a new datatype `type RenderStep a = ReaderT BoardInfo (State RenderState) a`
- Change these function types and modify their body accordingly:
  - `updateRenderState :: RenderMessage -> RenderStep ()`
  - `updateMessages :: [RenderMessage] -> RenderStep ()`
- Rename function `render` to `renderStep` with type `renderStep :: [RenderMessage] -> RenderStep Builder`. This function should update the `RenderState` w.r.t the received message, and produce a `Builder` of the updated state.
- Create a function `render` with type `render :: [RenderMessage] -> BoardInfo -> RenderState ->  (Builder, RenderState)`, which runs the monad stack producing the builder associated to the `RenderState` and the `RenderState` itself.
- Function `main` should be broken now. Make it work! Do you think `main` looks better now? By "better" I mean: Does it looks like code is more declarative?

## Step 2 (optional): The IO monad and async code

This step consists in re-implementing the module `EventQueue.hs`. This module implements an asynchronous bounded channel in which we push our key strokes. Working in the `IO` is not more difficult than working in the `State` or `Reader` monad. The only difference is that `IO` monad can throw exceptions at any time for any reason.

Any program which actually does something has to do input/ouput (reading files, putting text to console, etc...). The problem with the real world is that it is a mess, therefore if we have a function interacting with the real world, we should annotate it using `IO` monad.

### Task 2.1

- Make a backup copy of `EventQueue.hs`
- Create a new `EventQueue.hs` file and paste the template below
- Fill the template and check the game runs as always

#### template

```haskell
{-# LANGUAGE TypeApplications #-}

{- |
This module handle the external events of the game. That is: the user inputs and the time.
-}
module EventQueue where

import Control.Concurrent (
  MVar,
  readMVar,
  swapMVar,
 )
import Control.Concurrent.BoundedChan (
  BoundedChan,
  tryReadChan,
  tryWriteChan,
 )
import GameState (Movement (..))
import qualified GameState as Snake
import System.IO (hReady, stdin)

-- | The are two kind of events, a `ClockEvent`, representing movement which is not force by the user input, and `UserEvent` which is the opposite.
data Event = Tick | UserEvent Snake.Movement

-- | the `UserInputQueue` is an asynchronous bounded channel which contains snake movements. This channel is feeded by key strokes
type UserInputQueue = BoundedChan Snake.Movement

-- | The `EventQueue` has a `UserInputQueue` and the global speed of consumption (as a mutable reference) and the initial speed of the game.
data EventQueue = EventQueue
  { -- | An asynchronous queue of movements the snake needs to do.
    userInput :: UserInputQueue
  , -- | A mutable reference to a Int. This is used for modifying the speed of the game as we play
    currentSpeed :: MVar Int
  , -- | The initial speed
    initialSpeed :: Int
  }

-- | Given the current score and the initial speed, calculates the new speed.
--   The speed is increased by 10% every 10 points, up to 50 points.
calculateSpeed :: Int -> Int -> Int
calculateSpeed score initialSpeed = undefined

{- | Given the current score and the event queue, updates the new speed and returns it.
   This action is mutable, therefore must be run in the IO mondad
-}
setSpeed :: Int -> EventQueue -> IO Int
setSpeed score event_queue = undefined


-- In StackOverflow we trust.
-- This function reads the key strokes as a String.
-- The arrow keys correspond to the following strings
-- "\ESC[A" -> Up Arrow
-- "\ESC[D" -> Right Arrow
-- "\ESC[C" -> Left Arrow
-- "\ESC[B" -> Down Arrow
-- therefore the following code: 
--     k <- getKey
--     print $ k == "\ESC[B"
-- will print True when Down arrow is pressed
getKey :: IO String
getKey = reverse <$> getKey' ""
 where
  getKey' chars = do
    char <- getChar
    more <- hReady stdin
    (if more then getKey' else return) (char : chars)

{- | This function translates key strokes to movements and push then into the queue.
 The player is free to push keys as fast a he/she can but the userqueue is bounded,
 meaning that if we push a movement to a filled queue it gets discarded.
 This is intented for the game play, If we press keys faster than the game speed
 they will be enqueued and pushed into the game with delay.

Check getKey function's comment for a hint

-}
writeUserInput :: EventQueue -> IO ()
writeUserInput event_queue = undefined

-- | Read the EventQueue and generates an Event to pass to the user logic.
-- It should pass an UserEvent if the queue is not empty, otherwise a Tick
readEvent :: EventQueue -> IO Event
readEvent event_queue = undefined

```
