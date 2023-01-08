# Refactor 4: MTL classes

- [Refactor 4: MTL classes](#refactor-4-mtl-classes)
  - [Step 1: mtl constraints](#step-1-mtl-constraints)
    - [Task 1.0: Small change on Event](#task-10-small-change-on-event)
    - [Task 1.1: Create a newtype with the capabilities you want](#task-11-create-a-newtype-with-the-capabilities-you-want)
    - [Task 1.2: Abstract your functions](#task-12-abstract-your-functions)
  - [Step 2: Glue Together RenderState and GameState](#step-2-glue-together-renderstate-and-gamestate)
    - [Task 2.1: Define `Has` type classes](#task-21-define-has-type-classes)
    - [Task 2.2: Modify function `move` and `render`](#task-22-modify-function-move-and-render)
    - [Task 2.3: Create a new module and move `gameloop` there](#task-23-create-a-new-module-and-move-gameloop-there)

In this refactor you'll learn about `mtl`-style and how it improves code. First, you need to do one test: In module `GameState.hs` change `GameStep` type for the one below as see what happens when you build

```haskell
type GameStep a = StateT GameState (Reader BoardInfo) a -- import the necessary stuff
```

Probably many errors are jumping into your editor! As you can see, all we've done is swapping the order of `Reader` and `State` monad. This is not a semantic change (changing the order of monads can lead to semantic changes in general, but not for this two monads in particular). In other words, our functions should do exactly the same thing as before, modifying the state as expected and reading the configuration as expected too. So, where do all these errors come from? If you look carefully, it is all related to `lift` function.

The reason is that we are working with `GameStep` type, which is a concrete type. It is a specific monad, so we are bound to its implementation, therefore changing `ReaderT BoardInfo (State GameState) a` by `StateT GameState (Reader BoardInfo) a`, does change the underlying monad and propagates that through the code. Instead we'd like to work with an abstract version of this: we would like our functions to run in any monad with state `GameState` and with read-only environment `BoardInfo`. We don't care which monad it is. Below there are some examples of monads with these capabilities

```haskell
type GameStep1      a = ReaderT BoardInfo (State GameState)                a -- has access to GameState and BoardInfo as updatable state and read-only environ
type GameStep2      a = StateT  GameState (Reader BoardInfo)               a -- same as above
type GameStepIO     a = ReaderT BoardInfo (StateT GameState IO)            a -- same as above, but can do IO
type GameStepWriter a = ReaderT BoardInfo (StateT GameState (Writer Text)) a -- same as above, but can write-only into a Text, for example, usefull for logging
type GameStepError  a = ReaderT BoardInfo (StateT GameState (Except Text)) a -- same as above, but aborts execution on error.
```

As you can see, when we stack transformers one on top of each other, we end up with different monads with different capabilities. This is when `mtl` comes to rescue. This library defines typeclasses expressing exactly what we want: _any monad with having a mutable-like state of type `GameState`_ or _any monad with having a read-only environment of type `BoardInfo`_. These classes are called `MonadReader` and `MonadState` and they are indexed by the read and state types. Read the [documentation](https://hackage.haskell.org/package/mtl-2.2.2) of `mtl` library to get use to it.

This refactor is divided in two Steps:

- Step 1: Use `mtl` constraints to make your code more abstract
  - Task 1.0: Adequate the `Event` type for better readability
  - Task 1.1: Define newtypes for `GameStep` and `RenderStep` and write monad instances
  - Task 1.2: Rewrite all functions to work on abstract monads.
- Step 2: Glue the `RenderState` and the `GameState` into a single app
  - Task 2.1: Rewrite functions to work on `Has`-like type classes
  - Taks 2.2: Change the type of `render` and `move`
  - Task 2.3: Write an `App` with all you need. Redefine the `gameloop`

## Step 1: mtl constraints

This step, you will move from `Control.Monad.Trans.XXXX` to `Control.Monad.XXXX`. These modules belong to different packages `transformers` and `mtl`. The later depends on the former. The module hierarchy looks like the follow:

- Modules `Control.Monad.Trans.XXXX` have only concrete implementations of monad transformers. For example: module `Control.Monad.Trans.Reader` contains `ReaderT` type
- Modules `Control.Monad.XXXX` have concrete _and_ abstract implementations. For example: module `Control.Monad.Reader` contains type `ReaderT` and type class `MonadReader`

In general, you want to use modules from `mtl` package, hence `Control.Monad.XXXX` because it is more abstract. And abstraction is _almost always_ a good idea.

### Task 1.0: Small change on Event

This task is unrelated to `mtl` or monads, but it is convenient at this point of development.

- Move `Event` type from `EventQueue.hs` to `GameState.hs`
- Fix `import` staments in all files that aren't compiling and re-build the project
- Modify function `move` in `GameState.hs` :
  - Add one extra `Event` parameter so now it should have type `move :: Event -> BoardInfo -> GameState -> ([Board.RenderMessage], GameState)`
  - The logic under the new parameter is currently in `Main.gameloop` (when pattern matching on `event`). That logic should be moved to `move` function.

After this change, does the `gameloop` function look more similar to the diagram shown in the README.md?

### Task 1.1: Create a newtype with the capabilities you want

This task will expose you to a common pattern which is to define your monad stack within a newtype wrapper which implements only the features you'd like.

(hint: This exercise looks difficult, but is all about wrapping and unwrapping the newtype)

- Modify `GameStep` from `ReaderT BoardInfo (State GameState) a` to `newtype GameStep m a = GameStep {runGameStep :: ReaderT BoardInfo (StateT GameState m) a}` (using the language extension `FlexibleContexts`)
- Implement `Functor` instance for the newtype
- Implement `Applicative` instance for the newtype
- Implement `Monad` instance for the newtype
- Implement `MonadState GameState` instance for the newtype
- Implement `MonadReader BoardInfo` instance for the newtype

Essentially, you are telling the compiler that your `GameStep` type has these capabilities. Probably you've found this repetitive. And it is!!, actually the compiler can do this for you.

- In `RenderState.hs` define `newtype RenderStep m a = RenderStep {runRenderStep :: ReaderT BoardInfo (StateT RenderState m) a}`
- Implement `Functor`, `Applicative`, `Monad`, `MonadState GameState`, `MonadReader BoardInfo` for `RenderStep` using `GeneralizedNewtypeDeriving`.

### Task 1.2: Abstract your functions

- In every function using `GameStep` or `RenderStep` you should use `mtl` constraints. For example: if you have a function `fun :: GameStep a` now it will have type `fun :: (MonadState GameState, MonadReader BoardInfo) => m a`. This means _Don't use GameStep directly, but any monad with State and Read envs_

- The functions you should change are:
  - `makeRandomPoint` from `GameState.hs`
  - `newApple` from `GameState.hs`
  - `extendSnake` from `GameState.hs`
  - `displaceSnake` from `GameState.hs`
  - `step` from `GameState.hs`
  - `updateRenderState` from `RenderState.hs`
  - `updateMessages` from `RenderState.hs`
  - `renderStep` from `RenderState.hs`
- Change function `move` to have type `Monad m => Event -> BoardInfo -> GameState -> m ([Board.RenderMessage], GameState)`
- Change function `render` to have type `Monad m => [RenderMessage] -> BoardInfo -> RenderState -> m (Builder, RenderState)`
- fix compiler errors.

## Step 2: Glue Together RenderState and GameState

The current state of things is satisfactory, as we can change the monad stack and our code will still compile and run with minimum changes. If you don't believe me, try to swap `Reader` and `State` in monads  `GameStep` or `RenderStep`. Now you should not have the problem of all your functions not compiling.

Nevertheless, we have a little problem still. Take a look to `Main.gameloop` function. This function is very error prone, because we need to take care of manually passing updated state to the next execution of the loop. Also, it is on charge of pulling event, updating state and putting the render into the console. Just to reminder. This is how our architecture looks like:

![snake-fury arquitecture](../../snake-fury/assets/snake_arquitecture.png)

It makes little sense that the main loop explicitly has to care about the updated states. This should be done implicitly when calling `move` and `render` during the `gameloop`. In other words, we would like the implementation of `gameloop` to look like:

```haskell
-- Ideal implementation og gameloop.
gameloop :: (MonadState GameState, MonadState RenderState, MonadReader BoardInfo) => IO ()
gameloop = forever $ do
    waitTime
    event <- readEvent queue
    messages <- move event
    render messages
```

This is impossible to define in `Haskell` with `mtl`-style code. Think what would `get` return? the `GameState` or the `RenderState`? By the way `mtl` classes are implemented, this code can't be implemented. What we can do is to define a common state called `AppState` which has both the `GameState` and the `RenderState`. But, now we would run into a problem. Our functions are defined for `MonadState GameState` and `MonadState RenderState`.

### Task 2.1: Define `Has` type classes

- Copy this typeclass in `GameState.hs`

```haskell
-- This is the class of type which you can access a field of type GameState.
class HasGameState state where
  getGameState :: state -> GameState
  setGameState :: state -> GameState -> state
```

- Copy this typeclass in `RenderState.hs`

```haskell
-- This is the class of type which you can access a field of type RenderState.
class HasRenderState state where
  getRenderState :: state -> RenderState
  setRenderState :: state -> RenderState -> state
```

- Modify all functions with the constraint `MonadState GameState m` to have this constraint `MonadState s m, HasGameState s`
- Modify all functions with the constraint `MonadState RenderState m` to have this constraint `MonadState s m, HasRenderState s`

Do you understand what are we doing? Instead of saying _function f works on a RenderState_, we are saying _function f works on some state which has access to a RenderState_.

### Task 2.2: Modify function `move` and `render`

- modify function `move` to have type `move :: (MonadReader BoardInfo m, MonadState state m, HasGameState state) => Event -> m [Board.RenderMessage]`
  - notice that `move` should not call `runReaderT` or `runState` any more.
- modify function `render` to have type `render :: (MonadReader BoardInfo m, MonadState state m, HasRenderState state, MonadIO m) => [RenderMessage] -> m ()`.
  - Notice that `render` should not call `runReaderT` or `runState` any more.
  - `render` has a new functionality as you can see now it has a `MonadIO` constraint. That means, that we expect `render` to do something in the `IO` monad.
  - `render` should call `putStr` and `B.hPutBuilder` the same way `Main.gameloop` now does.

### Task 2.3: Create a new module and move `gameloop` there

- Change `Main.hs` by removing `gameloop`. This should be the new `Main.hs`. Copy paste it.

```haskell
{-# LANGUAGE NumericUnderscores #-}

module Main where

import Control.Concurrent (
  forkIO,
 )
import EventQueue (
  writeUserInput
 )
import Initialization (gameInitialization)
import System.Environment (getArgs)
import System.IO (BufferMode (NoBuffering), hSetBinaryMode, hSetBuffering, hSetEcho, stdin, stdout)
import App (AppState (AppState), run)

-- | main.
main :: IO ()
main = do
  -- enable reading key strokes
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False

  hSetBuffering stdout NoBuffering
  hSetBinaryMode stdout True

  -- Game Initializacion
  [h, w, fps] <- fmap read <$> getArgs
  let timeSpeed = 1_000_000 `div` fps -- One second is 1_000_000 microseconds, which is the unit used by GHC internally.
  (binf, gameState, renderState, eventQueue) <- gameInitialization h w timeSpeed

  -- Game Loop. We run two different threads, one for the gameloop (main) and one for user inputs.
  _ <- forkIO $ writeUserInput eventQueue
  let initialState = AppState gameState renderState
  run binf initialState eventQueue
```

- Create a new module call `App.hs` and copy paste the template below

```haskell
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App where
import GameState (GameState, move, HasGameState (getGameState, setGameState))
import RenderState (RenderState (score), BoardInfo, render, HasRenderState (getRenderState, setRenderState))
import Control.Monad.Reader (MonadReader (ask), asks, ReaderT (runReaderT))
import Control.Monad.State (MonadState (get), gets, StateT (runStateT), evalStateT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import EventQueue (EventQueue, readEvent, setSpeed)
import Control.Concurrent (threadDelay)
import Control.Monad (unless)

-- This is the new state, which glue together Game and Render states.
data AppState = AppState GameState RenderState

-- Our application is a readerT with and AppState and IO capabilities.
newtype App m a = App {runApp :: ReaderT BoardInfo (StateT AppState m) a}
  deriving (Functor , Applicative, Monad, MonadState AppState, MonadReader BoardInfo, MonadIO)

-- We need to make AppState and instance of HasGameState so we can use it with functions from `GameState.hs`
instance HasGameState AppState where
  getGameState = undefined
  setGameState = undefined

-- We need to make AppState and instance of HasRenderState so we can use it with functions from `RenderState.hs`
instance HasRenderState AppState where
  getRenderState = undefined
  setRenderState = undefined

-- This function should read an event from the queue, move the snake and render.
-- Notice the constraints.
gameStep :: (MonadReader BoardInfo m, MonadState state m, HasGameState state, HasRenderState state, MonadIO m) => EventQueue -> m ()
gameStep = undefined -- try implement this function without do-notation. Using just >>= operator.

-- The gameloop is easy as hell. Read the score and wait the requiered time. Then run the gameStep.
-- This function is implemented for easiness.
gameloop :: (MonadReader BoardInfo m, MonadState state m, HasGameState state, HasRenderState state, MonadIO m) => EventQueue -> m ()
gameloop queue = do
  s <- gets (score . getRenderState)
  new_speed <- liftIO $ setSpeed s queue
  liftIO $ threadDelay new_speed
  gameStep queue
  game_over <- gets (gameOver . getRenderState)
  unless game_over (gameloop queue)

-- This function runs the gameloop.
run :: BoardInfo -> AppState -> EventQueue -> IO ()
run binf app queue = gameloop queue `evalStateT` app `runReaderT` binf
```
