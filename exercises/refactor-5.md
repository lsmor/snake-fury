# Refactor 5: Build your own classes

- [Refactor 5: Build your own classes](#refactor-5-build-your-own-classes)
  - [Step 1: abstract away your code](#step-1-abstract-away-your-code)
    - [Task 1.1: Abstract the environment](#task-11-abstract-the-environment)
    - [Task 1.2: Create a single all-in `App` type](#task-12-create-a-single-all-in-app-type)
  - [Congratulations](#congratulations)

We are finally in the last step of this project. We are going to create a fully abstracted version of our code. The way we are doing this is by creating classes for each component of our code. Let me recall the general architecture of the code.

![snake-fury arquitecture](../../snake-fury/assets/snake_arquitecture.png)

Let's summarize what we have:

- Two external devices:
  - The keyboard input
  - The screen
- Two external services communicating with the external devices
  - The `EventQueue` (read from the keyboard)
  - The renderer device. In our case, it is the console, but it could be a SDL2's pixel buffer or a OpenGL Data Structure
- One main thread which read from the `EventQueue` and send to the renderer device. The main thread is separated in:
  - The GameState
  - The RenderState

Notice that the core of the software is the main thread. In theory we could change all external components without touching the logic at all. Examples of this are:

- We use ncurses frontend
  - We read events from terminal but using ncurses built-in Event system
  - We render to the console but using ncurses TUI capabilities.
- We use SDL frontend
  - Instead of reading directly inputs from console, we use SDL's built-in event queue
  - Instead of rendering to console we use SDL's surface buffer which is rendered using the GPU.

This refactoring is separated in one Step with two task.

- Step 1: Make your code abstract.
  - Task 1.1: Abstract the read only environment
  - Task 1.2: create type classes following the ideas above.

## Step 1: abstract away your code

The idea is that we are going to define an `App` type which will have instances for many type classes expressing the components above. If we'd like to change frontends, we will just create new type `AppSDL` and create the convenient instances.

### Task 1.1: Abstract the environment

- In `RenderState.hs` create a type class `HasBoardInfo env` which has one method called `getBoardInfo :: env -> BoardInfo`.
- Substitute in `RenderState.hs` and `GameState.hs` any constraints of `MonadReader BoardInfo m` by `MonadReader env m, HasBoardInfo env`

### Task 1.2: Create a single all-in `App` type

- modify `App.hs` so your `App` type has now an environment with the `BoardInfo` and the `EventQueue`.

```haskell
data AppState = AppState GameState RenderState
data Env = Env BoardInfo EventQueue
newtype App m a = App {runApp :: ReaderT Env (StateT AppState m) a}
  deriving (Functor , Applicative, Monad, MonadState AppState, MonadReader Env, MonadIO)
```

You may wonder why using an environment with the `EventQueue` on it, when the event queue is not part of the core logic (only the events it contains are). Certainly, we could pass the `EventQueue` as a parameter to all the functions that need it, but this is a little inconvenient. Also, you may notice that we are putting a mutable asynchronous queue into a read-only environment. That doesn't look like the things Haskell promises!! Precisely, Haskell is famous for not having global mutable states, or mutable variables out of the box. This pattern of pushing mutable structures into a read-only environment is actually quite common, and it is called the `ReaderT` pattern. Strictly speaking, we are _not_ using such a pattern here, but we are taking some of the ideas of this pattern and applying them to our piece of software.

- Define instances for `Env` so it can use functions in `GameState` and `RenderState` modules.

```haskell
instance HasBoardInfo Env where
  getBoardInfo = undefined

instance HasEventQueue Env where
  getEventQueue = undefined
```

- Define the following type classes. If you look closely, we only define what is strictly necessary for the core logic. We need to pull events from somewhere, we need to render somehow, and we need to update both state sending messages.

```haskell
class Monad m => MonadQueue m where
  pullEvent :: m Event          -- ^Pull an Event from the queue

class Monad m => MonadSnake m where
  updateGameState :: Event -> m [RenderMessage]
  updateRenderState :: [RenderMessage] -> m ()

class Monad m => MonadRender m where
  render :: m ()
  
```

You may be surprised `updateRenderState` does not produce a `Builder` as we do in the `RenderState` module. The reason for that is because we don't want to be bounded to a specific implementation of the rendering. `Builder` just works to render into the console as we do now, but it is useless if we'd like to change frontends.

- Now we need to define instances for `App` monad

```haskell
instance (???) => MonadQueue (App m) where -- you will need to fill the ???
  pullEvent = undefined

instance (???) => MonadSnake (App m) where -- you will need to fill the ???
  updateGameState = undefined
  updateRenderState = undefined

instance (???) => MonadRender (App m) where -- you will need to fill the ???
  render = undefined
```

- last, let's define the logic of the game.

```haskell

-- This set the the speed of the game on the score. Notice the constraint give access to all the components.
setSpeedOnScore :: (MonadReader env m, HasEventQueue env, MonadState state m, HasRenderState state, MonadIO m) => m Int
setSpeedOnScore = undefined

-- This is one step of the logic: read from the queue and-then update the game state and-then update the render state and-then render
gameStep :: (MonadQueue m, MonadSnake m, MonadRender m) => m ()
gameStep = undefined -- Try just using the operator >>= which you can read as "(m >>= f) do m and-then f"

-- The game loop implementation is provided. To pretty much can read in english.
gameloop :: (MonadQueue m, MonadSnake m, MonadRender m, MonadState state m, HasRenderState state, MonadReader env m, HasEventQueue env, MonadIO m) => m ()
gameloop = forever $ do
  w <- setSpeedOnScore 
  liftIO $ threadDelay w
  gameStep
  isGameOver <- gets (gameOver . getRenderState)
  unless isGameOver gameloop

-- Run the application as usual
run :: Env -> AppState -> IO ()
run env app = runApp gameloop `runReaderT` env `evalStateT` app

```

- Last step. Fix compiler errors.

## Congratulations

You've completed the `snake-fury` challenge. Hopefully all the code deletions, refactorings, etc... has give you a good understanding on how Haskell code is structured and how monads play a fundamental role on it.

Of course, there is a lot of missing topics. From here, you can try to write a SDL frontend using `sdl2` [bindings](https://hackage.haskell.org/package/sdl2) or ncurses frontend with the [`brick`](https://hackage.haskell.org/package/brick)

Other topics you should explore, are free-monads and effect systems, the ReaderT pattern, type level programming, and many many more.

If you've found any refactoring too difficult or badly introduced, feel free to open an issue.
