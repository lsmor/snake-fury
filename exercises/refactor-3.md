# Refactor 3: Monad Transformers

In this refactor you'll work on the `GameState` and `RenderState` modules. From this point on, you'll feel you are overengineering your code. This is a difficult topic. I'd say that simple is better than complex, so the implementation as in refactor 2 is a good balance between complexity and simplicity. Nevertheless, In order to learn deeper about monads we need to move into the topic of monad transformers and `mtl` style. If you feel your code is getting less readable, keep in mind this kind of patterns are suited for more evolved software, so it may not fit well in this situation. It wont be that bad, though. 

This refactor focuses in one concept with two task. 

- Step 1: Introduce the `ReaderT` transformer and monad stacks in general
  - Taks 1: use the `ReaderT` transformer to refactor a common pattern
  - Taks 2: Apply the same stack to the `RenderState` module

## Step 1: Monad Stacks.

As you can guess, `State` monad is just an example of monad. In Haskell we have monads for error handling (`Maybe` and `Either e`), for doing input/output (`IO`), for parallelism (`Eval`), for read-only / write-only / read-write state (resp. `Reader`, `Write`, `State`), etc... 

The problem with monads comes when you want to compose them. Meaning, what happend if I want to have do input/output and having a read-only environment, and a read-write state? I would need to use three different monad, but what type is that? AS we've seen in the previous refactor we have `State` for managing the read-write state. If I want to do all those effects, Does it exist a `IOReaderState` data type?. There existe something very similar: monad transformers.
### Task 1.1: Did you spot the subtle pattern?! That's the Reader Monad!!

Did you notice that many functions in the `GameState` module uses the `BoardInfo` datatype? Of course!, the information about the size of the board is crucial all along the software. The `Reader` monad essentialy abstract this pattern. When you have a function from some environment to some result `function :: env -> a`, that can be express as `Reader env a`. This is usefull when you have multiple functions taking the same `env`. For example

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

You may think with haven't won anything. That's kind of true, but intention matters here. In the second example is very clear from the type signature that `env` is an important datatype in our software. It is used for configuration and as a read-only environment. Let's refactor `GameStep` to introduce the `ReaderT` transformer.

- Introduce the following `imports`
  - `import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask, runReader)`
  - `import Control.Monad.Trans.Class ( MonadTrans(lift) )`
- change the `GameStep` to look like below. Try to understand what you are doing. (tip: why `ReaderT` but not `StateT`. Go to transformers library documentation and read more about them)
  - `type GameStep a = ReaderT BoardInfo (State GameState) a`
- Change all functions with signature `BoardInfo -> GameStep a` to `GameStep a`. Use function `ask` to get the read only env . These are:
  - `makeRandomPoint :: GameStep Point`
  - `newApple :: GameStep Point`
  - `extendSnake ::  Point -> GameStep DeltaBoard`
  - `displaceSnake :: Point -> GameStep DeltaBoard`
  - `step :: GameStep [Board.RenderMessage]`
- Notice that you need to use `lift` when using the `State` monad. (Why is that?)
- Modify function `move` to use `runReaderT`

### Task 1.2: Refactor `RenderState`

For the sake of practise, let's refactor `RenderState` module. Again, this feels like a total overengineering. Keep in mind this refactor will break something in the `main` module. This is done on pourpose.

- import the following:
  - `import Control.Monad.Trans.Reader (ReaderT (runReaderT), asks, ask)`
  - `import Control.Monad.Trans.State.Strict (State, put, get, runState, evalState)`
  - `import Control.Monad.Trans (lift)`
  - More import will be necessary, but you should find out yourself.
- Create a new datatype `type RenderStep a = ReaderT BoardInfo (State RenderState) a`
- Change these functions types and modify their body accordingly:
  - `updateRenderState :: RenderMessage -> RenderStep ()`
  - `updateMessages :: [RenderMessage] -> RenderStep ()`
- Rename function `render` to `renderStep` with type `renderStep :: [RenderMessage] -> RenderStep Builder`
- Create function `render` with type `render :: [RenderMessage] -> BoardInfo -> RenderState ->  (Builder, RenderState)`. Given a list of messages, it runs the monad stack and produces the builder associated to the Initial 