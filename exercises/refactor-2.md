# Refactor 2: implementing state as a monad

- [Refactor 2: implementing state as a monad](#refactor-2-implementing-state-as-a-monad)
  - [Step 1: Apply changes locally](#step-1-apply-changes-locally)
    - [Task 1.1: modify makeRandomPoint and newApple](#task-11-modify-makerandompoint-and-newapple)
    - [Task 1.2: Create two new functions](#task-12-create-two-new-functions)
  - [Step 2: Did you spot the pattern?! That's a monad](#step-2-did-you-spot-the-pattern-thats-a-monad)
    - [Task 2.1: Refactor your code using the state monad](#task-21-refactor-your-code-using-the-state-monad)

In this refactor you'll work on the `GameState` module. Your mission is to refactor the code to use the `State` monad. Before doing that you will refactor the code to make _isolated changes in the state_ (I'm explaining this soon, bear with me). To summarize, this refactor has two steps:

- step 1: make isolated changes in the state as pure functions
- step 2: realizing the emerging pattern as a monad.

Ok, enough buzzwords. In the `GameState` module you have functions like `newApple` or `nextHead` that take the `BoardInfo` and parts of the `GameState` and produce a result. This result is used in the `move` function to update the parts of the state that changes. For example, `newApple` produces a `(Point, StdGen)`. If the snake is eating the apple, this function is responsible of producing a `Point` for the new apple and a `StdGen` that will be used for the next apple. This has two inconveniences:

- The function `move` has too many responsibilities, it moves the snake body, checks if the apple is there, produces a new apple, etc...
- Every time we call any of this function, the caller must remember to update the state. In other words, we've delegated the `newApple`'s responsibility to the `newApple`'s caller.

Of course, not always is clear who is responsible for what... that's software development! In this case (for the sake of learning) we are going to modify the code so we pass state updates to the callee instead of the caller. Let's split the `move` function using a very common pattern!

## Step 1: Apply changes locally

In this step we are going to refactor the structure of our functions so all of them follow a pattern. Improving code quality.

Look at the type of `move`, it is `BoardInfo -> GameState -> (GameState, [RenderMessage])`. Meaning that, it takes the `GameState` and produces a pair consisting in the new state and a result, which in this case is `[RenderMessage]`. We can apply the same trick to every function: take the `GameState` and produce a pair with the new state and a result.

### Task 1.1: modify makeRandomPoint and newApple

- Change the function `makeRandomPoint` from type `BoardInfo -> StdGen -> (Point, StdGen)` to type `BoardInfo -> GameState -> (Point, GameState)`. The function should return the random point and a new state with the `randomGen` field updated.
- Fix any compiler error
- Change the function `newApple` from type `BoardInfo -> GameState -> (Point, StdGen)` to type `BoardInfo -> GameState -> (Point, GameState)`. The function should return the new apple point and a new state with the `applePosition` field updated.
- Fix any compiler error

### Task 1.2: Create two new functions

- Create function `extendSnake :: Point -> BoardInfo -> GameState -> (RenderState.DeltaBoard, GameState)` such that given a `point` representing the new position of the head, then it extends the current snake appending the new head. It returns the `GameState` with the field `snakeSeq` modified and `RenderState.DeltaBoard` with the changes we need to send to the `Board` in the `RenderState`. In other words, this function will be called when the snake is eating the apple.
- Create function `displaceSnake :: Point -> BoardInfo -> GameState -> (RenderState.DeltaBoard, GameState)` such that given a `point` representing the new position of the head, then it displaces the current snake by appending the new head and _removing the tail_. It returns the `GameState` with the field `snakeSeq` modified and `RenderState.DeltaBoard` with the changes we need to send to the `Board` in the `RenderState`. In other words, this function will be called when the snake is not eating the apple.

## Step 2: Did you spot the pattern?! That's a monad

In this step we are going to define the `State` monad and a justification of it based on the previous pattern we've introduced. We are covering the following topics:

- There is a clear pattern introduced in Step 1
- The previous refactor has some problems
- What is the `State` monad?
- How does it help to make better code?

We are verging to the part in which we use monads. The previous refactor has been difficult, but hopefully the function `move` now is clearer than before (if it isn't check the solution). On the way you've probably spotted a pattern: we are using always functions with almost the same structure: `some_function :: some_arguments -> GameState -> (Result, GameState)` (forget about `BoardInfo` and other arguments and let's focus on the last part of the signature.). These funtions always work the same way: they take some arguments, the game state and produce the updated game state and a result (a `Point`, a `DeltaBoard`, etc...).

Probably, in the previous step you've found yourself manually unpacking calls to these functions to use the updated state elsewhere. For example, I got something similar to this:

```haskell
let (delta, game_state2) = extendSnake newHead board_info game_state1
    (point, game_state3) = newApple board_info game_state2
```

This manual handling of states is clumsy and error prone. Very easily you can forget to pass the updated state to the next function (if you don't believe me check the commit history, it has happened to me). The `State` monad is the solution for this. Before talking about the `State` monad you should notice:

- The `State` monad is just an example (or an monad instance). It isn't THE definition of monad. You'll learn more monad examples in this challenge.
- The `State` monad does not provide _more_ functionality!! It's just a convenient interface for the problem we've seen before. This is a key concept: **Monads do not provide any extra functionality, they are just a convenient interface for many different patterns**.

Now, let's go to the definition. The `State` monad is nothing else than a function from a state to a pair of a result and the updated state `type State a = SomeState -> (a, SomeState)` (the actual implementation is a little bit different). I'd recommend [monday morning haskell blog](https://mmhaskell.com/monads) as a reference for learning monads in depth. Here we are providing a shallow explanation. A _very_ important concept to understand when learning the `State` monad is that you are not handling a piece of data, you are defining a function. Maybe, a better name would have been the `StateTransformation` monad. I know this sounds abstract right now, but keep this in mind: _the state monad defines a transformation on a piece of data that will be provided later_.

Now, how does the state monad help with the implementation? Essentialy, it applies state transformation automatically. Following the previous example:

```haskell
# Without state monad you have this
let (delta, game_state2) = extendSnake newHead board_info game_state1
    (point, game_state3) = newApple board_info game_state2
 in ...

# With state monad this
extendAndCreateNewApple newHead board_info = extendSnake newHead board_info >> newApple board_info >> ...
```

Wait what? Did the state handling disappear?. Yes!, that's the magic of the state monad. You defined small functions modifying the state and then you chain them together using operators like `>>`, `>>=` or `>=>`. Also, Haskell provides syntactic sugar for those operators in the form of the `do`-notation. You should read about it.

### Task 2.1: Refactor your code using the state monad

This will be a hard refactor. Keep that in mind, you'll need some time to get used to. Also remember that _the state monad defines a transformation on a piece of data that will be provided later_. This will be useful when using functions like `get` which seem to magically produce a piece of data out of nowhere.

- Use the following imports: `import Control.Monad.Trans.State.Strict (State, get, put, modify, gets, runState)`
- First define a type synonym `type GameStep a = State GameState a`. You'll need to import `Control.Monad.State.Strict`.
- Change functions so instead of having type `GameState -> (a, GameState)` they have `GameStep a`. Also, rename function `move` to `step` and create a function `move` which actually runs the `State` monad. You should refactor at least the following functions to have the given types:
  - `makeRandomPoint :: BoardInfo -> GameStep Point`
  - `newApple :: BoardInfo -> GameStep Point`
  - `extendSnake ::  Point -> BoardInfo -> GameStep DeltaBoard`
  - `displaceSnake ::  Point -> BoardInfo -> GameStep DeltaBoard`
  - `step :: BoardInfo -> GameStep [Board.RenderMessage]`. This is the function `move` _renamed_
  - `move :: BoardInfo -> GameState -> ([Board.RenderMessage], GameState)`. This is a _new_ function `move` which is defined in terms of `runState` and `step`

Good luck!
