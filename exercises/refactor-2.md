# Refactor 2: implementing state as a monad

In this refactor you'll work on the `GameState` module. Your mission is to refactor the code to use the `State` monad. Before doing that you will refactor the code to make _isolated changes in the state_ (I'm explaining this soon, bear with me). To summarize, this refactor has two steps:

- step 1: make isolated changes in the state as pure functions
- step 2: realizing the emerging pattern as a monad.

Ok, enough buzzwords. In the `GameState` module you have functions like `newApple` or `nextHead` that take the `BoardInfo` and parts of the `GameState` and produce a result. This result is used in the `move` function to update the parts of the state that changes. For example, `newApple` produces a `(Point, StdGen)`. If the snake is eating the apple, this functions is responsable of producing a `Point` for the new apple and a `StdGen` that will be used for the next apple. This has two inconvinients: 
 
- The function `move` has too many responsabilities, it moves the snake body, checks if the apple is there, produces a new apple, etc...
- Every time we call any of this functions, the caller must remember to update the state. In other words, we've delegated the `newApple`'s responsability to the `newApple`'s caller

Of course, not always is clear who is responsable for what... that's software development!. In this case (for the sake of learning) we are going to modify the code so we pass state updates to the callee instead of the caller. Let's split `move` function using a very common pattern!

## Step 1: Apply changes locally

Look at the type of `move`, it is `BoardInfo -> GameState -> (GameState, [RenderMessage])`. Meaning that, It takes the `GameState` and produces a pair consisting in the new state and a result, which in this case is `[RenderMessage]`. We can apply the same trick to every function: take the `GameState` and produce a pair with the new state and a result.

### Task 1.1: modify makeRandomPoint and newApple

- Change the function `makeRandomPoint` from type `BoardInfo -> StdGen -> (Point, StdGen)` to type `BoardInfo -> GameState -> (Point, GameState)`. The function should return the random point and a new state with the `randomGen` field updated. 
- Fix any compiler error
- Change the function `newApple` from type `BoardInfo -> GameState -> (Point, StdGen)` to type `BoardInfo -> GameState -> (Point, GameState)`. The function should return the new apple point and a new state with the `applePosition` field updated. 
- Fix any compiler error

### Task 1.2: Create two new functions

- Create function `extendSnake :: Point -> BoardInfo -> GameState -> (RenderState.DeltaBoard, GameState)` such that given a `point` representing the new position of the head, then it extends the current snake appending the new head. It returns the `GameState` with the field `snakeSeq` modified and `RenderState.DeltaBoard` with the changes we need to send to the `Board` in the `RenderState`. In other words, this function will be called when the snake is eating the apple. 
- Create function `displaceSnake :: Point -> BoardInfo -> GameState -> (RenderState.DeltaBoard, GameState)` such that given a `point` representing the new position of the head, then it displace the current snake by appending the new head and _removing the tail_. It returns the `GameState` with the field `snakeSeq` modified and `RenderState.DeltaBoard` with the changes we need to send to the `Board` in the `RenderState`. In other words, this function will be called when the snake is not eating the apple. 

### Step 2: Did you spot the pattern! That's a monad!

We are verging to the part in which we use monads. The previous refactor has been difficult, but hopefully the function `move` now is clearer than before (If it isn't check the solution). On the way you've probably spotted a pattern: we are using always functions with almost the same structure: `some_function :: some_arguments -> GameState -> (Result, GameState)` (forget about `BoardInfo` and other arguments an let's focus on the last part of the signature.). These funtions always work the same way, they take some arguments, the game state and produce the updated game state and a result (a `Point`, a `DeltaBoard`, etc...). 

Probably, In the previous step you've found yourself manually unpacking calls to this functions to use the updated state elsewhere. For example, I got something similar to this:

```haskell
let (delta, game_state2) = extendSnake newHead board_info game_state1
    (delta, game_state3) = newApple board_info game_state2
```

