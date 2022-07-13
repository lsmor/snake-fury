# Refactor 2: implementing state as a monad

In this refactor you'll work on the `GameState` module. Your mission is to refactor the code to use the `State` monad. Before doing that you will refactor the code to make _isolated changes in the state_ (I'm explaining this soon, bear with me). To summarize, this refactor has two steps:

- step 1: make isolated changes in the state as pure functions
- step 2: realizing the emerging pattern as a monad.

Ok, enough buzzwords. In the `GameState` module you have functions like `newApple` or `nextHead` that take the `BoardInfo` and parts of the `GameState` and produce a result. This result is used in the `move` function to update the parts of the state that changes. For example, `newApple` produces a `(Point, StdGen)`. If the snake is eating the apple, this functions is responsable of producing a `Point` for the new apple and a `StdGen` that will be used for the next apple. This has a little inconvinient: the function `move` has too many responsabilities, it moves the snake body, checks if the apple is there, produces a new apple, etc...

It depends on you implementation but probably the `move` function is kind of big. Let's split it using a very common pattern!

## Step 1: Apply changes locally

Look at the type of `move`, it is `BoardInfo -> GameState -> (GameState, [RenderMessage])`. Meaning that, It takes the `GameState` and produces a pair consisting in the new state and a result, which in this case is `[RenderMessage]`. We can apply the same trick to every function: take the `GameState` and produce a pair with the new state and a result.


### Task 1.1: 
