# Refactor 1: from mpv to full game

- [Refactor 1: from mpv to full game](#refactor-1-from-mpv-to-full-game)
  - [Step 1: Add score](#step-1-add-score)
    - [Task 1.1: Introduce a `score` field](#task-11-introduce-a-score-field)
    - [Task 1.2: Update the score](#task-12-update-the-score)
  - [Step 2: Increase the speed every 10 scores up to 5 speed-ups](#step-2-increase-the-speed-every-10-scores-up-to-5-speed-ups)
    - [Task 2.1: Change the `EventQueue` speed](#task-21-change-the-eventqueue-speed)
  - [Step 3: Performance issues](#step-3-performance-issues)
    - [Task 3.1: Render the score](#task-31-render-the-score)
  
In this refactor you'll modify the code to add a score board and to accelerate snake's speed as you get points. Also we'll point out a performance problem with the renderer. Your mission is to solve it.

This refactor has three steps:

## Step 1: Add score

you will update the code to make a score which is updated each time the snake eats an apple. The score is rendered in the terminal above the board. (if you prefer another rendering is up to you. Shouldn't be difficult to custom you own builder).

### Task 1.1: Introduce a `score` field

- Add a `score :: Int` field to `RenderState`. You can add it to `GameState` too, but since we use `score` solely to render it on the screen and to speed up the fps, it makes sense to keep it only in the rendering side of things.
- Once you add `score`, try to compile the code. Follow the errors until you got the game working again. Now let's implement the logic.

### Task 1.2: Update the score

- Because `score` is used in `RenderState` we need to update it via `RenderMessage`. Add a new case in the `RenderMessage` ADT to represent such message. Does the compiler complain about incomplete patterns? Which function/s is broken after this change?
- If you think carefully, we need to change `GameState.move` function, because now there are cases in which we need to send more than one message. Update the code to send a list of messages after each move.
- Now, something is broken... we used to proccess a single message in `RenderState.updateRenderState`. Create a new function `updateMessages :: RenderState -> [RenderMessage] -> RenderState` which updates the state, given many messages. Hint: it can be a one-line function.
- clean compiler errors if any.

## Step 2: Increase the speed every 10 scores up to 5 speed-ups

In this part you'll be asked to modify some code of the `EventQueue`, which runs in the `IO` monad. So we can say that this will be your first contact with monads. Congrats! The only problem is that `EventQueue` is an asynchronous queue of events, so it can be a little bit too much for your first contact. Read carefully and don't worry if you need to sneek out the solution doing `git checkout solution-refactor-1`.

### Task 2.1: Change the `EventQueue` speed

- Take a look at functions `EventQueue.calculateSpeed` and `EventQueue.setSpeed`. The former is a pure function which calculates the speed given the score and the initial speed. The second is a function in the `IO` monad. Try to understand it as better as you can, but don't worry if you can't fully understand it.
- Take a look at function `Main.gameloop`. The first line looks like `threadDelay $ initialSpeed queue`. Let's go step by step: `initialSpeed queue` accesses the `initialSpeed` field within the `EventQueue`. Unsurprisingly, this is the speed you set up when running the code. `threadDelay` essentialy stops the execution for a given number of microseconds. Therefore, this function looks at the speed you set when initializing the game and waits that much time.
- Your mission is to modify that part of the code to get the speed based on the `score` and wait that much time. Two changes are needed
  - the first line now should modify the speed. Use the function `setSpeed`. Given the right arguments it returns the new speed _in a monadic context!_ To access that value you have to _bind_ it: `new_speed <- setSpeed <args>`
  - Modify the second line (`threadDelay $ initialSpeed queue`) so now you wait the right amount of seconds.
- clean errors the compiler gives if any.

Now if you compile the code, you should notice that every 10 points the speed increases by a 10%. You can tweak this configuration in the `EventQueue.calculateSpeed` function.

## Step 3: Performance issues

If you run a big enough board or at high speed, you'll notice that the game starts to blink, or the board is rendered slowly. This is because the function `render` returns a `String`. This type is an historical mistake within Haskell ecosystem and should be banned. Unfortunately, this would break lot of code, so we have to live with that. `String` is a linked list of `Char` which is a very inefficient way of representing textual data. You should never use `String`... never! There are other many representations of textual data such as `Text` (utf-8), `ByteString` (raw bytes decoded as ascii), `Builder` (a buffer in memory), and lazy variations of each. Yes, the Haskell ecosystem for textual data is a mess.

For this challenge we are going to use a not very common representation of text: `ByteString.Builder`. The reason for that is because we are transforming the `RenderState` by concatenating strings text. `Builder` is a very efficient type for concatenation.

### Task 3.1: Render the score

- Read the documentation of `Data.ByteString.Builder` from `bytestring` package.
- Create a function `ppScore :: Int -> Builder` which pretty prints the score. Feel free to use a representation you like. Below you have some examples
- Modify the function `render` to have type `:: BoardInfo -> RenderState -> Builder`. Of course, the render function should plot the score and the board itself
- clean errors the compiler gives. How do you print a `Builder` into the console?  (hint: check `IO` functions in the `Data.ByteString.Builder` [module](https://hackage.haskell.org/package/bytestring-0.10.6.0/docs/Data-ByteString-Builder.html))

Here are some ways you can render the score:

```bash
# With stars lines    # With a board    # With motivation quote changing every 10 points
                                      
********              |--------|        score:  5 / do better!
score:10              |score:10|        score:  10 / keep going!
********              |--------|        score:  20 / on fire!
```
