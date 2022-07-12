# Refactor 1: from mpv to full game

In this refactor you'll modify the code to add a score board and to accelerate snake's speed as you get points. Also we'll point out a performance problem with the renderer. Your mission is to solve it.

This refactor has three parts:

## Part 1: Add score
you will update the code to make an score which is updated each time the snake eats an apple. The score is render in the terminal above the board. (if you prefer another rendering is up to you. Shouldn't be difficult to custom you own builder)

### Task 1.1: Introduce a `score` field
- Add a `score :: Int` field to `RenderState`. You can add it to `GameState` too, but since we use `score` solely to render it on the screen and to speed up the fps, It makes sense to keep it only in the rendering side of things.
- Once you add `score`, try to compile the code. Follow the errors until you got the game working again. Now lets, implement the logic.

### Task 1.2: Update the score
- Because `score` is used in `RenderState` we need to update it via `RenderMessage`. Add a new case in the `RenderMessage` ADT to represent such message. Does the compiler complain about incomplete patterns? Which function/s is broken after this change?
- If you think carefully, we need to change `GameState.move` function, because now there are cases in which we need to send more than one message. Update the code to send a list of messages after each move. 
- Now, something is broken... we used to proccess a single message in `RenderState.updateRenderState`. Create a new function `updateMessages :: RenderState -> [RenderMessage] -> RenderState` which updates the state given many messages. hint: it can be a one-line function.

## Part 2: Increase the speed every 10 scores up to 5 speed-ups.

In this part you'll be asked to modify some code of the `EventQueue`, which runs in the `IO` monad. So we can say that this will be your first contact with monads. Congrats!. The only problem is that `EventQueue` is an asynchronous queue of events, so it can be a little bit too much for your first contact. Read carefully and don't worry if you need to sneek out the solution doing `git checkout solution-refactor-1`. 

### Task 2.1: Change the `EventQueue` record

-- Add two fields
