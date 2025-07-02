# Multitasking

Haskell has great tools for dealing with concurrency. However, in praxis they are difficult to use.

This library aims to make concurrency easy by providing many built-in solutions for common concurrency patterns.
It is based on [structured concurrency](https://vorpus.org/blog/notes-on-structured-concurrency-or-go-statement-considered-harmful), not letting threads outlive their parent scope. Additionally, exceptions are propagated automatically. This means that you do not have to worry about:

- Zombie processes, since a thread can never outlive its parent scope.
- Dead processes, since exceptions will propagate to the parent thread.


## Examples

```haskell
awaitAll :: IO ()
awaitAll =
  -- open up a concurrency scope
  multitask $ \coordinator -> do
    -- launch tasks
    task1 <- start coordinator action1
    task2 <- start coordinator action2
    -- Wait for all actions to complete
    await coordinator
```

```haskell
awaitTask :: IO ()
awaitTask =
  -- open up a concurrency scope
  multitask $ \coordinator -> do
    -- start task
    task <- start coordinator $ pure 10
    
    -- do some work
    let x = 10

    -- wait for task to complete and get the result
    result <- await task

    -- prints 20
    print $ x + result
```

```haskell
raceTasks :: IO ()
raceTasks = multitask $ \coordinator ->
  slot <- newSlot
  _ <- start coordinator $ action1 >>= putSlot slot
  _ <- start coordinator $ action2 >>= putSlot slot
  result <- awaitSlot slot
  print result
```

```haskell
builtinRace :: IO ()
builtinRace = do
    result <- raceTwo (threadDelay 1000000 >> pure 10) (pure 20)
    print result
```

## Comparison with other libraries

- `ki`: Implements structured concurrency, but provides no high-level functionality. `multitasking` uses `ki` internally and provides high-level functionality. 
- `async`: Does not implement structured concurrency

## Acknowledgements

- Inspired by [Notes on structured concurrency](https://vorpus.org/blog/notes-on-structured-concurrency-or-go-statement-considered-harmful), which is implemented in the `trio` Python library.
