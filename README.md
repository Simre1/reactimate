# Reactimate

`reactimate` is a library implementing the AFRP paradigm. In contrast to other libraries, `reactimate` uses `IO` effects to increase performance and a concrete base type to eliminate typeclass performance problems. In addition, `reactimate` has some support for pull-based FRP, making it possible to deal with events which happen in-between simulation cycles.

## Signal

The most important type is the `Signal`. A `Signal a b` represents a computation which uses `a` as input and produces `b` as output. `reactimate` implements a lot of combinators to work with those `Signal`s.

Here, we use the `arr` function from the `Arrow` typeclass to create a simple `Signal` from the `(+5)` function.
```haskell
add5 :: Signal Int Int
add5 = arr (+5)
```

We can easily execute signals in sequence with the `>>>` combinator.
```haskell
add10 :: Signal Int Int
add10 = add5 >>> add5
```

### Running signals

Typically, you will run `Signal`s with `reactimate`. It will run the given `Signal` over and over again until it finally produces a `Just` value and then returns that value.
```haskell
main :: IO ()
main = do
  result <- reactimate someSignal
  putStrLn result

someSignal :: Signal () (Maybe String)
someSignal = ...
```

With `reactimate`, it is easily possible to implement a game loop or some other simulation loop.

### Stateful signals

`Signal`s can store some internal state during their execution. In general, `Signal`s do **not** produce the same output for the same input!

State can be easily integreated in a `Signal` with the `feedback` function:
```haskell
feedback :: s -> Signal (a, s) (b, s) -> Signal a b

sum :: Signal Int Int
sum = feedback 0 $ arr \(input, acc) -> 
  let output = input + acc
  in (output, output)
```

`feedback` takes some initial state and then accumulates this state over simulations. The state from the last execution is fed back as input. The `sum` signal produces the sum of all its inputs by keeping track of the last output.

# Reactimate Game

`reactimate-game` is a library for basic 2D games using `SDL`. [Take a look here](reactimate-game).

![reactimate-games examples](reactimate-game/screenshot.png)
