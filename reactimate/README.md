# Reactimate

`reactimate` is a library implementing the AFRP paradigm. In contrast to other libraries, `reactimate` has a static representation for signals to increase performance and a concrete base type to eliminate typeclass performance problems.

## Signal

The most important type is the `Signal`. A `Signal es a b` represents a computation which uses `a` as input and produces `b` as output. `reactimate` implements a lot of combinators to work with those `Signal`s.

Here, we use the `arr` function from the `Arrow` typeclass to create a simple `Signal` from the `(+5)` function.
```haskell
add5 :: Signal es Int Int
add5 = arr (+5)
```

We can easily execute signals in sequence with the `>>>` combinator.
```haskell
add10 :: Signal es Int Int
add10 = add5 >>> add5
```

## Effects

Usually, a `Signal` cannot execute side-effects. However, the effect list `es` can be used to store effect handles which can be used to run them.

### Running signals

Typically, you will run `Signal`s with `reactimate`. It will run the given `Signal` over and over again until it finally produces a `Just` value and then returns that value.
```haskell
main :: IO ()
main = do
  result <- runSetup $ reactimate someSignal
  putStrLn result

someSignal :: Signal () (Maybe String)
someSignal = ...
```

With `reactimate`, it is easily possible to implement a game loop or some other simulation loop.

### Stateful signals

`Signal`s can store some internal state during their execution. In general, `Signal`s do **not** produce the same output for the same input!

State can be easily integreated in a `Signal` with the `feedback` function:
```haskell
feedback :: b -> Signal (a, b) b -> Signal a b

sum :: Signal Int Int
sum = feedback 0 $ arr \(input, acc) -> input + acc
```

`feedback` takes some initial state and then accumulates this state over simulations. The state from the last execution is fed back as input. The `sum` signal produces the sum of all its inputs by keeping track of the last output.


### Experimental Pull-Push-based FRP

Conventional AFRP evaluates at a set frequency in time. If events happen in-between two evaluations, they can only be processed in the next evaluation.
Therefore, it is not possible to do something in the exact moment the event occurs.

`reactimate` has support for such events which happen outside of the simulation and occur at any time.
The idea is to evaluate the signal not at a specific frequency, but rather evaluate the signal whenever the event happens.

Here is an example:

```haskell
increasingEvent :: Event ()
increasingEvent = mapEvent (sumUp >>> arrIO print) (pulse 2 1)

main :: IO ()
main = reactimateEvent $ Nothing <$ increasingEvent
```

`pulse 2 1` emits an event with payload 1 every 2 seconds. `sumUp >>> arrIO print` sums up all inputs and prints the output.
`sampleEvent` will wait until its given event produces a `Just` value. This never happens here, so it just runs forever. 

## Microbenchmarks

Beware that micro benchmarks may not reflect 1 to 1 on real applications. The actual performance gain on applications still needs to be tested. Still, we can see that `reactimate` has lower overhead than other libraries.

```
Countdown benchmark
  Yampa:      OK
    20.5 ms ± 1.5 ms
  dunai:      OK
    44.5 ms ± 4.3 ms
  reactimate: OK
    175  μs ± 6.9 μs
Integrate benchmark
  Yampa:      OK
    71.9 ms ± 2.5 ms
  reactimate: OK
    6.18 ms ± 375 μs
Chaining (>>>) benchmark
  Yampa:      OK
    24.6 ms ± 788 μs
  dunai:      OK
    40.3 ms ± 1.8 ms
  reactimate: OK
    2.53 ms ± 202 μs
```

## Acknowledgements

Heavily inspired by [Yampa](https://github.com/ivanperez-keera/Yampa) and [dunai](https://github.com/ivanperez-keera/dunai).
