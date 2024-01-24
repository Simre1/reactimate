# Fast signals

`fast-signals` is a library implementing the AFRP paradigm.
It prioritizes performance at the cost of some purity and also integrates some ideas from pull-based FRP.

## Microbenchmarks

Beware that micro benchmarks may not reflect 1 to 1 on real applications. The actual performance gain on applications still needs to be tested.

```
Countdown benchmark
Times (ms)
                 min    mean    +/-sd  median    max 
Yampa:         26.232  27.190   1.465  26.757  32.328
dunai:         69.809  71.034   1.085  70.721  74.821
fast-signals:   0.282   0.291   0.013   0.287   0.338

Integrate benchmark
Times (ms)
                  min      mean      +/-sd    median      max  
Yampa:         1051.025  1051.025       NaN  1051.025  1051.025
fast-signals:    82.608    82.608       NaN    82.608    82.608

Chaining (>>>) benchmark
Times (ms)
                 min    mean    +/-sd  median    max 
Yampa:         24.234  28.083   1.976  28.354  30.920
dunai:         64.061  77.048   7.085  76.486  93.851
fast-signals:   7.374   8.300   0.807   8.039  10.705
```

## Acknowledgements

Heavily inspired by [Yampa](https://github.com/ivanperez-keera/Yampa) and [dunai](https://github.com/ivanperez-keera/dunai).
