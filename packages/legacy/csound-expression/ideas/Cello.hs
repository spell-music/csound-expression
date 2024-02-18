
import Csound.Base
{-
```haskell
> mixAt 0.25 smallHall2 (piano)
```

```
> dac $ hlift3 (\x y z-> midi $ onMsg (mlp (1500 * x) 0.35 . mul (leg 0.5 0.5 0.8 0.1). saw . (\x -> x * (1 + y * 0.07 * osc (12 * z))))) (uknob 0.4) (uknob 0.35) (uknob 0.45)
```

```haskell
> dac $ hlift4 (\t x y z-> midi $ onMsg (mlp (1500 * t) x . mul (leg 0.5 0.5 0.8 0.1). saw . (\x -> x * (1 + y * 0.07 * osc (12 * z))))) (uknob 0.4) (uknob 0.35) (uknob 0.35) (uknob 0.45)
```

```haskell
dac $ hlifts (\[p, t, x, y, z]-> mixAt 0.28 smallHall2 $ at fromMono $ midi $ onMsg (mlp (800) 0.45 . hp 50 10 . bat (\x -> sum [bp (300 + 200 * (p - 0.5)) 60 x, bp (700 + 300 * (t - 0.5)) 40 x, bp (1500 + 600 * (x - 0.5)) 30 x]) . mul (leg 0.5 0.5 0.8 0.1). saw . (\x -> x * (1 + y * 0.07 * osc (12 * z))))) [uknob 0.25, uknob 0.3, uknob 0.45, uknob 0.15, uknob 0.45]
```


```haskell
main = dac $ hlifts 
    (\[p, t, x, y, z]-> mixAt 0.28 smallHall2 $ at fromMono $ 
        midi $ onMsg (
            mlp (800) 0.45 . 
            hp 50 10 . 
            bat (\x -> sum [
                bp (300 + 200 * (p - 0.5)) 60 x, 
                bp (700 + 300 * (t - 0.5)) 40 x, 
                bp (1500 + 600 * (x - 0.5)) 30 x]) . 
            mul (leg 0.5 0.5 0.8 0.1). 
            saw . 
            (\x -> x * (1 + y * 0.07 * linseg [0, 0.2, 0.3, 0.2, 0.85, 5, 1] * osc (12 * z))))) 
    [uknob 0.25, uknob 0.3, uknob 0.45, uknob 0.15, uknob 0.45]
```

-}
q = 
    hlifts (\[p, t, x, y, z]-> mixAt 0.28 smallHall2 $ at fromMono $ midi $ 
        onMsg (\(amp, cps) -> ($ cps) ((mul $ sig amp) . at (
            mlp (800) 0.45 . 
            hp 50 10 . 
            bat (\x -> sum [
                bp (300 + 200 * (p - 0.5)) 60 x, 
                bp (700 + 300 * (t - 0.5)) 40 x, 
                bp (1500 + 600 * (x - 0.5)) 30 x]) . 
        mul (leg (0.1 + 0.5 * (1 - amp)) 0.5 0.8 0.1)) . 
        rndSaw . 
        (\x -> x * (1 + y * 0.07 * linseg [0, 0.2, 0.3, 0.2, 0.85, 5, 1] * osc (12 * z)))))) 
    [uknob 0.25, uknob 0.3, uknob 0.45, uknob 0.15, uknob 0.45]
