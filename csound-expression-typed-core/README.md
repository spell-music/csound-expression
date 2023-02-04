Typed core for Csound
================================

This pacckage offers very small typed core that wrapps dynamic expressions to typed ones.
It's tiny but augmented with csound-expression-opcodes it offers almost full coverage of the Csound language.

The csound-expression uses it to build functional model of music.
This package also can be used on it's own if you like the Csound way of doing stuff.

Let's briefly discuss the core.

Types
---------------------------

We have primitive types:

* `Sig` - control or audio rate signals (it depends on context)
* `D`   - init-rate constants 
* `Tab` - Csound tables (GEN-routines)
* `Str` - string type
* `Spec` - spectrum type (used with FFT-functions, pvs family of opcodes)

Also we have arrays:

* `Arr` - imperative arrays with arbitrary number of sizes
* `PureArr` - single dimension read-only array. It can be useful to create fast access tables.

We have classes do distinguish types:

* `Tuple` - tuples for primitive types
* `Arg` - tuples for types that can be used at initialization stage.

We can create mutable references which are global to the whole setup or local to the 
single instance of the instrument.

We can create reference with one of the functions:

```haskell
newRef, newCtrlRef :: Tuple a => a -> SE (Ref a)
```

The first argument is initial value, the result is wrapped in `SE` monad.
`SE` stands for side-effect. It's Csound's synonym for `IO`-monad.
Anythning that produces side effects: randomness, updates of the tables, set up globals and stuff like that.
If reference is defined inside instrument definition then it's local. If it's defined on the top level
it becomes global reference.

The `newCtrlRef` differs from `newRef` that in case of `Sig` or `D` it runs on control rate.

We can work with references by:

```haskell
writeRef :: Ref a -> a -> SE ()
readRef  :: Ref a -> SE a
```

it works like Hasell's mutable variables: IORef, TVar, etc.


Csound model
-----------------------------

Let's briefly review the Csound model to desribe music.
In Csound we have instruments and notes.
First we define instruments and then we can trigger them with notes.

Also we setup some globals values like audio sample rate, number of channels, JACK connections and so on.
To setup globals see the type `Options` in the docs.

So we have instruments and notes. Let's look at them

### Instrument

An instrument is a procedure that takes some tuple of constants as argument and does something
for some time:

```haskell
Arg a => a -> SE ()
```

The class Arg is restricted to various tuples of primitive values, that can be used 
as note parameters. There are only few of them: `D` numbers, `Str` strings and `Tab` tables.
an instrument has two stages:

* initialization - Csound goes over all constants and evaluates them
* performance at control rate - csound goes over and over control rate functions and updates signals.

We have only one function to define an instrument:

```haskell
newInstr :: Arg a => (a -> SE ()) -> Se (InstrId a)
```

It takes in a procedure and returns a reference to the instrument. 
It's a name for the instrument that can be used to trigger a note.
Note that it's typed with a type of the instrument input.

Let's look at an example of simple instrument:

```haskell
pureSine :: D -> SE ()
pureSine cps = outs res res
  where
    res = oscil 1 (sig cps) (sines [1])
```

This instrument creates pure sine wave (`oscil`), sends it to the global output (`outs`).
The output can be sent to speakers or written on disk to wav file. It depends on settings.
Don't bother with the details of concrete functions we can just look at the typical structure of the instrument.
We can create a name for it do trigger it with notes:

```haskell
main = runSE $ do
  sineId <- newInstr pureSine
```

### Note

So now we have defined an instrument. Let's play music with it.
To do it we need to trigger it with notes. A note is just a tuple:

```haskell
data Note a = Note
  { noteInstr    :: InstrId a   -- which instrument to trigger
  , noteStart    :: D           -- delay from current time in seconds
  , noteDuration :: D           -- how long instrument will play
  , noteArgs     :: a           -- initial arguments to the instrument
  }
```

we can trigger a note with:

```haskell
play :: Arg a => Note a -> SE ()
```

So to play a tuing fork for 5 seconds we can use:

```haskell
main = runSE $ do
  sineId <- newInstr pureSine
  play (Note sineId 0 5 440)
```

If we want to pass several parameters we can use tuples.

That's it. It's all you need to know to start making music with Csound's core.
Also if you like an easy to use functional approach you can use csound-expression library. 
Which adds functional model on top of this tiny core. And let's you treat instruments
like functions from inits to signals and we can also ues FRP-style events and other nice functional
programming stuff.

But the core is simple to grasp.

### Where to find audio-processors

The audio units like `oscil`, `outs` are defined in the csound-expression-opcodes package.
You can study the hackage docs and every opcode has reference to the original Csound manual documentation.
In fact those functions are genrated from the Csound's docs. Csound is very feature rich language.
It has more than 1000 different audio units of high quality. Enjoy!
I also recommend reading Csound Floss manual to know the Csound's details.

### Some peculiar stuff

Note that every instance of the instrument has it's own local scope of variables and they can not be shared
between instrument. To share values we can create global variables or use channel opcodes (see the Csound docs).
That's why we could define local and global variables /refs.

Unfortunately this distinction can lead to the valid Haskell code that is invalid in Csound.
In Csound we can not have nested instruments. That is we first define all our instruments and then we can
trigger them with notes. But the compositional style of SE-monad allows us to define instruments within 
instruments and share references between them. This will lead to bad Csound code.
Use only top-level references inside several instruments to share the values!

but thsi property is relaxed in the functional moel of csound-expression package.
In that package we can define nested instruments.
