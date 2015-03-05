Overview of the library
==================================

There are many functions in the library. Let's list the most useful ones:

## The converters:

~~~haskell
double :: Double -> D
int    :: Int    -> D
text   :: String -> Str
sig    :: D -> Sig
~~~

## The primitive wave forms 

They convert the time varied frequency to the signal: 

~~~haskell
osc :: Sig -> Sig       -- pure tone
saw :: Sig -> Sig       -- sawtooth
sqr :: Sig -> Sig       -- square wave
tri :: Sig -> Sig       -- triangle wave

oscBy :: Tab -> Sig -> Sig     -- oscillator with a specified wave
blosc :: Tab -> Sig -> Sig     -- a generic band-limited oscillator
~~~

There are unipolar variants of the waveforms (a unipolar signal varies from 0 to 1):
`uosc`, `usaw`, `usqr`, etc

## Envelopes:

Release is a time to linger the signal after note is over
(useful with midi). Two additional parameters are:
time of the release and the final value. Exponential envelopes
should be above zero (we can use the small numbers to imitate the zero)

~~~haskell
linseg :: [D] -> Sig                -- linear envelope
expseg :: [D] -> Sig                -- exponential envelope

                                    -- with release:
linsegr :: [D] -> D -> D -> Sig     -- linear envelope 
expsegr :: [D] -> D -> D -> Sig     -- exponential envelope
~~~

## Filters

Parameters: the last argument is always the signal to filter 
the first parameter is cut-off frequency for `lp` and `hp`
and the center frequency for `bp` and `br`. The second argument
is a band-width (resonance).

~~~haskell
lp  :: Sig -> Sig -> Sig -> Sig     -- low pass
hp  :: Sig -> Sig -> Sig -> Sig     -- high pass
bp  :: Sig -> Sig -> Sig -> Sig     -- band pass
br  :: Sig -> Sig -> Sig -> Sig     -- band reject
~~~

There are Butterworth variants of the filters: `blp`, `bhp`, `bbp`, `bbr`.

## Reverberation

~~~haskell
nreverb  :: Sig -> Sig -> Sig
~~~

It takes a signal to process, the delay time and the speed of decay (0 to 1).

~~~haskell
reverbsc :: Tuple a => Sig -> Sig -> Sig -> Sig -> a
reverbsc aleft aright feedBackLevel cutOffFrequency
~~~

It's a stereo processing. It takes a two signals the feedback level (0 to 1) 
and cut off frequency of the low pass filter (usually it's 10000).


## Reading the files


~~~haskell
readSnd :: String -> (Sig, Sig)  -- read once
loopSnd :: String -> (Sig, Sig)  -- read in loop
loopSndBy :: D -> String -> (Sig, Sig) -- read in loop with given period (in seconds)
~~~

If we have a wav (or aiff) file we can read it with the given speed (the first argument):

~~~haskell
readWav :: Sig -> String -> (Sig, Sig)
loopWav :: Sig -> String -> (Sig, Sig)
~~~

Note that speed is a Signal and can vary with time. Use negative
values to read in reverse. When speed equals one it's normal reading.

Low-level functions:

~~~haskell
mp3in   :: Tuple a => Str -> a
diskin2 :: Tuple a => Str -> Sig -> a
~~~

The function `diskin2` reads only wav-files. The additional parameter is
the speed of the playback.


## Constructing the arrays

~~~haskell
sines :: [Double] -> Tab    -- list of the partials to sine harmonics 
lins  :: [Double] -> Tab    -- array of linear segments 
                            -- (parameters are like in `linseg`)
exps  :: [Double] -> Tab    -- array of exponential segments
                            -- (parameters are like in `expseg`)                        
~~~             

## Noises

The first parameter is the amplitude, the second one is beta
for the low pass filter (-1 to 1) for the function `noise`, and the frequency of the
random values for the function `randi`.

~~~haskell
noise   :: Sig -> Sig -> SE Sig     -- white noise
randi   :: Sig -> Sig -> SE Sig     -- random linear segments
pinkish :: Sig -> SE Sig            -- pink noise
~~~

Simplified noises:

~~~haskell
white, pink :: SE Sig
~~~

## Events

Defined in the module `Csound.Control.Evt`

The event stream `Evt` is a `Functor` and `Monoid`

~~~haskell
metroE   :: Sig -> Evt ()
filterE  :: (a -> BoolD) -> Evt a -> Evt a
repeatE  :: a -> Evt b -> Evt a
cycleE   :: (Arg a) => [a] -> Evt b -> Evt a
oneOf    :: (Arg a) => [a] -> Evt b -> Evt a
randSkip :: D -> Evt a -> Evt a
~~~

## Invoking the instruments

~~~haskell
-- renderes the midi instrument    
midi    :: Sigs a => (Msg -> SE a) -> SE a         

-- renderes the midi instrument 
-- on the given channel
midin   :: Sigs a => Int -> (Msg -> SE a) -> SE a  

-- mix the signals from score
mix     :: (CsdSco f, Sigs a) => f (Mix a) -> a 

-- invokes an instrument on the score
sco     :: (CsdSco f, Arg a, Sigs b) => (a -> SE b) -> f a -> f (Mix b)

-- applies an effect to the score
eff     :: (CsdSco f, Sigs a, Sigs b) => (a -> SE b) -> f (Mix a) -> f (Mix b)

-- invokes an instrument on the event stream 

-- event stream contains duration of the note
sched   :: (Arg a, Sigs b) => (a -> SE b) -> Evt (D, a) -> b

-- event stream contains delay time and duration of the note
trig    :: (Arg a, Sigs b) => (a -> SE b) -> Evt (D, D, a) -> b

-- triggers an instrument with the first event
-- stream and holds the note while the second event stream is silent
schedUntil :: (Arg a, Sigs b) => (a -> SE b) -> Evt a -> Evt c -> b
~~~

## Truncating/repeating the signal

~~~haskell
takeSnd   :: Sigs a => Double -> a -> a   
repeatSnd :: Sigs a => D      -> a -> a
~~~

With `takeSnd` we can truncate the signals to the given amount of seconds.
We use only first `n`-seconds from the signals. The `repeatSnd` repeats
the signal with the given period.

## Rendering the Csound files

Defined in the module `Csound.IO`

The type class `RenderCsd` contains the sings that can be rendered to file.
It's something that produces the sound or triggers the Csound procedures.

~~~haskell
-- plays a signal in real time 
dac     :: RenderCsd a => a -> IO ()            

-- plays a signal in real time with virtual midi-keyboard
vdac     :: RenderCsd a => a -> IO ()            

-- saves the csound file with the given name
writeCsd :: RenderCsd a => String -> a -> IO ()

-- saves the csound file to 'tmp.csd' and invokes the csound on it
csd :: RenderCsd a => a -> IO ()
~~~

## Options

Defined in the module `Csound.Options`.

With options we can set the global settings of the rendering process.
The type `Options` is a monoid with the meaning that we can 
concatenate partially defined options and get more specified ones.

To specify the options we use the rendering functions with suffix `By`:

~~~haskell
options = mconcat [setRates 44800 64, setDac, setAdc]

main = csdBy options asignal
~~~

The most common options:

~~~haskell
-- sets the sample rate and the block size
setRates :: Int -> Int -> Options

-- sets the buffer sizes (the define the granularity of the real-time performance)
setBufs :: Int -> Int -> Options 

-- where to direct the output
setOutput :: String -> Options

-- from where to recieve the input
setInput :: String -> Options

-- sets the specific output and input

-- directs the output to dac
setDac :: Options

-- recieves the input from adc.
setAdc :: Options

setThru = mappend setDac setAdc
~~~

## Score

Defined in the module `Temporal.Music.Score` from the package `temporal-music-notation`:

~~~haskell
-- Constructs a score with the single note (it lasts for one second)
temp :: a -> Score a

-- Constructs a score that contains nothing and lasts for some time.
rest :: Double -> Score a

-- Stretches the score in the time domain with the given coefficient.
-- It gets faster or slower.
str  :: Double -> Score a -> Score a

-- Delays all events with the given amount of time.
del  :: Double -> Score a -> Score a

-- A sequential composition of scores. It's short for melody.
-- It plays the scores one after the other.
mel  :: [Score a] -> Score a

-- A parallel composition. It's short for harmony.
-- It plays all scores at the same time.
har  :: [Score a] -> Score a

-- Repeats the score several times.
loop :: Int -> Score a -> Score a
~~~

## GUI

Main elements:

~~~haskell
--        Label     Diapason   Init      Result   
--                  of the     value
--                  value

knob   :: String -> ValSpan -> Double -> Source Sig
slider :: String -> ValSpan -> Double -> Source Sig

button :: String -> Source (Evt Unit)
toggle :: String -> Source (Evt D)

--                      Label     Alternatives     Id of the
--                                                 default
--                                                 value 
radioButton :: Arg a => String -> [(String, a)] -> Int -> Source (Evt a)

-- shows a static text
box    :: String -> Display
~~~

Creating value spans:

Linear and exponential spans with the give bounds:

~~~haskell
linSpan :: Double -> Double -> ValSpan
expSpan :: Double -> Double -> ValSpan
~~~

The linear unit span:

~~~haskell
uspan :: ValSpan
uspan = linSpan 0 1
~~~

### Layout

~~~haskell
-- horizontal placement
hor :: [Gui] -> Gui

-- vertical placement
ver :: [Gui] -> Gui

-- scaling of the element within the group
-- (element is contained in the horizontal 
-- or vertical container)
sca :: Double -> Gui -> Gui
~~~

### Creating a windows with GUIs

Creates a single window

~~~haskell
panel :: Gui -> SE ()
~~~

Creates a single window that is listening for keyboard events

~~~haskell
keyPanel :: Gui -> SE ()
~~~

Creates a single windows and we can specify title and size of the window:

~~~haskell
panelBy :: String -> Maybe Rect -> Gui -> SE ()
~~~

## Keyboard events

~~~haskell
data KeyEvt = Press Key | Release Key
data Key = CharKey | F1 | F2 | ...

keyIn   :: KeyEvt -> Evt Unit
~~~

Press and release a simple key:

~~~haskell
charOn  :: Char   -> Evt Unit
charOff :: Char   -> Evt Unit
~~~

We should create a window to be able to listen on keyboard events
with the function `keyPanel` or `keyPanelBy`.

------------------------------------------------------------------

* [Home](https://github.com/anton-k/csound-expression/blob/master/tutorial/Index.md)