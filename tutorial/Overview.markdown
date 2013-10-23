Overview of the library
==================================

There are many functions in the library. Let's list the most usefull ones:

## The converters:

~~~
double :: Double -> D
int    :: Int    -> D
text   :: String -> Str
sig    :: D -> Sig
~~~

## The primitive wave forms 

They convert the time varied frequency to the signal: 

~~~
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
(usefull with midi). Two additional parameters are:
time of the release and the final value. Exponential envelopes
should be above zero (we can use the small numbers to imitate the zero)

~~~
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
for `bp` and `br` is a band-width.

~~~
lp  :: Sig -> Sig -> Sig            -- low pass
hp  :: Sig -> Sig -> Sig            -- high pass
bp  :: Sig -> Sig -> Sig -> Sig     -- band pass
br  :: Sig -> Sig -> Sig -> Sig     -- band reject
~~~

There are butterworth variants of the filters: `blp`, `bhp`, `bbp`, `bbr`.

## Reverberation

~~~
nreverb  :: Sig -> Sig -> Sig
~~~

It takes a signal to process, the delay time and the speed of decay (0 to 1).

~~~
reverbsc :: Tuple a => Sig -> Sig -> Sig -> Sig -> a
reverbsc aleft aright feedBackLevel cutOffFrequency
~~~

It's a stereo processing. It takes a two signals the feedback level (0 to 1) 
and cut off frequency of the low pass filter (usually it's 10000).


## Reading the files

~~~
mp3in   :: Tuple a => Str -> a
diskin2 :: Tuple a => Str -> Sig -> a
~~~

The function `diskin2` reads only wav files. The additional parameter is
the speed of the playback.


## Constructing the arrays

~~~
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

~~~
noise   :: Sig -> Sig -> SE Sig     -- white noise
randi   :: Sig -> Sig -> SE Sig     -- random linear segments
pinkish :: Sig -> SE Sig            -- pink noise
~~~

## Events

Defined in the module `Csound.Control.Evt`

The event stream `Evt` is a `Functor` and `Monoid`

~~~
metroE   :: Sig -> Evt ()
filterE  :: (a -> BoolD) -> Evt a -> Evt a
repeatE  :: a -> Evt b -> Evt a
cycleE   :: (Arg a) => [a] -> Evt b -> Evt a
oneOf    :: (Arg a) => [a] -> Evt b -> Evt a
randSkip :: D -> Evt a -> Evt a
~~~

## Invoking the instruments

~~~
-- renderes the midi instrument    
midi    :: Sigs a => (Msg -> SE a) -> a         

-- renderes the midi instrument 
-- on the given channel
midin   :: Sigs a => Int -> (Msg -> SE a) -> a  

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
~~~

## Rendering the Csound files

Defined in the module `Csound.IO`

The type class `RenderCsd` contains the sings that can be rendered to file.
It's something that produces the sound or triggers the csound procedures.

~~~
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

~~~
options = mconcat [setRates 44800 64, setDac, setAdc]

main = cvsdBy options asignal
~~~

The most common options:

~~~
-- sets the sample rate and the block size
setRates :: Int -> Int -> Options

-- sets the buffer sizes (the define the granylarity of the real-time performance)
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

~~~
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



