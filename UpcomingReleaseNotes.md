CE 5.3
=================

More generic types for sending to speakers
-----------------------

More inputs and outputs for rendering to speakers (the type becomes more generic)


User defined options
----------------------------------

User defined options. now user can create his own default settings.
In HOME directory start ghci and run:

~~~haskell
> saveUserOptions options
~~~

The line creates a file `.csound-expression-rc` in the current directory.
When options are rendered this file is read for defaults.

For example by default sample rate is set to 44100.
This is a good value for real-time performance on modest computers.
But if you have a powerful computer or use Csound mostly to write
tracks at home and you are all for precision and accuracy rather then for
speed of performance. You can set your own sample rate like this:

~~~haskell
> saveUserOptions (setRates 48000 10)
~~~

another example for Linux users.
By default Csound uses pulseaudio.  But if you want to use JACK by default
you can just execute in the ghci at your home directory:

~~~haskell
> saveUserOptions (setJack "csound")
~~~

And now each time you invoke the csound rendering function
it's going to be wrapped in the JACK audio-unit with name csound.
The same things can be set for midi interfaces or output message level.

If we run the `saveUserOptions` command at the home directory
the options become global for the whole system.

But we can also set default options on per project basis.
If the CE finds the file like this in your current directory
it uses the options from it instead of the options from the
user's HOME directory.

Also we can use this trick to shadow the global settings if
we don't want to use them for the current project.
Just  execute:

~~~haskell
> saveUserOptions def
~~~

to fall back to the CE defaults.


Bug fixes:
--------------------------------

Bug fix for sendOsc