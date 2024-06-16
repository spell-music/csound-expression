module Csound.Typed.Opcode.Miscellaneous (
    
    
    
    directory, fareylen, fareyleni, framebuffer, modmatrix, nchnls_hw, olabuffer, pwd, select, system_i, system, tableshuffle, tableshufflei) where

import Control.Monad.Trans.Class
import Control.Monad
import Csound.Dynamic
import Csound.Typed

-- 

-- | 
-- Reads a directory and outputs to a string array a list of file names.
--
-- Reads a directory for files and passes them to a string array. Users can set the file type by passing a file extension as a string.
--
-- > SFiles[]  directory  SDirectory[, SExtention]
--
-- csound doc: <https://csound.com/docs/manual/directory.html>
directory ::  Str -> Str
directory b1 =
  Str $ f <$> unStr b1
  where
    f a1 = opcs "directory" [(Sr,[Sr,Sr])] [a1]

-- | 
-- returns the length of a Farey Sequence.
--
-- This opcode can be used in conjunction with GENfarey.
-- It calculates the length of Farey Sequence Fn. Its length is given by:
-- |Fn| = 1 + SUM over n phi(m) 
-- where phi(m) is Euler's totient function, which gives the number of integers â¤ m that are coprime to m.
--
-- > kfl  fareylen  kfn
--
-- csound doc: <https://csound.com/docs/manual/fareylen.html>
fareylen ::  Tab -> Sig
fareylen b1 =
  Sig $ f <$> unTab b1
  where
    f a1 = opcs "fareylen" [(Kr,[Kr])] [a1]

-- | 
-- returns the length of a Farey Sequence.
--
-- This opcode can be used in conjunction with GENfarey.
-- It calculates the length of Farey Sequence Fn. Its length is given by:
-- |Fn| = 1 + SUM over n phi(m) 
-- where phi(m) is Euler's totient function, which gives the number of integers â¤ m that are coprime to m.
--
-- > ifl  fareyleni  ifn
--
-- csound doc: <https://csound.com/docs/manual/fareyleni.html>
fareyleni ::  Tab -> D
fareyleni b1 =
  D $ f <$> unTab b1
  where
    f a1 = opcs "fareyleni" [(Ir,[Ir])] [a1]

-- | 

--
-- > kout[]  framebuffer  ain, isize
-- > aout  framebuffer  kin, isize
--
-- csound doc: <https://csound.com/docs/manual/framebuffer.html>
framebuffer ::  Sig -> D -> Sig
framebuffer b1 b2 =
  Sig $ f <$> unSig b1 <*> unD b2
  where
    f a1 a2 = opcs "framebuffer" [(Kr,[Ar,Ir]),(Ar,[Kr,Ir])] [a1,a2]

-- | 
-- Modulation matrix opcode with optimizations for sparse matrices.
--
-- The opcode can be used to let a large number of k-rate modulator
--       variables modulate a large number of k-rate parameter variables,
--       with arbitrary scaling of each modulator-to-parameter
--       connection.  Csound ftables are used to hold both the input
--       (parameter)  variables, the modulator variables, and the scaling
--       coefficients. Output variables are written to another Csound ftable.
--
-- >  modmatrix  iresfn, isrcmodfn, isrcparmfn, imodscale, inum_mod, \\
-- >     inum_parm, kupdate
--
-- csound doc: <https://csound.com/docs/manual/modmatrix.html>
modmatrix ::  Tab -> Tab -> Tab -> D -> D -> D -> Sig -> SE ()
modmatrix b1 b2 b3 b4 b5 b6 b7 =
  SE $ join $ f <$> (lift . unTab) b1 <*> (lift . unTab) b2 <*> (lift . unTab) b3 <*> (lift . unD) b4 <*> (lift . unD) b5 <*> (lift . unD) b6 <*> (lift . unSig) b7
  where
    f a1 a2 a3 a4 a5 a6 a7 = opcsDep_ "modmatrix" [(Xr,[Ir,Ir,Ir,Ir,Ir,Ir,Kr])] [a1
                                                                                ,a2
                                                                                ,a3
                                                                                ,a4
                                                                                ,a5
                                                                                ,a6
                                                                                ,a7]

-- | 

--
-- > idacc,iadcc  nchnls_hw 
--
-- csound doc: <https://csound.com/docs/manual/nchnls_hw.html>
nchnls_hw ::   (D,D)
nchnls_hw  =
  pureTuple $ return $ f 
  where
    f  = mopcs "nchnls_hw" ([Ir,Ir],[]) []

-- | 

--
-- > aout  olabuffer  kin, ioverlap
--
-- csound doc: <https://csound.com/docs/manual/olabuffer.html>
olabuffer ::  Sig -> D -> Sig
olabuffer b1 b2 =
  Sig $ f <$> unSig b1 <*> unD b2
  where
    f a1 a2 = opcs "olabuffer" [(Ar,[Kr,Ir])] [a1,a2]

-- | 
-- Asks the underlying operating system for the current directory
--       name as a string.
--
-- pwd call the operating system to determine
--       the current directory (folder).  pwd runs
--       at i-time only.
--
-- > Sres  pwd 
--
-- csound doc: <https://csound.com/docs/manual/pwd.html>
pwd ::   Str
pwd  =
  Str $ return $ f 
  where
    f  = opcs "pwd" [(Sr,[])] []

-- | 
-- Select sample value based on audio-rate comparisons.
--
-- Select sample value from three based on audio-rate comparisons of
--       two signals.
--
-- > aout  select  a1, a2, aless, aequal, amore
--
-- csound doc: <https://csound.com/docs/manual/select.html>
select ::  Sig -> Sig -> Sig -> Sig -> Sig -> Sig
select b1 b2 b3 b4 b5 =
  Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5
  where
    f a1 a2 a3 a4 a5 = opcs "select" [(Ar,[Ar,Ar,Ar,Ar,Ar])] [a1,a2,a3,a4,a5]

-- | 
-- Call an external program via the system call
--
-- system and system_i call
--     any external command understood by the operating system, similarly
--     to the C function     system(). system_i runs
--     at i-time only, while 
--       system runs both at initialization and
--       performance time.
--
-- > ires  system_i  itrig, Scmd, [inowait]
--
-- csound doc: <https://csound.com/docs/manual/system.html>
system_i ::  D -> Str -> D
system_i b1 b2 =
  D $ f <$> unD b1 <*> unStr b2
  where
    f a1 a2 = opcs "system_i" [(Ir,[Ir,Sr,Ir])] [a1,a2]

-- | 
-- Call an external program via the system call
--
-- system and system_i call
--     any external command understood by the operating system, similarly
--     to the C function     system(). system_i runs
--     at i-time only, while 
--       system runs both at initialization and
--       performance time.
--
-- > kres  system  ktrig, Scmd, [knowait]
--
-- csound doc: <https://csound.com/docs/manual/system.html>
system ::  Sig -> Str -> Sig
system b1 b2 =
  Sig $ f <$> unSig b1 <*> unStr b2
  where
    f a1 a2 = opcs "system" [(Kr,[Kr,Sr,Kr])] [a1,a2]

-- | 
-- shuffles the content of a function table so that each element of the source
--       table is put into a different random position.
--
-- This opcode can be used in order to shuffle the content of
--       function tables into a random order but without loosing any of
--       the elements. Imagine shuffling a deck of cards. Each element of
--       the table is copied to a different random position. If that
--       position was already chosen before then the next free position
--       is chosen. The length of the table remains the same.
--
-- >  tableshuffle  ktablenum
--
-- csound doc: <https://csound.com/docs/manual/tableshuffle.html>
tableshuffle ::  Sig -> SE ()
tableshuffle b1 =
  SE $ join $ f <$> (lift . unSig) b1
  where
    f a1 = opcsDep_ "tableshuffle" [(Xr,[Kr])] [a1]

-- | 
-- shuffles the content of a function table so that each element of the source
--       table is put into a different random position.
--
-- This opcode can be used in order to shuffle the content of
--       function tables into a random order but without loosing any of
--       the elements. Imagine shuffling a deck of cards. Each element of
--       the table is copied to a different random position. If that
--       position was already chosen before then the next free position
--       is chosen. The length of the table remains the same.
--
-- >  tableshufflei  itablenum
--
-- csound doc: <https://csound.com/docs/manual/tableshuffle.html>
tableshufflei ::  D -> SE ()
tableshufflei b1 =
  SE $ join $ f <$> (lift . unD) b1
  where
    f a1 = opcsDep_ "tableshufflei" [(Xr,[Ir])] [a1]