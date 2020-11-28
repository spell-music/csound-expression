module Csound.Typed.Opcode.Miscellaneous (
    
    
    
    directory, fareylen, fareyleni, modmatrix, pwd, select, system_i, system, tableshuffle, tableshufflei) where

import Control.Applicative
import Control.Monad.Trans.Class
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
-- csound doc: <http://csound.com/docs/manual/directory.html>
directory ::  Str -> Str
directory b1 = Str $ f <$> unStr b1
    where f a1 = opcs "directory" [(Sr,[Sr,Sr])] [a1]

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
-- csound doc: <http://csound.com/docs/manual/fareylen.html>
fareylen ::  Tab -> Sig
fareylen b1 = Sig $ f <$> unTab b1
    where f a1 = opcs "fareylen" [(Kr,[Kr])] [a1]

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
-- csound doc: <http://csound.com/docs/manual/fareyleni.html>
fareyleni ::  Tab -> D
fareyleni b1 = D $ f <$> unTab b1
    where f a1 = opcs "fareyleni" [(Ir,[Ir])] [a1]

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
-- csound doc: <http://csound.com/docs/manual/modmatrix.html>
modmatrix ::  Tab -> Tab -> Tab -> D -> D -> D -> Sig -> SE ()
modmatrix b1 b2 b3 b4 b5 b6 b7 = SE $ (depT_ =<<) $ lift $ f <$> unTab b1 <*> unTab b2 <*> unTab b3 <*> unD b4 <*> unD b5 <*> unD b6 <*> unSig b7
    where f a1 a2 a3 a4 a5 a6 a7 = opcs "modmatrix" [(Xr,[Ir,Ir,Ir,Ir,Ir,Ir,Kr])] [a1
                                                                                  ,a2
                                                                                  ,a3
                                                                                  ,a4
                                                                                  ,a5
                                                                                  ,a6
                                                                                  ,a7]

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
-- csound doc: <http://csound.com/docs/manual/pwd.html>
pwd ::   Str
pwd  = Str $ return $ f 
    where f  = opcs "pwd" [(Sr,[])] []

-- | 
-- Select sample value based on audio-rate comparisons.
--
-- Select sample value from three based on audio-rate comparisons of
--       two signals.
--
-- > aout  select  a1, a2, aless, aequal, amore
--
-- csound doc: <http://csound.com/docs/manual/select.html>
select ::  Sig -> Sig -> Sig -> Sig -> Sig -> Sig
select b1 b2 b3 b4 b5 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unSig b4 <*> unSig b5
    where f a1 a2 a3 a4 a5 = opcs "select" [(Ar,[Ar,Ar,Ar,Ar,Ar])] [a1,a2,a3,a4,a5]

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
-- csound doc: <http://csound.com/docs/manual/system.html>
system_i ::  D -> Str -> D
system_i b1 b2 = D $ f <$> unD b1 <*> unStr b2
    where f a1 a2 = opcs "system_i" [(Ir,[Ir,Sr,Ir])] [a1,a2]

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
-- csound doc: <http://csound.com/docs/manual/system.html>
system ::  Sig -> Str -> Sig
system b1 b2 = Sig $ f <$> unSig b1 <*> unStr b2
    where f a1 a2 = opcs "system" [(Kr,[Kr,Sr,Kr])] [a1,a2]

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
-- csound doc: <http://csound.com/docs/manual/tableshuffle.html>
tableshuffle ::  Sig -> SE ()
tableshuffle b1 = SE $ (depT_ =<<) $ lift $ f <$> unSig b1
    where f a1 = opcs "tableshuffle" [(Xr,[Kr])] [a1]

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
-- csound doc: <http://csound.com/docs/manual/tableshuffle.html>
tableshufflei ::  D -> SE ()
tableshufflei b1 = SE $ (depT_ =<<) $ lift $ f <$> unD b1
    where f a1 = opcs "tableshufflei" [(Xr,[Ir])] [a1]