-- | A gallery of the percussive sounds.
module Csound.Catalog.Drum(
    -- * Hans Mikelson Drum kit
    hmBd1, hmBd2, hmBd3, hmSn1, hmSn2, hmSweeo, hmBoink, hmOhh, hmChh, hmCr, hmClap,

    -- * Tr808 Drum kit
    trBd, trSn, trOhh, trChh, trHtom, trMtom, trLtom, trCym, trCl, trRim, trMar, trHcon, trMcon, trLcon,

    -- * Korg MiniPops Drum kit
    mpBd, mpSn1, mpSn2, mpRim, mpCym1, mpCym2, mpBon1, mpBon2, mpBon3, mpCl, mpCow, mpGro, mpMar, mpQj, mpTam,

    -- * Csound percussive models kit    
    boo, gro, sbells, tam, cab, crun, shake, spaper, 
) where

import Csound.Base
import Csound.Sam

import qualified Csound.Catalog.Drum.Hm       as H
import qualified Csound.Catalog.Drum.Tr808    as T
import qualified Csound.Catalog.Drum.MiniPops as M


hmBd1   = H.bd1
hmBd2   = H.bd2
hmBd3   = H.bd3
hmSn1   = H.sn1
hmSn2   = H.sn2
hmSweeo = H.sweep
hmBoink = H.boink
hmOhh   = H.ohh
hmChh   = H.chh
hmCr    = H.cr
hmClap  = H.clap

trBd   = T.bd
trSn   = T.sn
trOhh  = T.ohh
trChh  = T.chh
trHtom = T.htom
trMtom = T.mtom
trLtom = T.ltom
trCym  = T.cym
trCl   = T.cl
trRim  = T.rim
trMar  = T.mar
trHcon = T.hcon
trMcon = T.mcon
trLcon = T.lcon

mpBd   = M.bd
mpSn1  = M.sn1
mpSn2  = M.sn2
mpRim  = M.rim
mpCym1 = M.cym1
mpCym2 = M.cym2
mpBon1 = M.bon1
mpBon2 = M.bon2
mpBon3 = M.bon3
mpCl   = M.cl
mpCow  = M.cow
mpGro  = M.gro
mpMar  = M.mar
mpQj   = M.qj
mpTam  = M.tam

mkSam = limSam 1 

boo = mkSam $ bamboo 1 0.01
gro = mkSam $ guiro  1 0.01
sbells = mkSam $ sleighbells 1 0.01
tam = mkSam $ tambourine 1 0.01
cab = mkSam $ cabasa 1 0.01
crun = mkSam $ crunch 1 0.1
shake cps = mkSam $ shaker 1 cps 8 0.999 100 `withD` 0
spaper = mkSam $ sandpaper 1 0.01
