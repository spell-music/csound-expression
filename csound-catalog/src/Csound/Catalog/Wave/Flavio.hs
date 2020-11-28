module Csound.Catalog.Wave.Flavio(
	amFlavio, fmFlavio, simpleSust, simpleFading
) where

import Data.List
import Control.Monad

import Csound.Base 

icero     =  0.000001
icasi     =  0.0001	 		

-- epiano-s

-- irel1 = 16
amFlavio irel1 cps = aout
	where 		
  		irel1     = 16  		
  		iamf      = 1
  		irel2     = irel1 * 0.7
	 	kamp      = expsegr [icero, 0.05, 1, 1, 0.7, irel1, icasi, irel2, icero]  irel2 icero 	 	
	 	aam       = kamp * osc (iamf * cps)
	 	aout      = aam  * osc cps

-- irel1 = 6, ifm = (2, 7), 
fmFlavio irel1 ifm cps = aout
	where		
		irel2   = irel1 * 0.5	
		idec    = 1
		iatt    = 0.01

		(iidx1, iidx2, iidx3, iidx4, iidx5) = (4,      4,      4,     4,      3)

		kamp    = expsegr    [icero, iatt, 1, idec, 0.7, irel1, icasi, irel2, icero] irel2 icero
		kidx    = linsegr    [iidx1, iatt, iidx2, idec, iidx3, irel1, iidx4, irel2, iidx5] irel2 iidx5

		afrq 	= kidx * osc (ifm * cps)
		aout    = kamp * osc (cps * (1 + afrq))


simpleSust   = genSimple 0.25  0.1
simpleFading = genSimple icasi icero

genSimple isust1 isust2 irel (amp, dcps) = do
	aleft  <- fmap pure $ random 1 (11  * sig amp)
	aright <- fmap pure $ random 1 (10.5  * sig amp)
	return (aleft, aright)
	where
		cps = sig dcps
		pure ichr = aout
			where
				irel1 = irel * (0.5 + amp)
				iatt    = 0.01
				idec    = 1
				irel2   = 0.75 * irel1

				-- kamp    = expsegr    [1, idec, 0.7, irel1, icasi, irel2, icero] irel2 icero
				kamp    = expsegr    [1, idec, 0.7, irel1, isust1, irel2, isust2] irel2 icero
				kcf     = 2 * sig amp * linsegr    [3000, irel1 + 1, 500] irel2 500
				a3      = kamp * (osc (cps - ichr) + osc (cps - ichr))* 0.5

				aout = blp kcf a3

