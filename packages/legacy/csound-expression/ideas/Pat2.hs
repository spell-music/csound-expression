
type Gen a = Producer a IO ()

data Pat a 
	= Val a
	| Len Int (Pat a)
	| OneOf [Pat a]	
	| Path [Pat a]

-- switch ???  Switch (Pat Int) [Pat a] 
	
lin xs = Path $ fmap (Len 1 . Val) xs

runPat :: Pat a -> Gen a

lin [1,1,2,3] >>= \n -> [] !! n

