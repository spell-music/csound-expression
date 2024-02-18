{-# Language RankNTypes, ExistentialQuantification #-}
import Csound.Base

data Gen a = forall s . Tuple s => Gen 
    { genInit    :: SE s
    , genUpdate  :: s -> SE s
    , genObserve :: s -> SE a
    }

instance Functor Gen where 
    fmap f (Gen init update observe) = Gen init update (fmap f . observe)

instance Applicative Gen where
    pure a = Gen (return unit) return (const $ return a)

    Gen initF updateF observeF <*> Gen initA updateA observeA = Gen init update observe
        where
            init = pair initF initA
            update (sf, sa) = pair (updateF sf) (updateA sa)
            observe (sf, sa) = (observeF sf) <*> (observeA sa)

            pair :: Applicative f => f a -> f b -> f (a, b)
            pair = liftA2 (,) 

test :: Gen D -> IO ()
test g = dac $ mul 0.5 $ schedHarp 0.2 instr $ fmap return $ runGen g $ metro 2
    where instr cps = (return . mul (linseg [1, 0.2, 0]) . osc . sig) cps

test2 :: Gen D -> IO ()
test2 g = dac $ at ( + nticks [3, 3, 2] 120)  $ mul 0.5 $ schedHarp 0.2 instr $ fmap return $ runGen (gen2 (choose [1, 0.5, 0.5, 0.2]) g) $ randSkip 0.8 $ (every 0 [3, 3, 2, 4, 3, 1] $ metro 8)
    where instr (amp, cps) = (return . mul (sig amp * linseg [1, 0.2, 0]) . osc . sig) cps

runGen :: Gen a -> Tick -> Evt a
runGen (Gen init update observe) tick = Evt $ \bam -> do
    ref <- newRef =<< init
    runEvt tick $ \_ -> do
        s <- readRef ref
        bam =<< observe s
        writeRef ref =<< update s

gen2 :: Gen a -> Gen b -> Gen (a, b)
gen2 a b = liftA2 (,) a b

gen3 :: Gen a -> Gen b -> Gen c -> Gen (a, b, c)
gen3 a b c = liftA3 (,,) a b c

gen4 :: Gen a -> Gen b -> Gen c -> Gen d -> Gen (a, b, c, d)
gen4 a b c d = (,,,) <$> a <*> b <*> c <*> d

cycleG :: Arg a => [a] -> Gen a
cycleG xs = fmap (atArg xs) $ rangeG (0, int $ length xs)

rangeG :: (D, D) -> Gen D
rangeG (a, b) = Gen init update observe
    where
        init = return a

        update s = return $ ifB (s1 >=* a + b) a s1
            where s1 = s + 1            

        observe = return

choose :: Arg a => [a] -> Gen a
choose vals = fmap (atArg vals) $ randIntsG (0, int $ length vals)

randIntsG :: (D, D) -> Gen D
randIntsG (a, b) = Gen init update observe
    where
        init = random a b
        update _ = fmap (int' . readSnap) $ random (sig a) (sig b)
        observe s = printk 0 (sig s) >> return s

freqChoose :: Arg a => [(D, a)] -> Gen a
freqChoose = undefined

brownianGauss :: Arg a => Sig -> [a] -> Gen a
brownianGauss dx vals = fmap (atArg vals) $ brownianGaussInts dx (0, int $ length vals)

brownianGaussInts :: Sig -> (D, D) -> Gen D
brownianGaussInts maxDx (a, b) = Gen init update observe
    where
        init = random a b

        update s = do
            dx <- gauss maxDx
            let s1 = int' $ sig s + dx
            s2 <- random (sig a) (sig b)
            return $ (int' . readSnap) $ ifB (s1 `greaterThanEquals` sig b ||* s1 `lessThan` sig a) s2 s1

        observe = return

brownianSteps :: Arg a => [Double] -> [a] -> Gen a
brownianSteps steps vals = fmap (atArg vals) $ brownianGaussIntsBy nextRand (0, int $ length vals)
    where nextRand = duserrnd (randDist steps)

brownianGaussIntsBy :: SE Sig -> (D, D) -> Gen D
brownianGaussIntsBy nextRand (a, b) = Gen init update observe
    where
        init = random a b

        update s = do
            dx <- nextRand
            let s1 = int' $ sig s + dx
            s2 <- random (sig a) (sig b)
            return $ (int' . readSnap) $ ifB (s1 `greaterThanEquals` sig b ||* s1 `lessThan` sig a) s2 s1

        observe = return


chain :: [(Sig, Gen a)] -> Gen a
chain = undefined

rndChain :: [(Sig2, Gen a)] -> Gen a
rndChain = undefined

-- idea to go from lists to tabs (portions of tabs)

data TabSeg = TabSeg { tabSegStart :: Sig, tabSegEnd :: Sig, tabSegData :: Tab }

cycleTab :: TabSeg -> Gen D
cycleTab = undefined

chooseTab :: TabSeg -> Gen D
chooseTab = undefined


