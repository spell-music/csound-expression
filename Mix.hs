module Mix where

type Note = [Prim]
type Instr = E

data Mix a 
    = Sco Instr (Track Note)    
    | Mix ([Sig] -> SE [Sig]) (Track (Mix a))

sco :: (Arg a, Out b) => (a -> b) -> Track a       -> Track (Mix (NoSE b))
mix :: (Out a, Out b) => (a -> b) -> Track (Mix a) -> Track (Mix (NoSE b))

mixOut :: Out a => Track (Mix a) -> a

renderCsd :: (Out a) => Track (Mix a) -> String
writeCsd  :: (Out a) => String -> Track (Mix a) -> IO ()




