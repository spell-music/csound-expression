import Csound.Base
import Csound.Typed.Control (OscRef)

oscPort = initOsc 7712

getSig :: ListenOsc Sig
getSig = listenOscVal oscPort

getSig2 :: ListenOsc Sig2
getSig2 = listenOscVal oscPort

main = dac $ do
  cps <- getSig "/cps" 100
  amp <- getSig "/amp" 0.5
  (cfq, res) <- getSig2 "/filt" (2500, 0.1)
  runEvt (listenOsc oscPort "/text" "sf") $ \(s, n) -> printks s 0 [] >> printks (text ": %f") 0 [n]
  return $ amp * (mlp cfq res $ sqr cps)
