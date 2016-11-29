

listenOsc :: OscVal a => OscPort -> OscAddr -> Evt a


opcode OSC_port_addr, 0, kkS
    ...
endop

kval1 init 0
kval2 init 0
nextmsg:
    kk OSClisten .....
if (kk == 0) goto ex
    OSC_port_addr kval1, kval2    
ex:
endif

----------------

nport = 7700

names <- newStrArr size 

runEvt (listenOsc nport) $ \(OscInt channel, fileName) -> do
    writeArr names channel fileName
    updateChannelPlyback channel

