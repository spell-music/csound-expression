<CsoundSynthesizer>

<CsOptions>

--output=dac --midi-device=a --nodisplays

</CsOptions>

<CsInstruments>

sr = 44100
ksmps = 64
nchnls = 2
0dbfs = 1.0
giPort init 1

opcode FreePort, i, 0
xout giPort
giPort = giPort + 1
endop

opcode ActiveNorm_2, aa, aa
a1, a2 xin
kActive active p1
prints "active: %d\\n", kActive
kActiveSqrt sqrt kActive
a1 = a1 / kActiveSqrt
a2 = a2 / kActiveSqrt
xout a1, a2
endop

opcode Send_2, 0, iaa
iport, a1, a2 xin
a1, a2 ActiveNorm_2 a1, a2
S1 sprintf "p1_%d", iport
 chnmix a1, S1
S2 sprintf "p2_%d", iport
 chnmix a2, S2
endop

opcode Receive_2, aa, i
iport xin
S1 sprintf "p1_%d", iport
a1 chnget S1
S2 sprintf "p2_%d", iport
a2 chnget S2
 chnclear S1
 chnclear S2
 xout a1, a2
endop


opcode Sco_0, aa, iiii

ilen, itabInstr, itabStart, itabDur xin

iport FreePort
istep init 0

prints "len %d\\n", ilen
until istep >= ilen do    
    prints "play instance %d\\n", istep
    iInstr tab_i istep, itabInstr
    iStart tab_i istep, itabStart
    iDur   tab_i istep, itabDur
    event_i "i", iInstr, iStart, iDur, iport    
    istep = istep + 1
od

a1, a2 Receive_2 iport
xout a1, a2

endop

opcode Sco_1, aa, ii

ilen, iTab xin

iport FreePort
istep init 0

prints "len %d\\n", ilen
until istep >= ilen do    
    
    iInstr_ix = istep * 4
    iStart_ix = istep * 4 + 1
    iDur_ix = istep * 4 + 2
    iP5_ix = istep * 4 + 3
    iInstr tab_i iInstr_ix, iTab
    iStart tab_i iStart_ix, iTab
    iDur   tab_i iDur_ix, iTab
    iP5    tab_i iP5_ix, iTab

    prints "play instance %d\\n", iInstr_ix
    prints "i %d\\n", iInstr
    event_i "i", iInstr, iStart, iDur, iport, iP5
    istep = istep + 1
od

a1, a2 Receive_2 iport
xout a1, a2

endop

opcode Evt_1, aa, iiik

ilen, iscale, iTab, kTrig xin

iport FreePort
kstep init 0

if kTrig == 1 then
    kstep = 0
    until kstep >= ilen do        
        kInstr_ix = kstep * 4
        kStart_ix = kstep * 4 + 1
        kDur_ix = kstep * 4 + 2
        kP5_ix = kstep * 4 + 3
        kInstr tab kInstr_ix, iTab
        kStart tab kStart_ix, iTab
        kDur   tab kDur_ix, iTab
        kP5    tab kP5_ix, iTab

        prints "play instance %d\\n", kInstr_ix
        prints "i %d\\n", kInstr
        event "i", kInstr, (kStart * iscale), (kDur * iscale), iport, kP5
        kstep = kstep + 1
    od        
endif

prints "len %d\\n", ilen


a1, a2 Receive_2 iport
xout a1, a2

endop





instr 2
kenv linseg 0, 0.01, 1, p3 - 0.02, 1, 0.01, 0
aout oscil 0.5, p5, 1
Send_2 p4, aout * kenv, aout * kenv
endin

instr 1
kenv linseg 0, 0.01, 1, p3 - 0.02, 1, 0.01, 0
aout oscil 0.5, p5, 1
Send_2 p4, aout * kenv, aout * kenv
endin

instr 3

ktrig metro 1
a1, a2 Evt_1 7, 0.5, 2, ktrig
outs a1, a2
endin 

</CsInstruments>

<CsScore>

f1 0 8192 10  1.0

;f0 604800.0

f2 0 64 -2 1 0 0.5 220 1 1 0.1 110 2 2 0.5 330 2 3 0.5 440 1 2.5 0.1 55 2 3.5 0.2 880 1 0.5 0.1 55

f3 0 8 -2 0 1 2 3 
f4 0 8 -2 0.5 0.1 0.5 0.5
f5 0 8 -2 220 110 330 440

i 3 0.0 10.0 

;i 39 0.0 -1.0 
;i 38 0.0 -1.0 
;i 36 0.0 -1.0 

</CsScore>

</CsoundSynthesizer>