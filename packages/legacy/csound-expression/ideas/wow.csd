; I can patch instruments to instruments
<CsoundSynthesizer>
<CsOptions>

</CsOptions>

<CsInstruments>

sr = 44100
ksmps = 64
nchnls = 1
0dbfs = 1

giNote = 0

        instr 2
Sname   sprintf "sink%d", p6
aout    oscils p4, p5, 0
        chnmix aout, Sname
        endin


        instr   100
giNote  =       giNote + 1
iStep   =       p3 / 4

        print giNote
        event_i "i", 2, 0,          iStep, 0.3, p4, giNote
        event_i "i", 2, iStep,      iStep, 0.2, (9/8)*p4, giNote
        event_i "i", 2, 2*iStep,    iStep, 0.2, 1.5*p4, giNote
        event_i "i", 2, 3*iStep,    iStep, 0.1, 2*p4, giNote

Sname   sprintf "sink%d", giNote
aout    chnget  Sname

Sout    sprintf "sink%d", p5
        chnmix aout, Sout
        chnclear Sname
        endin

        instr   200
giNote  =       giNote + 1

        print giNote
        event_i "i", 100, 0, p3, p4,     giNote
        event_i "i", 100, 0, p3, 1.5*p4, giNote

Sname   sprintf "sink%d", giNote
aout    chnget  Sname        
        out     aout
        chnclear Sname
        endin

</CsInstruments>

<CsScore>

i200 0 2.5 150
i200 0 2.5 153
i200 1 2 440
i200 1 0.3 500
i200 1.5 0.3 300
i200 1.8 0.3 400
i200 2.2 0.2 600
e

</CsScore>

</CsoundSynthesizer>

