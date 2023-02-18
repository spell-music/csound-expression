<CsoundSynthesizer>

<CsOptions>

--nodisplays

</CsOptions>

<CsInstruments>

sr = 44100
ksmps = 64
nchnls = 1
0dbfs = 1.0
gargg0 init 0.0
giPort init 1
opcode FreePort, i, 0
xout giPort
giPort = giPort + 1
endop




instr osc
ar0 = gargg0
ir3 = 1.0
ir4 = p5
ar1 oscil3 ir3, ir4, 2
ar2 = (p4 * ar1)
ar1 = (ar0 + ar2)
gargg0 = ar1
endin

instr 21

endin

instr 20
gargg0 = 0.0
 event_i "i", 19, 604800.0, 1.0e-2
endin

instr 19
 turnoff2 18, 0.0, 0.0
 exitnow 
endin

instr 18
ar0 = gargg0
arl0 init 0.0
ar1 clip ar0, 0.0, 0dbfs
ar0 = (ar1 * 0.8)
arl0 = ar0
ar0 = arl0
 out ar0
endin

</CsInstruments>

<CsScore>

f2 0 8192 10  1.0

f0 604800.0

i 21 0.0 -1.0 
i 20 0.0 -1.0 
i 18 0.0 -1.0 

</CsScore>

</CsoundSynthesizer>