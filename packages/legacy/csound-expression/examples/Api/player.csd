<CsoundSynthesizer>

<CsOptions>

--nodisplays

</CsOptions>

<CsInstruments>

sr = 44100
ksmps = 64
nchnls = 2
0dbfs = 1.0
gargg3 init 0.0
gargg2 init 0.0
gargg1 init 0.0
gargg0 init 0.0
giPort init 1
opcode FreePort, i, 0
xout giPort
giPort = giPort + 1
endop




instr stop
ir1 = 0.0
ir2 = 0.1
 turnoff2 "wav", ir1, ir2
 turnoff2 "mp3", ir1, ir2
 turnoff 
endin

instr mp3
ar0 = gargg2
ar1 = gargg3
kr0 linseg 0.0, 1.0e-2, 1.0, 1.0, 1.0
ar2 upsamp kr0
kr0 linsegr 1.0, 1.0, 1.0, 0.1, 0.0
ar3 upsamp kr0
ar4 = (ar2 * ar3)
ar2, ar3 mp3in p4
ar5 = (ar4 * ar2)
ar2 = (ar0 + ar5)
gargg2 = ar2
ar0 = (ar4 * ar3)
ar2 = (ar1 + ar0)
gargg3 = ar2
endin

instr wav
ar0 = gargg0
ar1 = gargg1
kr0 linseg 0.0, 1.0e-2, 1.0, 1.0, 1.0
ar2 upsamp kr0
kr0 linsegr 1.0, 1.0, 1.0, 0.1, 0.0
ar3 upsamp kr0
ar4 = (ar2 * ar3)
ir8 = 1.0
ar2, ar3 diskin2 p4, ir8
ar5 = (ar4 * ar2)
ar2 = (ar0 + ar5)
gargg0 = ar2
ar0 = (ar4 * ar3)
ar2 = (ar1 + ar0)
gargg1 = ar2
endin

instr 21

endin

instr 20
gargg3 = 0.0
gargg2 = 0.0
gargg1 = 0.0
gargg0 = 0.0
 event_i "i", 19, 604800.0, 1.0e-2
endin

instr 19
 turnoff2 18, 0.0, 0.0
 exitnow 
endin

instr 18
ar0 = gargg0
ar1 = gargg1
ar2 = gargg2
ar3 = gargg3
arl0 init 0.0
arl1 init 0.0
ar4 = (ar0 + ar2)
ar0 clip ar4, 0.0, 0dbfs
ar2 = (ar0 * 0.8)
arl0 = ar2
ar0 = (ar1 + ar3)
ar1 clip ar0, 0.0, 0dbfs
ar0 = (ar1 * 0.8)
arl1 = ar0
ar0 = arl0
ar1 = arl1
 outs ar0, ar1
endin

</CsInstruments>

<CsScore>



f0 604800.0

i 21 0.0 -1.0 
i 20 0.0 -1.0 
i 18 0.0 -1.0 

</CsScore>

</CsoundSynthesizer>