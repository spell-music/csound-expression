/* Solina Chorus, based on Solina String Ensemble Chorus Module
  
   based on:

   J. Haible: Triple Chorus
   http://jhaible.com/legacy/triple_chorus/triple_chorus.html

   Hugo Portillo: Solina-V String Ensemble
   http://www.native-instruments.com/en/reaktor-community/reaktor-user-library/entry/show/4525/ 

   Parabola tabled shape borrowed from Iain McCurdy delayStereoChorus.csd:
   http://iainmccurdy.org/CsoundRealtimeExamples/Delays/delayStereoChorus.csd

   Author: Steven Yi
   Date: 2016.05.22

   */


gi_solina_parabola ftgen 0, 0, 65537, 19, 0.5, 1, 180, 1 

;; 3 sine wave LFOs, 120 degrees out of phase
opcode sol_lfo_3, aaa, KK
  kfreq, kamp xin

  aphs phasor kfreq

  a0   = tablei(aphs, gi_solina_parabola, 1, 0, 1)
  a120 = tablei(aphs, gi_solina_parabola, 1, 0.333, 1)
  a240 = tablei(aphs, gi_solina_parabola, 1, -0.333, 1)

  xout (a0 * kamp), (a120 * kamp), (a240 * kamp)
endop

opcode solina_chorus, a, aKKKK

  aLeft, klfo_freq1, klfo_amp1, klfo_freq2, klfo_amp2 xin

  imax = 100

  ;; slow lfo
  as1, as2, as3 sol_lfo_3 klfo_freq1, klfo_amp1

  ;; fast lfo
  af1, af2, af3  sol_lfo_3 klfo_freq2, klfo_amp2

  at1 = limit(as1 + af1 + 5, 0.0, imax)
  at2 = limit(as2 + af2 + 5, 0.0, imax)
  at3 = limit(as3 + af3 + 5, 0.0, imax)
    
  a1 vdelay3 aLeft, at1, imax 
  a2 vdelay3 aLeft, at2, imax 
  a3 vdelay3 aLeft, at3, imax 

xout (a1 + a2 + a3) / 3
endop
