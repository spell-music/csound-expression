/* audaciouseq - multi-band equalizer 
   
   A Csound UDO implementation of Audacious' EQ:

   https://github.com/audacious-media-player/audacious/blob/master/src/libaudcore/equalizer.cc

*/


/* Q value for band-pass filters 1.2247 = (3/2)^(1/2)
 * Gives 4 dB suppression at Fc*2 and Fc/2 */

#define Q # 1.2247449 #

/* single-sample, 2nd order IIR filter */
opcode audacious_bp2, k, kik
kin, ifC, kgain xin

;; Calc Coefficients
ith = 2 * $M_PI * (ifC / sr)
iC  = (1 - tan(ith * $Q / 2)) / (1 + tan(ith * $Q / 2))

ia0 = (1 + iC) * cos(ith)
ia1 = -iC 
ib0 = (1 - iC) / 2 
ib1 = -1.005

kout init 0
kwq0 init 0
kwq1 init 0

kG = pow(10, kgain / 20)  - 1

;; Filter Code
kyt = kin 
kw = kyt * ib0 + kwq0 * ia0 + kwq1 * ia1
kyt += (kw + kwq1 * ib1) * kG 

;; Memory
kwq1 = kwq0
kwq0 = kw

xout kyt 
endop

/* 10-band EQ 
   Input: asig, kgain1, kgain2, ...
   Output: aout

   10 kgain arguments maps to each band
   Bands are: 31.25, 52.6, 125, 500, 1000, 
              2000, 4000, 8000, 16000 
*/
opcode audaciouseq, a, akkkkkkkkkk

ain, kgain1, kgain2, kgain3, kgain4, kgain5, 
     kgain6, kgain7, kgain8, kgain9, kgain10 xin

aout = 0
kndx = 0

while (kndx < ksmps) do
  ksamp audacious_bp2 ain[kndx], 31.25, kgain1
  ksamp audacious_bp2 ksamp, 62.5, kgain2
  ksamp audacious_bp2 ksamp, 125, kgain3
  ksamp audacious_bp2 ksamp, 250, kgain4
  ksamp audacious_bp2 ksamp, 500, kgain5
  ksamp audacious_bp2 ksamp, 1000, kgain6
  ksamp audacious_bp2 ksamp, 2000, kgain7
  ksamp audacious_bp2 ksamp, 4000, kgain8
  ksamp audacious_bp2 ksamp, 8000, kgain9
  aout[kndx] audacious_bp2 ksamp, 16000, kgain10

  kndx += 1
od

xout aout

endop


