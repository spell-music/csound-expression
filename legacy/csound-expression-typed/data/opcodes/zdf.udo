
; Zero Delay Feedback Filters
; 
; Based on code by Will Pirkle, presented in:
;
; http://www.willpirkle.com/Downloads/AN-4VirtualAnalogFilters.2.0.pdf
; 
; and in his book "Designing software synthesizer plug-ins in C++ : for 
; RackAFX, VST3, and Audio Units"
;
; ZDF using Trapezoidal integrator by Vadim Zavalishin, presented in "The Art 
; of VA Filter Design" (https://www.native-instruments.com/fileadmin/ni_media/
; downloads/pdf/VAFilterDesign_1.1.1.pdf)
;
; UDO versions by Steven Yi (2016.xx.xx)


;; 1-pole (6dB) lowpass/highpass filter
;; takes in a a-rate signal and cutoff value in frequency
opcode zdf_1pole, aa, ak
  ain, kcf  xin

  ; pre-warp the cutoff- these are bilinear-transform filters
  kwd = 2 * $M_PI * kcf
  iT  = 1/sr 
  kwa = (2/iT) * tan(kwd * iT/2) 
  kg  = kwa * iT/2 

  ; big combined value
  kG  = kg / (1.0 + kg)

  ahp init 0
  alp init 0

  ;; state for integrators
  kz1 init 0

  kindx = 0
  while kindx < ksmps do
    ; do the filter, see VA book p. 46 
    ; form sub-node value v(n) 
    kin = ain[kindx]
    kv = (kin - kz1) * kG 

    ; form output of node + register 
    klp = kv + kz1 
    khp = kin - klp 

    ; z1 register update
    kz1 = klp + kv  

    alp[kindx] = klp
    ahp[kindx] = khp
    kindx += 1
  od

  xout alp, ahp
endop


;; 1-pole (6dB) lowpass/highpass filter
;; takes in a a-rate signal and cutoff value in frequency
opcode zdf_1pole, aa, aa
  ain, acf  xin

  ; pre-warp the cutoff- these are bilinear-transform filters
  iT  = 1/sr 

  ahp init 0
  alp init 0

  ;; state for integrators
  kz1 init 0

  kindx = 0
  while kindx < ksmps do
    ; pre-warp the cutoff- these are bilinear-transform filters
    kwd = 2 * $M_PI * acf[kindx]
    kwa = (2/iT) * tan(kwd * iT/2) 
    kg  = kwa * iT/2 

    ; big combined value
    kG  = kg / (1.0 + kg)

    ; do the filter, see VA book p. 46 
    ; form sub-node value v(n) 
    kin = ain[kindx]
    kv = (kin - kz1) * kG 

    ; form output of node + register 
    klp = kv + kz1 
    khp = kin - klp 

    ; z1 register update
    kz1 = klp + kv  

    alp[kindx] = klp
    ahp[kindx] = khp
    kindx += 1
  od

  xout alp, ahp
endop

;; 1-pole allpass filter
;; takes in an a-rate signal and corner frequency where input
;; phase is shifted -90 degrees
opcode zdf_allpass_1pole, a, ak
  ain, kcf xin
  alp, ahp zdf_1pole ain, kcf
  aout = alp - ahp
  xout aout
endop


;; 1-pole allpass filter
;; takes in an a-rate signal and corner frequency where input
;; phase is shifted -90 degrees
opcode zdf_allpass_1pole, a, aa
  ain, acf xin
  alp, ahp zdf_1pole ain, acf
  aout = alp - ahp
  xout aout
endop


;; 2-pole (12dB) lowpass/highpass/bandpass filter
;; takes in a a-rate signal, cutoff value in frequency, and
;; Q factor for resonance
opcode zdf_2pole,aaa,aKK

  ain, kcf, kQ     xin

  ; pre-warp the cutoff- these are bilinear-transform filters
  kwd = 2 * $M_PI * kcf
  iT  = 1/sr 
  kwa = (2/iT) * tan(kwd * iT/2) 
  kG  = kwa * iT/2 
  kR  = 1 / (2 * kQ)

  ;; output signals
  alp init 0
  ahp init 0
  abp init 0

  ;; state for integrators
  kz1 init 0
  kz2 init 0

  ;;
  kindx = 0
  while kindx < ksmps do
    khp = (ain[kindx] - (2 * kR + kG) * kz1 - kz2) / (1 + (2 * kR * kG) + (kG * kG))
    kbp = kG * khp + kz1
    klp = kG * kbp + kz2

    ; z1 register update
    kz1 = kG * khp + kbp  
    kz2 = kG * kbp + klp  

    alp[kindx] = klp
    ahp[kindx] = khp
    abp[kindx] = kbp
    kindx += 1
  od

  xout alp, abp, ahp

endop


;; 2-pole (12dB) lowpass/highpass/bandpass filter
;; takes in a a-rate signal, cutoff value in frequency, and
;; Q factor for resonance
opcode zdf_2pole,aaa,aaa

  ain, acf, aQ     xin

  iT  = 1/sr 

  ;; output signals
  alp init 0
  ahp init 0
  abp init 0

  ;; state for integrators
  kz1 init 0
  kz2 init 0

  ;;
  kindx = 0
  while kindx < ksmps do

    ; pre-warp the cutoff- these are bilinear-transform filters
    kwd = 2 * $M_PI * acf[kindx]
    kwa = (2/iT) * tan(kwd * iT/2) 
    kG  = kwa * iT/2 

    kR = 1 / (2 * aQ[kindx]) 

    khp = (ain[kindx] - (2 * kR + kG) * kz1 - kz2) / (1 + (2 * kR * kG) + (kG * kG))
    kbp = kG * khp + kz1
    klp = kG * kbp + kz2

    ; z1 register update
    kz1 = kG * khp + kbp  
    kz2 = kG * kbp + klp 

    alp[kindx] = klp
    ahp[kindx] = khp
    abp[kindx] = kbp
    kindx += 1
  od

  xout alp, abp, ahp

endop

;; 2-pole (12dB) lowpass/highpass/bandpass/notch filter
;; takes in a a-rate signal, cutoff value in frequency, and
;; Q factor for resonance
opcode zdf_2pole_notch,aaaa,aKK

  ain, kcf, kQ     xin

  ; pre-warp the cutoff- these are bilinear-transform filters
  kwd = 2 * $M_PI * kcf
  iT  = 1/sr 
  kwa = (2/iT) * tan(kwd * iT/2) 
  kG  = kwa * iT/2 
  kR  = 1 / (2 * kQ)

  ;; output signals
  alp init 0
  ahp init 0
  abp init 0
  anotch init 0

  ;; state for integrators
  kz1 init 0
  kz2 init 0

  ;;
  kindx = 0
  while kindx < ksmps do
    kin = ain[kindx]
    khp = (kin - (2 * kR + kG) * kz1 - kz2) / (1 + (2 * kR * kG) + (kG * kG))
    kbp = kG * khp + kz1
    klp = kG * kbp + kz2
    knotch = kin - (2 * kR * kbp)

    ; z1 register update
    kz1 = kG * khp + kbp  
    kz2 = kG * kbp + klp  

    alp[kindx] = klp
    ahp[kindx] = khp
    abp[kindx] = kbp
    anotch[kindx] = knotch
    kindx += 1
  od

  xout alp, abp, ahp, anotch

endop

;; 2-pole (12dB) lowpass/highpass/bandpass/notch filter
;; takes in a a-rate signal, cutoff value in frequency, and
;; Q factor for resonance
opcode zdf_2pole_notch,aaaa,aaa

  ain, acf, aQ     xin

  iT  = 1/sr 

  ;; output signals
  alp init 0
  ahp init 0
  abp init 0
  anotch init 0

  ;; state for integrators
  kz1 init 0
  kz2 init 0

  ;;
  kindx = 0
  while kindx < ksmps do

    ; pre-warp the cutoff- these are bilinear-transform filters
    kwd = 2 * $M_PI * acf[kindx]
    kwa = (2/iT) * tan(kwd * iT/2) 
    kG  = kwa * iT/2 

    kR = 1 / (2 * aQ[kindx])

    kin = ain[kindx]
    khp = (kin - (2 * kR + kG) * kz1 - kz2) / (1 + (2 * kR * kG) + (kG * kG))
    kbp = kG * khp + kz1
    klp = kG * kbp + kz2
    knotch = kin - (2 * kR * kbp)

    ; z1 register update
    kz1 = kG * khp + kbp  
    kz2 = kG * kbp + klp 

    alp[kindx] = klp
    ahp[kindx] = khp
    abp[kindx] = kbp
    anotch[kindx] = knotch
    kindx += 1
  od

  xout alp, abp, ahp, anotch

endop

;; moog ladder

opcode zdf_ladder, a, akk

  ain, kcf, kres     xin
  aout init 0

  kR = limit(1 - kres, 0.025, 1)

  kQ = 1 / (2 * kR) 

  kwd = 2 * $M_PI * kcf
  iT  = 1/sr 
  kwa = (2/iT) * tan(kwd * iT/2) 
  kg  = kwa * iT/2 

  kk = 4.0*(kQ - 0.707)/(25.0 - 0.707)

  kg_2 = kg * kg
  kg_3 = kg_2 * kg

  ; big combined value
  ; for overall filter
  kG  = kg_2 * kg_2  
  ; for individual 1-poles
  kG_pole = kg/(1.0 + kg)

  ;; state for each 1-pole's integrator 
  kz1 init 0
  kz2 init 0
  kz3 init 0
  kz4 init 0

  kindx = 0
  while kindx < ksmps do
    ;; processing
    kin = ain[kindx]

    kS = kg_3 * kz1 + kg_2 * kz2 + kg * kz3 + kz4
    ku = (kin - kk *  kS) / (1 + kk * kG)

    ;; 1st stage
    kv = (ku - kz1) * kG_pole 
    klp = kv + kz1
    kz1 = klp + kv

    ;; 2nd stage
    kv = (klp - kz2) * kG_pole 
    klp = kv + kz2
    kz2 = klp + kv

    ;; 3rd stage
    kv = (klp - kz3) * kG_pole 
    klp = kv + kz3
    kz3 = klp + kv

    ;; 4th stage
    kv = (klp - kz4) * kG_pole 
    klp = kv + kz4
    kz4 = klp + kv

    aout[kindx] = klp

    kindx += 1
  od

  xout aout
endop


opcode zdf_ladder, a, aaa

  ain, acf, ares     xin
  aout init 0

  iT  = 1/sr 

  ;; state for each 1-pole's integrator 
  kz1 init 0
  kz2 init 0
  kz3 init 0
  kz4 init 0

  kindx = 0
  while kindx < ksmps do

    kR = limit(1 - ares[kindx], 0.025, 1)

    kQ = 1 / (2 * kR) 

    kwd = 2 * $M_PI * acf[kindx]
    kwa = (2/iT) * tan(kwd * iT/2) 
    kg  = kwa * iT/2 

    kk = 4.0*(kQ - 0.707)/(25.0 - 0.707)

    kg_2 = kg * kg
    kg_3 = kg_2 * kg

    ; big combined value
    ; for overall filter
    kG  = kg_2 * kg_2  
    ; for individual 1-poles
    kG_pole = kg/(1.0 + kg)

    ;; processing
    kin = ain[kindx]

    kS = kg_3 * kz1 + kg_2 * kz2 + kg * kz3 + kz4
    ku = (kin - kk *  kS) / (1 + kk * kG)

    ;; 1st stage
    kv = (ku - kz1) * kG_pole 
    klp = kv + kz1
    kz1 = klp + kv

    ;; 2nd stage
    kv = (klp - kz2) * kG_pole 
    klp = kv + kz2
    kz2 = klp + kv

    ;; 3rd stage
    kv = (klp - kz3) * kG_pole 
    klp = kv + kz3
    kz3 = klp + kv

    ;; 4th stage
    kv = (klp - kz4) * kG_pole 
    klp = kv + kz4
    kz4 = klp + kv

    aout[kindx] = klp

    kindx += 1
  od

  xout aout
endop

;; 4-pole

opcode zdf_4pole, aaaaaa, akk
  ain, kcf, kres xin

  alp2, abp2, ahp2 zdf_2pole ain, kcf, kres

  abp4 init 0
  abl4 init 0
  alp4 init 0

  xout alp2, abp2, ahp2, alp4, abl4, abp4
endop

opcode zdf_4pole, aaaaaa, aaa
  ain, acf, ares xin

  alp2, abp2, ahp2 zdf_2pole ain, acf, ares
  abp4 init 0
  abl4 init 0
  alp4 init 0

  xout alp2, abp2, ahp2, alp4, abl4, abp4
endop


opcode zdf_4pole_hp, aaaaaa, akk
  ain, kcf, kres xin

  alp2, abp2, ahp2 zdf_2pole ain, kcf, kres

  ahp4 init 0
  abh4 init 0
  abp4 init 0

  xout alp2, abp2, ahp2, abp4, abh4, ahp4
endop

opcode zdf_4pole_hp, aaaaaa, aaa
  ain, acf, ares xin

  alp2, abp2, ahp2 zdf_2pole ain, acf, ares

  ahp4 init 0
  abh4 init 0
  abp4 init 0

  xout alp2, abp2, ahp2, abp4, abh4, ahp4
endop

;; TODO - implement
opcode zdf_peak_eq, a, akkk
  ain, kcf, kres, kdB xin

  aout init 0

  xout aout
endop

opcode zdf_high_shelf_eq, a, akk
  ain, kcf, kdB xin

  ;; TODO - convert db to K, check if reusing zdf_1pole is sufficient
  kK init 0

  alp, ahp zdf_1pole ain, kcf

  aout = ain + kK * ahp

  xout aout
endop

opcode zdf_low_shelf_eq, a, akk
  ain, kcf, kdB xin

  ;; TODO - convert db to K, check if reusing zdf_1pole is sufficient
  kK init 0

  alp, ahp zdf_1pole ain, kcf

  aout = ain + kK * alp

  xout aout
endop
