;; 12db/oct low-pass filter based on Korg 35 module
;; (found in MS-10 and MS-20).
;; 
;; Based on code by Will Pirkle, presented in:
;; 
;; http://www.willpirkle.com/Downloads/AN-5Korg35_V3.pdf
;; 
;; [ARGS]
;; 
;; ain - audio input
;; acutoff - frequency of cutoff
;; kQ - filter Q [1, 10.0] (k35-lpf will clamp to boundaries)
;; knonlinear - use non-linear processing
;; ksaturation - saturation for tanh distortion
opcode k35_lpf, a, aKKKK

  ain, kcutoff, kQ, knonlinear, ksaturation xin

  kz1 init 0
  kz2 init 0
  kz3 init 0
  kv1 init 0
  kv2 init 0
  kv3 init 0
  aout init 0

  kg init 0
  kG init 0
  kK init 0
  klastcut init -1
  klastQ init -1
  kS35 init 0 
  kalpha init -1 
  klpf2_beta init -1 
  khpf1_beta init -1 

  kindx = 0
  kQ = limit(kQ, 1.0, 10.0)
  kcf = kcutoff 

  if (klastcut != kcf) then
    ; pre-warp the cutoff- these are bilinear-transform filters
    kwd = 2 * $M_PI * kcf
    iT  = 1/sr 
    kwa = (2/iT) * tan(kwd * iT/2) 
    kg  = kwa * iT/2 
    kG  = kg / (1 + kg)

  endif

  if (klastQ != kQ) then
    kK  = 0.01 + ((2.0 -  0.01) * (kQ / 10.0))
  endif

  if ((klastcut != kcf) || (klastQ != kQ)) then
    klpf2_beta = (kK - (kK * kG)) / (1.0 + kg)
    khpf1_beta = -1.0 / (1.0 + kg)
    kalpha = 1.0 / (1.0 - (kK * kG) + (kK * kG * kG))
  endif

  klastcut = kcf
  klastQ = kQ
  
  while (kindx < ksmps) do
    ksig = ain[kindx]

    ;; lpf1
    kv1 = (ksig - kz1) * kG
    klp1 = kv1 + kz1 
    kz1 = klp1 + kv1
   
    ku = kalpha * (klp1 + kS35)

    if (knonlinear == 1) then
      ku = tanh(ku * ksaturation)
    endif

    ;; lpf2
    kv2 = (ku - kz2) * kG
    klp2 = kv2 + kz2 
    kz2 = klp2 + kv2
    ky = kK * klp2

    ;; hpf1
    kv3 = (ky - kz3) * kG
    klp3 = kv3 + kz3 
    kz3 = klp3 + kv3
    khp1 = ky - klp3

    kS35 = (klpf2_beta * kz2) + (khpf1_beta * kz3)

    kout = (kK > 0) ? (ky / kK) : ky 

    aout[kindx] = kout

    kindx += 1
  od

  xout aout

endop



opcode k35_lpf, a, aaKKK

  ain, acutoff, kQ, knonlinear, ksaturation xin

  kz1 init 0
  kz2 init 0
  kz3 init 0
  kv1 init 0
  kv2 init 0
  kv3 init 0
  aout init 0

  kg init 0
  kG init 0
  kK init 0
  klastcut init -1
  klastQ init -1
  kS35 init 0 
  kalpha init -1 
  klpf2_beta init -1 
  khpf1_beta init -1 

  kindx = 0
  kQ = limit(kQ, 1.0, 10.0)

  if (klastQ != kQ) then
    kK  = 0.01 + ((2.0 -  0.01) * (kQ / 10.0))
  endif

  klastQ = kQ
  
  while (kindx < ksmps) do
    kcf = acutoff[kindx]
    ksig = ain[kindx]

    if (klastcut != kcf) then
      ; pre-warp the cutoff- these are bilinear-transform filters
      kwd = 2 * $M_PI * kcf
      iT  = 1/sr 
      kwa = (2/iT) * tan(kwd * iT/2) 
      kg  = kwa * iT/2 
      kG  = kg / (1 + kg)

    endif

    if ((klastcut != kcf) || (klastQ != kQ)) then
      klpf2_beta = (kK - (kK * kG)) / (1.0 + kg)
      khpf1_beta = -1.0 / (1.0 + kg)
      kalpha = 1.0 / (1.0 - (kK * kG) + (kK * kG * kG))
    endif

    ;; lpf1
    kv1 = (ksig - kz1) * kG
    klp1 = kv1 + kz1 
    kz1 = klp1 + kv1
   
    ku = kalpha * (klp1 + kS35)

    if (knonlinear == 1) then
      ku = tanh(ku * ksaturation)
    endif

    ;; lpf2
    kv2 = (ku - kz2) * kG
    klp2 = kv2 + kz2 
    kz2 = klp2 + kv2
    ky = kK * klp2

    ;; hpf1
    kv3 = (ky - kz3) * kG
    klp3 = kv3 + kz3 
    kz3 = klp3 + kv3
    khp1 = ky - klp3

    kS35 = (klpf2_beta * kz2) + (khpf1_beta * kz3)

    kout = (kK > 0) ? (ky / kK) : ky 

    aout[kindx] = kout

    klastcut = kcf
    kindx += 1
  od

  xout aout

endop

;; 6db/oct high-pass filter based on Korg 35 module
;; (found in MS-10 and MS-20).
;; 
;; Based on code by Will Pirkle, presented in:
;; 
;; http://www.willpirkle.com/Downloads/AN-7Korg35HPF_V2.pdf 
;; 
;; [ARGS]
;; 
;; ain - audio input
;; acutoff - frequency of cutoff
;; kQ - filter Q [1, 10.0] (k35_hpf will clamp to boundaries)
;; knonlinear - use non-linear processing
;; ksaturation - saturation for tanh distortion

opcode k35_hpf, a, aKKKK

  ain, kcutoff, kQ, knonlinear, ksaturation xin

  kz1 init 0
  kz2 init 0
  kz3 init 0
  kv1 init 0
  kv2 init 0
  kv3 init 0
  aout init 0

  kg init 0
  kG init 0
  kK init 0
  klastcut init -1
  klastQ init -1
  kS35 init 0 
  kalpha init -1 
  khpf2_beta init -1 
  klpf1_beta init -1 

  kindx = 0
  kQ = limit(kQ, 1.0, 10.0)
  kcf = kcutoff 

  if (klastcut != kcf) then
    ; pre-warp the cutoff- these are bilinear-transform filters
    kwd = 2 * $M_PI * kcf
    iT  = 1/sr 
    kwa = (2/iT) * tan(kwd * iT/2) 
    kg  = kwa * iT/2 
    kG  = kg / (1 + kg)

  endif

  if (klastQ != kQ) then
    kK  = 0.01 + ((2.0 -  0.01) * (kQ / 10.0))
  endif

  if ((klastcut != kcf) || (klastQ != kQ)) then
    khpf2_beta = -kG / (1.0 + kg)
    klpf1_beta = 1.0 / (1.0 + kg)
    kalpha = 1.0 / (1.0 - (kK * kG) + (kK * kG * kG))
  endif

  klastcut = kcf
  klastQ = kQ
  
  while (kindx < ksmps) do
    ksig = ain[kindx]

    ;; hpf1
    kv1 = (ksig - kz1) * kG
    klp1 = kv1 + kz1 
    kz1 = klp1 + kv1
    ky1 = ksig - klp1
   
    ku = kalpha * (ky1 + kS35)
    ky = kK * ku

    if (knonlinear == 1) then
      ky = tanh(ky * ksaturation)
    endif

    ;; hpf2
    kv2 = (ky - kz2) * kG
    klp2 = kv2 + kz2 
    kz2 = klp2 + kv2
    khp2 = ky - klp2 

    ;; lpf1
    kv3 = (khp2 - kz3) * kG
    klp3 = kv3 + kz3 
    kz3 = klp3 + kv3

    kS35 = (khpf2_beta * kz2) + (klpf1_beta * kz3)

    kout = (kK > 0) ? (ky / kK) : ky 

    aout[kindx] = kout

    kindx += 1
  od

  xout aout

endop


opcode k35_hpf, a, aaKKK

  ain, acutoff, kQ, knonlinear, ksaturation xin

  kz1 init 0
  kz2 init 0
  kz3 init 0
  kv1 init 0
  kv2 init 0
  kv3 init 0
  aout init 0

  kg init 0
  kG init 0
  kK init 0
  klastcut init -1
  klastQ init -1
  kS35 init 0 
  kalpha init -1 
  khpf2_beta init -1 
  klpf1_beta init -1 

  kindx = 0
  kQ = limit(kQ, 1.0, 10.0)

  if (klastQ != kQ) then
    kK  = 0.01 + ((2.0 -  0.01) * (kQ / 10.0))
  endif

  klastQ = kQ
  
  while (kindx < ksmps) do
    kcf = acutoff[kindx]
    ksig = ain[kindx]

    if (klastcut != kcf) then
      ; pre-warp the cutoff- these are bilinear-transform filters
      kwd = 2 * $M_PI * kcf
      iT  = 1/sr 
      kwa = (2/iT) * tan(kwd * iT/2) 
      kg  = kwa * iT/2 
      kG  = kg / (1 + kg)

    endif

    if ((klastcut != kcf) || (klastQ != kQ)) then
      khpf2_beta = -kG / (1.0 + kg)
      klpf1_beta = 1.0 / (1.0 + kg)
      kalpha = 1.0 / (1.0 - (kK * kG) + (kK * kG * kG))
    endif

    ;; hpf1
    kv1 = (ksig - kz1) * kG
    klp1 = kv1 + kz1 
    kz1 = klp1 + kv1
    ky1 = ksig - klp1
   
    ku = kalpha * (ky1 + kS35)
    ky = kK * ku

    if (knonlinear == 1) then
      ky = tanh(ky * ksaturation)
    endif

    ;; hpf2
    kv2 = (ky - kz2) * kG
    klp2 = kv2 + kz2 
    kz2 = klp2 + kv2
    khp2 = ky - klp2 

    ;; lpf1
    kv3 = (khp2 - kz3) * kG
    klp3 = kv3 + kz3 
    kz3 = klp3 + kv3

    kS35 = (khpf2_beta * kz2) + (klpf1_beta * kz3)

    kout = (kK > 0) ? (ky / kK) : ky 

    aout[kindx] = kout

    klastcut = kcf
    kindx += 1
  od

  xout aout

endop
