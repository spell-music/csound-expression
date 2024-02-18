

/* 
   Transposed Direct Form II Biquad 

   Based on C++ code by Nigel Redmon: 
   http://www.earlevel.com/main/2012/11/26/biquad-c-source-code/


   OUTPUT
     aout - filtered signal

   INPUT
     asig - input signal
     ifilter_type - filter type: 
        0 - Low Pass
        1 - High Pass
        2 - Band Pass
        3 - Notch
        4 - Peaking
        5 - Low Shelf
        6 - High Shelf
     acutoff - cutoff frequency
     aQ - Q value
     again - gain (used by peaking, low shelf, and high shelf)

*/
opcode tdf2, a, aiaaa

asig, ifilter_type, acutoff, aQ, again xin

/* Memory */
kz1 init 0
kz2 init 0

klast_Fc init 0
klast_Q init 0
klast_gain init 0

ka0 init 0
ka1 init 0
ka2 init 0
kb1 init 0
kb2 init 0

aout init 0


kndx = 0
while (kndx < ksmps) do
  kcut = acutoff[kndx]
  kQ = aQ[kndx]
  kgain = again[kndx]

  if(kcut != klast_Fc || kQ != klast_Q || kgain != klast_gain) then

    kK = tan($M_PI * (kcut / sr))
    kV = pow(10, abs(kgain) / 20.0)
    kK2 = kK * kK

    if (ifilter_type == 0) then           ;; LPF
      knorm = 1 / (1 + kK / kQ + kK2)
      ka0 = kK2 * knorm
      ka1 = 2 * ka0
      ka2 = ka0
      kb1 = 2 * (kK2 - 1) * knorm
      kb2 = (1 - kK / kQ + kK2) * knorm

    elseif (ifilter_type == 1) then       ;; HPF
      knorm = 1 / (1 + kK / kQ + kK2)
      ka0 = 1 * knorm
      ka1 = -2 * ka0
      ka2 = ka0
      kb1 = 2 * (kK2 - 1) * knorm
      kb2 = (1 - kK / kQ + kK2) * knorm

    elseif (ifilter_type == 2) then       ;; BPF
      knorm = 1 / (1 + kK / kQ + kK2)
      ka0 = kK / kQ * knorm
      ka1 = 0
      ka2 = -ka0
      kb1 = 2 * (kK2 - 1) * knorm
      kb2 = (1 - kK / kQ + kK2) * knorm

    elseif (ifilter_type == 3) then       ;; Notch 
      knorm = 1 / (1 + kK / kQ + kK2)
      ka0 = (1 + kK2) * knorm
      ka1 = 2 * (kK2 - 1) * knorm
      ka2 = ka0
      kb1 = ka1
      kb2 = (1 - kK / kQ + kK2) * knorm

    elseif (ifilter_type == 4) then       ;; Peaking 
      if (kgain >= 0) then                ;; boost
          knorm = 1 / (1 + kK/kQ  + kK2);
          ka0 = (1 + kV/kQ * kK + kK2) * knorm;
          ka1 = 2 * (kK2 - 1) * knorm;
          ka2 = (1 - kV/kQ * kK + kK2) * knorm;
          kb1 = ka1;
          kb2 = (1 - 1/kQ * kK + kK2) * knorm;
      else                                ;; cut
          knorm = 1 / (1 + kV/kQ * kK + kK2)
          ka0 = (1 + kK/kQ + kK2) * knorm
          ka1 = 2 * (kK2 - 1) * knorm
          ka2 = (1 - kK/kQ + kK2) * knorm
          kb1 = ka1
          kb2 = (1 - kV/kQ * kK + kK2) * knorm
      endif

    elseif (ifilter_type == 5) then       ;; Low Shelf 
      if (kgain >= 0) then                ;; boost
        knorm = 1 / (1 + sqrt(2) * kK + kK2)
        ka0 = (1 + sqrt(2*kV) * kK + kV * kK2) * knorm
        ka1 = 2 * (kV * kK2 - 1) * knorm
        ka2 = (1 - sqrt(2*kV) * kK + kV * kK2) * knorm
        kb1 = 2 * (kK2 - 1) * knorm
        kb2 = (1 - sqrt(2) * kK + kK2) * knorm
      else                                ;; cut
        knorm = 1 / (1 + sqrt(2*kV) * kK + kV * kK2)
        ka0 = (1 + sqrt(2) * kK + kK2) * knorm
        ka1 = 2 * (kK2 - 1) * knorm
        ka2 = (1 - sqrt(2) * kK + kK2) * knorm
        kb1 = 2 * (kV * kK2 - 1) * knorm
        kb2 = (1 - sqrt(2*kV) * kK + kV * kK2) * knorm
      endif

    elseif (ifilter_type == 6) then       ;; High Shelf 
      if (kgain >= 0) then                ;; boost
        knorm = 1 / (1 + sqrt(2) * kK + kK2)
        ka0 = (kV + sqrt(2*kV) * kK + kK2) * knorm
        ka1 = 2 * (kK2 - kV) * knorm
        ka2 = (kV - sqrt(2*kV) * kK + kK2) * knorm
        kb1 = 2 * (kK2 - 1) * knorm
        kb2 = (1 - sqrt(2) * kK + kK2) * knorm
      else                                ;; cut
        knorm = 1 / (kV + sqrt(2*kV) * kK + kK2)
        ka0 = (1 + sqrt(2) * kK + kK2) * knorm
        ka1 = 2 * (kK2 - 1) * knorm
        ka2 = (1 - sqrt(2) * kK + kK2) * knorm
        kb1 = 2 * (kK2 - kV) * knorm
        kb2 = (kV - sqrt(2*kV) * kK + kK2) * knorm
      endif

    endif
  endif

  kin = asig[kndx]

  /* TDF2 Biquad Calculation */
  kout = kin * ka0 + kz1
  kz1 = kin * ka1 + kz2 - kb1 * kout
  kz2 = kin * ka2 - kb2 * kout

  /* Output, state saving for next pass */
  aout[kndx] = kout

  klast_Fc = kcut
  klast_Q = kQ

  kndx += 1
od

xout aout

endop

