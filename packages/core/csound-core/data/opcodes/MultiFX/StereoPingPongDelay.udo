opcode StereoPingPongDelay, aa, aaKKKKKi
    aInL, aInR, kdelayTime, kFeedback, kMix, kWidth, kDamp, iMaxDelayTime xin

    iporttime   =       .1          ;PORTAMENTO TIME
    kporttime   linseg      0, .001, iporttime  ;USE OF AN ENVELOPE VALUE THAT QUICKLY RAMPS UP FROM ZERO TO THE REQUIRED VALUE. THIS PREVENTS VARIABLES GLIDING TO THEIR REQUIRED VALUES EACH TIME THE INSTRUMENT IS STARTED
    kdlt        portk       kdelayTime, kporttime    ;PORTAMENTO IS APPLIED TO THE VARIABLE 'gkdlt'. A NEW VARIABLE 'kdlt' IS CREATED.
    adlt        interp      kdlt            ;A NEW A-RATE VARIABLE 'adlt' IS CREATED BY INTERPOLATING THE K-RATE VARIABLE 'kdlt' 

    ;;;LEFT CHANNEL OFFSET;;;NO FEEDBACK!!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    abufferL_OS delayr  iMaxDelayTime          ;CREATE A DELAY BUFFER OF imaxdelay SECONDS DURATION
    adelsigL_OS     deltap3 adlt                ;TAP THE DELAY LINE AT adlt SECONDS
    adelsigL_OS tone adelsigL_OS, kDamp
            delayw  aInL                ;WRITE AUDIO SOURCE INTO THE BEGINNING OF THE BUFFER

    ;;;LEFT CHANNEL DELAY;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    abufferL    delayr  iMaxDelayTime*2            ;CREATE A DELAY BUFFER OF 5 SECONDS DURATION (EQUIVALENT TO THE MAXIMUM DELAY TIME POSSIBLE USING THIS EXAMPLE)
    adelsigL    deltap3 adlt*2              ;TAP THE DELAY LINE AT gkdlt SECONDS
    adelsigL    tone adelsigL, kDamp
            delayw  adelsigL_OS + (adelsigL * kFeedback)    ;WRITE AUDIO SOURCE FROM OFFSETTTING DELAY AND FEEDBACK SIGNAL INTO THE BEGINNING OF THE BUFFER
    
    abufferR    delayr  iMaxDelayTime*2            ;CREATE A DELAY BUFFER OF 5 SECONDS DURATION (EQUIVALENT TO THE MAXIMUM DELAY TIME POSSIBLE USING THIS EXAMPLE)
    adelsigR    deltap3 adlt*2              ;TAP THE DELAY LINE AT gkdlt SECONDS
    adelsigR    tone adelsigR, kDamp
            delayw  aInR+(adelsigR*kFeedback)   ;WRITE AUDIO SOURCE AND FEEDBACK SIGNAL INTO THE BEGINNING OF THE BUFFER

    ;CREATE LEFT AND RIGHT CHANNEL MIXES
    aOutL       sum     (adelsigL  + adelsigL_OS)* kMix, aInL * (1-kMix), (1 - kWidth) * adelsigR
    aOutR       sum     adelsigR                 * kMix, aInR * (1-kMix), (1 - kWidth) * adelsigL     
            xout        aOutL, aOutR        ;CREATE A MIX BETWEEN THE WET AND THE DRY SIGNALS AT THE OUTPUT
endop
