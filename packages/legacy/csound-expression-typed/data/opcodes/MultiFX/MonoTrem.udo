
; MonoTrem
; ----------------
; Tremolo effect
;
; aout  MonoTrem  ain,krate,kdepth,kwave
;
; Performance
; -----------
; ain    --  input audio
; krate  --  rate control of the lfo of the effect *NOT IN HERTZ* (range 0 to 1)
; kdepth --  depth of the lfo of the effect (range 0 to 1)
; kwave  --  waveform used by the lfo (0=sine 1=triangle 2=square)

opcode  MonoTrem,a,aKKK
    ain,krate,kdepth,kwave  xin ;READ IN INPUT ARGUMENTS
    krate   expcurve    krate,5         ;CREATE AN EXPONENTIAL REMAPPING OF krate
    krate   scale   krate,50,0.1            ;RESCALE VALUE
    ktrig   changed kwave               ;IF LFO WAVEFORM TYPE IS CHANGED GENERATE A MOMENTARY '1' (BANG)
    if ktrig=1 then                 ;IF A 'BANG' HAS BEEN GENERATED IN THE ABOVE LINE
        reinit  UPDATE              ;BEGIN A REINITIALIZATION PASS FROM LABEL 'UPDATE' SO THAT LFO WAVEFORM TYPE CAN BE UPDATED
    endif                       ;END OF THIS CONDITIONAL BRANCH
    UPDATE:                     ;LABEL CALLED UPDATE
    klfo    lfo kdepth, krate, i(kwave)     ;CREATE AN LFO
    rireturn                            ;RETURN FROM REINITIALIZATION PASS
    klfo    =   (klfo*0.5)+0.5          ;RESCALE AND OFFSET LFO SO IT STAY WITHIN THE RANGE 0 - 1 ABOUT THE VALUE 0.5
    if kwave=2 then                     ;IF SQUARE WAVE MODULATION HAS BEEN CHOSEN...
        klfo    portk   klfo, 0.001     ;SMOOTH THE SQUARE WAVE A TINY BIT TO PREVENT CLICKS
    endif                               ;END OF THIS CONDITIONAL BRANCH 
    
    klfo    =   klfo+(0.5-(kdepth*0.5)) ;MODIFY LFO AT ZERO DEPTH VALUE IS 1 AND AT MAX DEPTH CENTRE OF MODULATION IS 0.5
    alfo    interp  klfo                ;INTERPOLATE K-RATE LFO AND CREATE A-RATE VARIABLE
    aout   =   ain*(alfo^2)             ;REDEFINE GLOBAL AUDIO LEFT CHANNEL SIGNAL WITH TREMELO    
    xout    aout                        ;SEND AUDIO BACK TO CALLER INSTRUMENT
endop