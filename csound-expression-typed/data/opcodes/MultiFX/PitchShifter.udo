; PitchShifter
; ------------
; A pitch shifter effect based on FFT technology
;
; aout  PitchShifter  ain,kmix,kpitch,kfine,kfback
;
; Performance
; -----------
; ain    --  input audio to be pitch shifted
; kmix   --  dry / wet mix of the output signal (range 0 to 1)
; kpitch --  pitch shifting interval in thousands of a semitone (suggested range -0.012 to 0.012)
; kfine  --  fine control of pitch shifting interval in octaves (range -1/12 to 1/12)
; kfback --  control of the amount of output signal fed back into the input of the effect (suggested range 0 to 1)

opcode	PitchShifter,a,aKKKi
	ain,kmix,kscal,kfback,ifftsize	xin			;READ IN INPUT ARGUMENTS
	iWet	ftgentmp	0,0,1024,-7,0,512,1,512,1	;RESCALING FUNCTION FOR WET LEVEL CONTROL
	iDry	ftgentmp	0,0,1024,-7,1,512,1,512,0	;RESCALING FUNCTION FOR DRY LEVEL CONTROL
	kWet	table	kmix, iWet, 1				;RESCALE WET LEVEL CONTROL ACCORDING TO FUNCTION TABLE iWet
	kDry	table	kmix, iDry, 1				;RESCALE DRY LEVEL CONTROL ACCORDING TO FUNCTION TABLE iWet
	aPS	init	0                                       ;INITIALIZE aOutL FOR FIRST PERFORMANCE TIME PASS
	; kscal	=	octave(((kpitch*1000)/12)+kfine)	;DERIVE PITCH SCALING RATIO. NOTE THAT THE 'COARSE' PITCH DIAL BECOMES STEPPED IN SEMITONE INTERVALS	
	ioverlap  = ifftsize / 4
	iwinsize  = ifftsize
	fsig1 	pvsanal	ain+(aPS*kfback), ifftsize,ioverlap,iwinsize,0	;PHASE VOCODE ANALYSE LEFT CHANNEL
	fsig2 	pvscale fsig1, kscal				;RESCALE PITCH
	aPS 	pvsynth fsig2					;RESYNTHESIZE FROM FSIG
	aout	sum 	ain*kDry, aPS*kWet			;REDEFINE GLOBAL AUDIO SIGNAL FROM MIX OF DRY AND WET SIGNALS
		xout	aout					;SEND AUDIO BACK TO CALLER INSTRUMENT
endop
