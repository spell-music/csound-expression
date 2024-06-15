; FreqShifter
; ----------------
; A frequency shifter effect using the hilbert filter
;
; aout  FreqShifter  adry,kmix,kfreq,kmult,kfback
;
; Performance
; -----------
; adry   --  input audio to be frequency shifted
; kmix   --  dry / wet mix of the output signal (range 0 to 1)
; kfreq  --  frequency of frequency shifter effect (suggested range -1000 to 1000)
; kmult  --  multiplier of frequency value for fine tuning control (suggested range -1 to 1)
; kfback --  control of the amount of output signal fed back into the input of the effect (suggested range 0 to 1)

opcode	FreqShifter,a,aKKKK
	adry,kmix,kfreq,kmult,kfback	xin			;READ IN INPUT ARGUMENTS
	iWet	ftgentmp	0,0,1024,-7,0,512,1,512,1	;RESCALING FUNCTION FOR WET LEVEL CONTROL
	iDry	ftgentmp	0,0,1024,-7,1,512,1,512,0	;RESCALING FUNCTION FOR DRY LEVEL CONTROL
	isine	ftgentmp	0,0,4096,10,1			;A SINE WAVE SHAPE
	kWet	table	kmix, iWet, 1				;RESCALE WET LEVEL CONTROL ACCORDING TO FUNCTION TABLE giWet
	kDry	table	kmix, iDry, 1				;RESCALE DRY LEVEL CONTROL ACCORDING TO FUNCTION TABLE giWet
	aFS	init	0					;INITILISE FEEDBACK SIGNAL (FOR FIRST K-PASS)
	ain	=	adry + (aFS * kfback)			;ADD FEEDBACK SIGNAL TO INPUT (AMOUNT OF FEEDBACK CONTROLLED BY 'Feedback Gain' SLIDER)
	areal, aimag hilbert ain				;HILBERT OPCODE OUTPUTS TWO PHASE SHIFTED SIGNALS, EACH 90 OUT OF PHASE WITH EACH OTHER
	kporttime	linseg	0,0.001,0.02
	kfshift	portk	kfreq*kmult, kporttime
	;QUADRATURE OSCILLATORS. I.E. 90 OUT OF PHASE WITH RESPECT TO EACH OTHER
	;OUTUTS	OPCODE	AMPLITUDE | FREQ. | FUNCTION_TABLE | INITIAL_PHASE (OPTIONAL;DEFAULTS TO ZERO)
	asin 	oscili       1,    kfshift,     isine,          0
	acos 	oscili       1,    kfshift,     isine,          0.25	
	;RING MODULATE EACH SIGNAL USING THE QUADRATURE OSCILLATORS AS MODULATORS
	amod1	=		areal * acos
	amod2	=		aimag * asin	
	;UPSHIFTING OUTPUT
	aFS	= (amod1 - amod2)
	aout	sum	aFS*kWet, adry*kDry		;CREATE WET/DRY MIX
		xout	aout				;SEND AUDIO BACK TO CALLER INSTRUMENT
endop
