; RingModulator
; ----------------
; An ring modulating effect with an envelope follower
;
; aout  RingModulator  ain,kmix,kfreq,kenv
;
; Performance
; -----------
; ain    --  input audio to be pitch shifted
; kmix   --  dry / wet mix of the output signal (range 0 to 1)
; kfreq  --  frequency of thew ring modulator *NOT IN HERTZ* (range 0 to 1)
; kenv   --  amount of dynamic envelope following modulation of frequency (range 0 to 1)

opcode	RingModulator,a,aKKK
	ain,kmix,kfreq,kenv	xin					;READ IN INPUT ARGUMENTS
	kfreq		expcurve	kfreq,4				;CREATE AN EXPONENTIAL REMAPPING OF kfreq
	kfreq		scale	kfreq,5000,10				;RESCALE 0 - 1 VALUE TO 10 - 5000
	iWet	ftgentmp	0,0,1024,-7,0,512,1,512,1		;RESCALING FUNCTION FOR WET LEVEL CONTROL
	iDry	ftgentmp	0,0,1024,-7,1,512,1,512,0		;RESCALING FUNCTION FOR DRY LEVEL CONTROL
	isine	ftgentmp	0,0,4096,10,1				;SINE WAVE
	kWet	table	kmix, iWet, 1					;RESCALE WET LEVEL CONTROL ACCORDING TO FUNCTION TABLE iWet
	kDry	table	kmix, iDry, 1					;RESCALE DRY LEVEL CONTROL ACCORDING TO FUNCTION TABLE iDry
	kporttime	linseg	0,0.001,0.02				;PORTAMENTO VARIABLE
	kModFrq	portk	kfreq, kporttime				;SMOOTH VARIABLE CHANGES
	aFollow		follow2		ain, 0.01, 0.1			;AMPLITUDE FOLLOWING AUDIO SIGNAL
	kFollow		downsamp	aFollow
	kFollow	logcurve	kFollow/0dbfs,20
	kModFrq	=	kModFrq + (cpsoct(kFollow*kenv*30))     	;CREATE A LEFT CHANNEL MODULATING FREQUENCY BASE ON THE STATIC VALUE CREATED BY kfreq AND THE AMOUNT OF DYNAMIC ENVELOPE FOLLOWING GOVERNED BY kenv
	aMod	poscil	1, kModFrq, isine  				;CREATE RING MODULATING SIGNAL
	aout	sum	ain*kDry, ain*aMod*kWet				;MIX DRY AND WET SIGNALS
		xout	aout						;SEND AUDIO BACK TO CALLER INSTRUMENT
endop
