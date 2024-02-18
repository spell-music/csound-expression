; AnalogDelay
; ----------------
; A analog style delay with signal degradation and saturation options
;
; aout  AnalogDelay  ain,kmix,ktime,kfback,ktone
;
; Performance
; -----------
; ain    --  input audio to which the flanging effect will be applied
; kmix   --  dry / wet mix of the output signal (range 0 to 1)
; ktime  --  delay time of the effect in seconds
; kfback --  control of the amount of output signal fed back into the input of the effect (exceeding 1 (100%) is possible and will result in saturation clipping effects)
; ktone  --  control of the amount of output signal fed back into the input of the effect (range 0 to 1)


opcode	AnalogDelay,a,aKKKK
	ain,kmix,ktime,kfback,ktone	xin			;READ IN INPUT ARGUMENTS
	ktone	expcurve	ktone,4				;CREATE AN EXPONENTIAL REMAPPING OF ktone
	ktone	scale	ktone,12000,100				;RESCALE 0 - 1 VALUE
	iWet	ftgentmp	0,0,1024,-7,0,512,1,512,1	;RESCALING FUNCTION FOR WET LEVEL CONTROL
	iDry	ftgentmp	0,0,1024,-7,1,512,1,512,0	;RESCALING FUNCTION FOR DRY LEVEL CONTROL
	kWet	table	kmix, iWet, 1				;RESCALE WET LEVEL CONTROL ACCORDING TO FUNCTION TABLE iWet
	kDry	table	kmix, iDry, 1                 		;RESCALE DRY LEVEL CONTROL ACCORDING TO FUNCTION TABLE iWet
	kporttime	linseg	0,0.001,0.1			;RAMPING UP PORTAMENTO TIME
	kTime	portk	ktime, kporttime*3			;APPLY PORTAMENTO SMOOTHING TO DELAY TIME PARAMETER
	kTone	portk	ktone, kporttime			;APPLY PORTAMENTO SMOOTHING TO TONE PARAMETER
	aTime	interp	kTime					;INTERPOLATE AND CREAT A-RATE VERSION OF DELAY TIME PARAMETER
	aBuffer	delayr	5					;READ FROM (AND INITIALIZE) BUFFER
	atap	deltap3	aTime					;TAP DELAY BUFFER
	atap	clip	atap, 0, 0dbfs*0.8			;SIGNAL IS CLIPPED AT MAXIMUM AMPLITUDE USING BRAM DE JONG METHOD
	atap	tone	atap, kTone				;LOW-PASS FILTER DELAY TAP WITHIN DELAY BUFFER 
		delayw	ain+(atap*kfback)			;WRITE INPUT AUDIO AND FEEDBACK SIGNAL INTO DELAY BUFFER
	aout	sum	ain*kDry, atap*kWet			;MIX DRY AND WET SIGNALS 
		xout	aout					;SEND AUDIO BACK TO CALLER INSTRUMENT
endop
