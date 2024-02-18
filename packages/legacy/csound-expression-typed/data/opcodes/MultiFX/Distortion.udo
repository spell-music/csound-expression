; Distortion
; ----------------
; A distortion effect offering stomp-box-like controls
;
; aout  Distortion  ain,klevel,kdrive,ktone
;
; Performance
; -----------
; ain    --  input audio to be distorted
; klevel --  output level of the effect (range: 0 to 1)
; kdrive --  intensity of the distortion effect (range: 0 to 1)
; ktone  --  tone of a lowpass filter (range: 0 to 1)

opcode	Distortion, a, aKKK
	ain,klevel,kdrive,ktone	xin							;READ IN INPUT ARGUMENTS
	klevel		scale		klevel,0.8,0					;RESCALE LEVEL CONTROL
	kdrive		expcurve	kdrive,8					;EXPONENTIALLY REMAP kdrive
	kdrive		scale		kdrive,0.4,0.01					;RESCALE kdrive
	kLPF		expcurve	ktone,4						;EXPONENTIALLY REMAP ktone
	kLPF		scale		kLPF,12000,200					;RESCALE klpf
	kGainComp1	logcurve	ktone,700					;LOGARITHMIC RESCALING OF ktone TO CREAT A GAIN COMPENSATION VARIABLE FOR WHEN TONE IS LOWERED
	kGainComp1	scale		kGainComp1,1,5					;RESCALE GAIN COMPENSATION VARIABLE
	kpregain	=		(kdrive*100)					;DEFINE PREGAIN FROM kdrive
	kpostgain	=		0.5 * (((1-kdrive) * 0.4) + 0.6)		;DEFINE POSTGAIN FROM kdrive
	aDist		distort1	ain*(32768/0dbfs), kpregain, kpostgain, 0, 0	;CREATE DISTORTION SIGNAL
	aDist		butlp		aDist/(32768/0dbfs), kLPF			;LOWPASS FILTER DISTORTED SIGNAL
			xout		aDist*klevel*kGainComp1				;SEND AUDIO BACK TO CALLER INSTRUMENT. RESCALE WITH USER LEVEL CONTROL AND GAIN COMPENSATION			
endop
