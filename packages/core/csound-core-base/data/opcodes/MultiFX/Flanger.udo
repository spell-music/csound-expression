; Flanger
; ----------------
; A flanger effect following the typical design of a so called 'stomp box'
;
; aout  Flanger  ain,krate,kdepth,kdelay,kfback
;
; Performance
; -----------
; ain    --  input audio to which the flanging effect will be applied
; krate  --  rate control of the lfo of the effect *NOT IN HERTZ* (range 0 to 1)
; kdepth --  depth of the lfo of the effect (range 0 to 1)
; kdelay --  static delay offset of the flanging effect (range 0 to 1)
; kfback --  feedback and therefore intensity of the effect (range 0 to 1)


opcode	Flanger,a,aKKKK
	ain,krate,kdepth,kdelay,kfback	xin					;READ IN INPUT ARGUMENTS
	krate		expcurve	krate,50				;CREATE AN EXPONENTIAL REMAPPING OF krate
	krate		scale	krate,14,0.001					;RESCALE VALUE	
	kdelay		expcurve	kdelay,200				;CREATE AN EXPONENTIAL REMAPPING OF kdelay
	kdelay		scale		kdelay,0.1,0.00015			;RESCALE VALUE	
	ilfoshape	ftgentmp	0, 0, 131072, 19, 0.5, 1, 180, 1	;U-SHAPE PARABOLA FOR LFO
	kporttime	linseg		0, 0.001, 0.1 				;USE OF AN ENVELOPE VALUE THAT QUICKLY RAMPS UP FROM ZERON TO THE REQUIRED VALUE PREVENTS VARIABLES GLIDING TO THEIR REQUIRED VALUES EACH TIME THE INSTRUMENT IS STARTED
	kdlt		portk		kdelay, kporttime 			;PORTAMENTO IS APPLIED TO A VARIABLE. A NEW VARIABLE 'kdlt' IS CREATED.
	adlt		interp		kdlt					;A NEW A-RATE VARIABLE 'adlt' IS CREATED BY INTERPOLATING THE K-RATE VARIABLE 'kdlt'
	kdep		portk		kdepth*0.01, kporttime 			;PORTAMENTO IS APPLIED TO A VARIABLE. A NEW VARIABLE 'kdep' IS CREATED.
	amod		oscili		kdep, krate, ilfoshape			;OSCILLATOR THAT MAKES USE OF THE POSITIVE DOMAIN ONLY U-SHAPE PARABOLA WITH FUNCTION TABLE NUMBER ilfoshape
	adlt		sum		adlt, amod				;STATIC DELAY TIME AND MODULATING DELAY TIME ARE SUMMED
	adelsig		flanger 	ain, adlt, kfback , 1.2			;FLANGER SIGNAL CREATED
	aout		sum		ain*0.5, adelsig*0.5			;CREATE DRY/WET MIX 
			xout		aout					;SEND AUDIO BACK TO CALLER INSTRUMENT
endop
