; StChorus
; ----------------
; A stereo chorus effect
;
; aout  StChorus  ainL,ainR,krate,kdepth,kwidth
;
; Performance
; -----------
; ainL   --  first/left input audio
; ainR   --  second/right input audio
; krate  --  rate control of the lfo of the effect *NOT IN HERTZ* (range 0 to 1)
; kdepth --  depth of the lfo of the effect (range 0 to 1)
; kwidth --  width of stereo widening (range 0 to 1)


opcode	StChorus,aa,aaKKK
	ainL,ainR,krate,kdepth,kwidth	xin					;READ IN INPUT ARGUMENTS
	krate	expcurve	krate,20					;CREATE AN EXPONENTIAL REMAPPING OF krate
	krate	scale	krate,7,0.001						;RESCALE VALUE
	ilfoshape	ftgentmp	0, 0, 131072, 19, 1, 0.5, 0,  0.5	;POSITIVE DOMAIN ONLY SINE WAVE	
	kporttime	linseg	0,0.001,0.02					;RAMPING UP PORTAMENTO VARIABLE
	kChoDepth	portk	kdepth*0.01, kporttime				;SMOOTH VARIABLE CHANGES WITH PORTK
	aChoDepth	interp	kChoDepth					;INTERPOLATE TO CREATE A-RATE VERSION OF K-RATE VARIABLE
	amodL 		osciliktp 	krate, ilfoshape, 0			;LEFT CHANNEL LFO
	amodR 		osciliktp 	krate, ilfoshape, kwidth*0.5		;THE PHASE OF THE RIGHT CHANNEL LFO IS ADJUSTABLE
	amodL		=		(amodL*aChoDepth)+.01			;RESCALE AND OFFSET LFO (LEFT CHANNEL)
	amodR		=		(amodR*aChoDepth)+.01			;RESCALE AND OFFSET LFO (RIGHT CHANNEL)
	aChoL		vdelay	ainL, amodL*1000, 1.2*1000			;CREATE VARYING DELAYED / CHORUSED SIGNAL (LEFT CHANNEL) 
	aChoR		vdelay	ainR, amodR*1000, 1.2*1000			;CREATE VARYING DELAYED / CHORUSED SIGNAL (RIGHT CHANNEL)
	aoutL		sum 	aChoL*0.6, ainL*0.6                 		;MIX DRY AND WET SIGNAL (LEFT CHANNEL) 
	aoutR		sum 	aChoR*0.6, ainR*0.6				;MIX DRY AND WET SIGNAL (RIGHT CHANNEL)
			xout	aoutL,aoutR					;SEND AUDIO BACK TO CALLER INSTRUMENT
endop