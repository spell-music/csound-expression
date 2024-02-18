; Phaser
; ----------------
; An phase shifting effect that mimics the design of a so called 'stomp box'
;
; aout  Phaser  ain,krate,kdepth,kfreq,kfback
;
; Performance
; -----------
; ain    --  input audio to be pitch shifted
; krate  --  rate of lfo of the effect (range 0 to 1)
; kdepth --  depth of lfo of the effect (range 0 to 1)
; kfreq  --  centre frequency of the phase shifting effect in octaves (suggested range 6 to 11)
; kfback --  feedback and therefore intensity of the effect (range 0 to 1)

opcode	Phaser,a,aKKKK
	ain,krate,kdepth,kfreq,kfback	xin					;READ IN INPUT ARGUMENTS
	krate		expcurve	krate,10				;CREATE AN EXPONENTIAL REMAPPING OF krate
	krate		scale	krate,14,0.01					;RESCALE 0 - 1 VALUE TO 0.01 - 14	
	klfo	lfo	kdepth*0.5, krate, 1					;LFO FOR THE PHASER (TRIANGULAR SHAPE)
	aout	phaser1	ain, cpsoct((klfo+(kdepth*0.5)+kfreq)), 8, kfback	;PHASER1 IS APPLIED TO THE INPUT AUDIO
		xout	aout							;SEND AUDIO BACK TO CALLER INSTRUMENT
endop
