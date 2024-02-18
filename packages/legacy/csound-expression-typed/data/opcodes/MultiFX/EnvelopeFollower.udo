; EnvelopeFollower
; ----------------
; A dynamic envelope following resonant lowpass filter
;
; aout  EnvelopeFollower  ain,ksens,kfreq,kres
;
; Performance
; -----------
; ain    --  input audio to be filtered
; ksens  --  sensitivity of the envelope follower (suggested range: 0 to 1)
; kfreq  --  base frequency of the filter before modulation by the input dynamics (range: 0 to 1)
; kres   --  resonance of the lowpass filter (suggested range: 0 to 0.99)


opcode	EnvelopeFollower,a,aKKK
	ain,ksens,kfreq,kres	xin						;READ IN INPUT ARGUMENTS
	kfreq		expcurve	kfreq,4					;CREATE AN EXPONENTIAL REMAPPING OF kfreq
	kfreq		scale	kfreq,10000,10					;RESCALE 0 - 1 VALUE
	ksens		logcurve	ksens,100					;CREATE LOGARITHMIC REMAPPING OF ksens
	aFollow		follow2		ain, 0.01, 0.05				;AMPLITUDE FOLLOWING AUDIO SIGNAL
	kFollow		downsamp	aFollow					;DOWNSAMPLE TO K-RATE
	kFollow		expcurve	kFollow/0dbfs,3				;ADJUSTMENT OF THE RESPONSE OF DYNAMICS TO FILTER FREQUENCY MODULATION
	kFrq		=		kfreq + (cpsoct(kFollow*ksens*150))	;CREATE A LEFT CHANNEL MODULATING FREQUENCY BASE ON THE STATIC VALUE CREATED BY kfreq AND THE AMOUNT OF DYNAMIC ENVELOPE FOLLOWING GOVERNED BY ksens
	kFrq		port		kFrq, 0.05				;SMOOTH CONTROL SIGNAL USING PORTAMENTO
	kFrq		limit		kFrq, 20,sr/2				;LIMIT FREQUENCY RANGE TO PREVENT OUT OF RANGE FREQUENCIES  
	;IF REALTIME PERFORMNCE IS AN ISSUE, USE moogvcf2 INSTEAD OF moogladder
	aout		moogladder	ain, kFrq, kres				;REDEFINE GLOBAL AUDIO SIGNAL AS FILTERED VERSION OF ITSELF
			xout		aout					;SEND AUDIO BACK TO CALLER INSTRUMENT
endop
