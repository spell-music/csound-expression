; PitchShifterDelay
; ----------------
; A pitch shifter effect that employs delay lines
;
; aout  PitchShifterDelay  ain,ktrans,kdlt,kFB1,kFB2,imaxdlt
;
; Initialisation
; --------------
; imaxdlt --  maximum delay time (kdlt should not exceed this value)
;
; Performance
; -----------
; ain     --  input audio to be pitch shifted
; ktrans  --  pitch transposition (in semitones)
; kdlt    --  delay time employed by the pitch shifter effect (should be within the range ksmps/sr and imaxdlt) 
; kFB1    --  feedback using method 1 (output from delay taps are fed back directly into their own buffers before enveloping and mixing)
; kFB2    --  feedback using method 2 (enveloped and mixed output from both taps is fed back into both buffers)
 
opcode	PitchShifterDelay,a,aKKKKi
	;		setksmps	1			;UDO ksmps CAN BE SET INDEPENDENTLY OF GLOBAL ksmps
	ain,ktrans,kdlt,kFB1,kFB2,imaxdlt	xin
	ihalfsine	ftgen	0, 0, 1025, 9, 0.5, 1, 0	;HALF SINE  WINDOW FUNCTION USED FOR AMPLITUDE ENVELOPING
	koctfract	=	ktrans/12			;TRANSPOSITION AS FRACTION OF AN OCTAVE
	kratio		=	octave(koctfract)		;RATIO TO PRODUCE PITCH SHIFT
	krate		=	(kratio-1)/kdlt			;SUBTRACT 1/1 SPEED
	
	aphase1		phasor	-krate				;MOVING PHASE 1-0
	aphase2		phasor	-krate, .5			;MOVING PHASE 1-0 - PHASE OFFSET BY 180 DEGREES (.5 RADIANS)
	
	agate1		tablei	aphase1, ihalfsine, 1, 0, 1	;WINDOW FUNC =HALF SINE
	agate2		tablei	aphase2, ihalfsine, 1, 0, 1	;WINDOW FUNC =HALF SINE
	
	adlt		interp	kdlt				;CREATE A RATE (INTERPOLATED FROM K-RATE) VERSION OF kdlt
	aout		init	0				;INITIALISE OUTPUT AUDIO SIGNAL (NEEDED FOR FEEDBACK SIGNAL (METHOD 2))
	
	abuffer		delayr	imaxdlt				;DECLARE DELAY BUFFER
	adelsig1	deltap3	aphase1 * adlt			;VARIABLE TAP
	aGatedSig1	=	adelsig1 * agate1
			delayw	ain + (aGatedSig1*kFB1) + (aout*kFB2)	;WRITE AUDIO TO THE BEGINNING OF THE DELAY BUFFER, MIX IN FEEDBACK SIGNAL - PROPORTION DEFINED BY gkFB
	
	abuffer		delayr	imaxdlt				;DECLARE DELAY BUFFER
	adelsig2	deltap3	aphase2 * adlt			;VARIABLE TAP
	aGatedSig2	=	adelsig2 * agate2
			delayw	ain + (aGatedSig2*kFB1) + (aout*kFB2)	;WRITE AUDIO TO THE BEGINNING OF THE DELAY BUFFER, MIX IN FEEDBACK SIGNAL - PROPORTION DEFINED BY gkFB
	aout		=	(aGatedSig1 + aGatedSig2) * 0.5
	aout		dcblock2	aout			;REMOVE DC OFFSET (DC OFFSET CAN SOMETIMES BE A PROBLEM WHEN BOTH FEEDBACKS ARE COMBINED)
		xout	aout					;SUM AND RESCALE PITCH SHIFTER OUTPUTS (LEFT CHANNEL)
endop

