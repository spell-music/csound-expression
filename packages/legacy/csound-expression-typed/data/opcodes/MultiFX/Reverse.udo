; Reverse
; ----------------
; An effect that reverses an audio stream in chunks
;
; aout  Reverse  ain,ktime
;
; Performance
; -----------
; ain    --  input audio to be reversed
; ktime  --  time duration of each chunk (suggested range: 0.3 to 2)

opcode	Reverse, a, aK				;nb. CAPITAL K CREATE A K-RATE VARIABLE THAT HAS A USEFUL VALUE ALSO AT I-TIME
	ain,ktime	xin			;READ IN INPUT ARGUMENTS
	ktrig	changed	ktime			;IF ktime CONTROL IS MOVED GENERATE A MOMENTARY '1' IMPULSE
	if ktrig=1 then				;IF A TRIGGER HAS BEEN GENERATED IN THE LINE ABOVE...
		reinit	UPDATE			;...BEGIN A REINITILISATION PASS FROM LABEL 'UPDATE'
	endif					;END OF CONDITIONAL BRANCH
	UPDATE:					;LABEL CALLED 'UPDATE'
	itime	=	i(ktime)		;CREATE AN I-TIME VERSION OF ktime
	aptr	phasor	2/itime			;CREATE A MOVING PHASOR THAT WITH BE USED TO TAP THE DELAY BUFFER
	aptr	=	aptr*itime		;SCALE PHASOR ACCORDING TO THE LENGTH OF THE DELAY TIME CHOSEN BY THE USER
	ienv	ftgentmp	0,0,1024,7,0,(1024*0.01),1,(1024*0.98),1,(0.01*1024),0	;ANTI-CLICK ENVELOPE SHAPE
 	aenv	poscil	1, 2/itime, ienv	;CREATE A CYCLING AMPLITUDE ENVELOPE THAT WILL SYNC TO THE TAP DELAY TIME PHASOR 
 	abuffer	delayr	itime			;CREATE A DELAY BUFFER
	atap	deltap3	aptr			;READ AUDIO FROM A TAP WITHIN THE DELAY BUFFER
		delayw	ain			;WRITE AUDIO INTO DELAY BUFFER
	rireturn				;RETURN FROM REINITIALISATION PASS
	xout	atap*aenv			;SEND AUDIO BACK TO CALLER INSTRUMENT. APPLY AMPLITUDE ENVELOPE TO PREVENT CLICKS.
endop
