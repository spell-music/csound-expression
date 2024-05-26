; LoFi
; ----------------
; 'Low Fidelity' distorting effects of bit reduction and downsampling (foldover)
;
; aout  LoFi  ain,kbits,kfold
;
; Performance
; -----------
; ain    --  input audio to have low fidelity distortion effects applied
; kbits  --  bit depth reduction (suggested range 0 to 0.6)
; kfold  --  amount of foldover (range 0 to 1)


opcode	LoFi,a,aKK
	ain,kbits,kfold	xin									;READ IN INPUT ARGUMENTS
	kfold		expcurve	kfold,500						;CREATE AN EXPONENTIAL REMAPPING OF kfold
	kfold		scale	kfold,1024,1							;RESCALE 0 - 1 VALUE TO 1 - 1024	
	kvalues		pow	2, ((1-(kbits^0.25))*15)+1					;RAISES 2 TO THE POWER OF kbitdepth. THE OUTPUT VALUE REPRESENTS THE NUMBER OF POSSIBLE VALUES AT THAT PARTICULAR BIT DEPTH
	k16bit		pow	2, 16								;RAISES 2 TO THE POWER OF 16
	aout		=	(int((ain*32768*kvalues)/k16bit)/32768)*(k16bit/kvalues)	;BIT DEPTH REDUCE AUDIO SIGNAL
	aout		fold 	aout, kfold							;APPLY SAMPLING RATE FOLDOVER
		xout	aout									;SEND AUDIO BACK TO CALLER INSTRUMENT
endop
