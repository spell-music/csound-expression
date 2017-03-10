StereoPingPongDelay.csd
Written by Iain McCurdy, 2008

THE DELAY IS PLACED IN A SEPARATE, ALWAYS ON, INSTRUMENT FROM THE SOURCE SOUND PRODUCING INSTRUMENT.
THE IS A COMMONLY USED TECHNIQUE WITH TIME SMEARING OPCODES AND EFFECTS LIKE REVERBS AND DELAYS.

;ksmps MAY NEED TO BE LOW (AND kr THEREFORE HIGH) WHEN WORKING WITH SHORT DELAY TIMES DEFINED INITIALLY AT KRATE

<CsoundSynthesizer>

<CsOptions>
-iadc -odac -dm0
</CsOptions>

<CsInstruments>

sr 		= 	44100	;SAMPLE RATE
ksmps 		= 	32	;NUMBER OF AUDIO SAMPLES IN EACH CONTROL CYCLE (MAY NEED TO BE LOW WHEN WORKING WITH SHORT DELAY TIMES DEFINED INITIALLY AT KRATE)
nchnls 		= 	2	;NUMBER OF CHANNELS (2=STEREO)
0dbfs		=	1	;MAXIMUM AMPLITUDE

gimaxdelay	=	5	;MAXIMUM DELAY TIME VARIABLE USED BOTH BY THE ORCHESTRA CODE AND BY FLTK WIDGET CODE IN THE HEADER

;FLTK INTERFACE CODE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FLcolor	255, 255, 255, 250, 50, 50 	;SETUP BASIC COLOURS
;			LABEL                    | WIDTH | HEIGHT | X | Y
		FLpanel	"Stereo Ping-Pong Delay",   500,    300,    0,  0

;                                                      				ON | OFF | TYPE | WIDTH | HEIGHT | X | Y | OPCODE | INS | STARTTIM | DUR
gkImpulse,ihImpulse		FLbutton	"Short Impulse Sound",		1,    0,    21,    200,     40,    5,  5,    0,      1,      0,      0.1

;GENERAL_TEXT_SETTINGS			SIZE | FONT |  ALIGN | RED | GREEN | BLUE
			FLlabel		13,      1,      3,    255,   255,   255		;LABELS MADE INVISIBLE (I.E. SAME COLOR AS PANEL)
;BUTTON BANKS					TYPE | NUMX | NUMY | WIDTH | HEIGHT | X | Y | OPCODE | INS | STARTTIM | DUR
gkmethod, ihmethod		FLbutBank	11,     1,     2,     18,    2*15,    70, 45+2,  -1
gkSyncNdx, gihSyncNdx		FLbutBank	11,     1,     3,     18,    3*15,   270, 45+2,  -1
gkLFO_Shape, gihLFO_Shape	FLbutBank	11,     1,     2,     18,    2*15,   400, 45+2,  -1
FLhide	gihSyncNdx
FLhide	gihLFO_Shape
;GENERAL_TEXT_SETTINGS			SIZE | FONT |  ALIGN | RED | GREEN | BLUE
			FLlabel		13,      1,      3,     0,     0,     0			;LABELS MADE VISIBLE AGAIN

;TEXT BOXES							TYPE | FONT | SIZE | WIDTH | HEIGHT | X |  Y
ih		 	FLbox  	"Method:", 			1,       6,    12,     60,      15,   5,  45+2
ih		 	FLbox  	"Multi-delay",			1,       5,    12,     80,      15,  90,  45+2
ih		 	FLbox  	"LFO        ",			1,       5,    12,     80,      15,  90,  60+2
gih1		 	FLbox  	"LFO Sync:", 			1,       6,    12,     70,      15, 200,  45+2
gih2		 	FLbox  	"1/2",				1,       5,    12,     30,      15, 290,  45+2
gih3		 	FLbox  	"1/4",				1,       5,    12,     30,      15, 290,  60+2
gih4		 	FLbox  	"1/8",				1,       5,    12,     30,      15, 290,  75+2
gih5		 	FLbox  	"LFO Shape:", 			1,       6,    12,     70,      15, 330,  45+2
gih6		 	FLbox  	"Sine  ",			1,       5,    12,     60,      15, 420,  45+2
gih7		 	FLbox  	"Square",			1,       5,    12,     60,      15, 420,  60+2
FLhide	gih1
FLhide	gih2
FLhide	gih3
FLhide	gih4
FLhide	gih5
FLhide	gih6
FLhide	gih7

;VALUE_DISPLAY_BOXES			 	WIDTH | HEIGHT | X | Y
iddlt				FLvalue	" ",      70,    20,     5, 125
idmix				FLvalue	" ",      70,    20,     5, 175
idfeedbackamt			FLvalue	" ",      70,    20,     5, 225
idamp				FLvalue	" ",      70,    20,     5, 275
idInGain			FLvalue	" ",      50,    20,   210,  25


;SLIDERS					            			MIN |  MAX     | EXP | TYPE |      DISP    | WIDTH | HEIGHT | X | Y
gkdlt,ihdlt			FLslider 	"Delay Time (sec.)",		.001,gimaxdelay, -1,    23,           iddlt,   490,     25,    5, 100
gkmix,ihmix			FLslider 	"Dry/Wet Mix",			0,      1,        0,    23,           idmix,   490,     25,    5, 150
gkfeedamt,ihfeedamt		FLslider 	"Feedback Amount",		-1,     1,        0,    23,   idfeedbackamt,   490,     25,    5, 200
gkamp,ihamp			FLslider 	"Output Amplitude Rescaling",	0,      1,        0,    23,           idamp,   490,     25,    5, 250
gkInGain,ihInGain		FLslider 	"Live Input Gain",		0,      1,        0,    23,        idInGain,   285,     20,  210,   5

;SET_INITIAL_VALUES		VALUE | HANDLE
		FLsetVal_i	.33, 	ihdlt
		FLsetVal_i	0.5, 	ihmix
		FLsetVal_i	0.66, 	ihfeedamt
		FLsetVal_i	.7, 	ihamp
		FLsetVal_i	0, 	ihInGain
		FLsetVal_i	0, 	gihSyncNdx
		FLsetVal_i	1, 	gihLFO_Shape

		FLpanel_end	;END OF PANEL CONTENTS

;INSTRUCTIONS AND INFO PANEL
				FLpanel	" ", 515, 540, 512, 0
				FLscroll     515, 540, 0, 0
;TEXT BOXES												TYPE | FONT | SIZE | WIDTH | HEIGHT | X | Y
ih		 	FLbox  	"                    Stereo Ping-Pong Delay                   ", 	1,      6,     14,    490,    30,     5,   0
ih		 	FLbox  	"-------------------------------------------------------------", 	1,      5,     14,    490,    20,     5,  20
ih		 	FLbox  	"This example offers two different implementations of a stereo", 	1,      5,     14,    490,    20,     5,  40
ih		 	FLbox  	"ping-pong delay.                                             ", 	1,      5,     14,    490,    20,     5,  60
ih		 	FLbox  	"                                                             ", 	1,      5,     14,    490,    20,     5,  80
ih		 	FLbox  	"Method  1                                                    ", 	1,      8,     14,    490,    20,     5, 100
ih		 	FLbox  	"This method echoes input signals alternately in the left and ", 	1,      5,     14,    490,    20,     5, 120 
ih		 	FLbox  	"right channels. It does this using a total of three delay    ", 	1,      5,     14,    490,    20,     5, 140 
ih		 	FLbox  	"buffers: one for the left channel with a feedback loop, one  ", 	1,      5,     14,    490,    20,     5, 160 
ih		 	FLbox  	"for the right channel with a feedback loop and a third one to", 	1,      5,     14,    490,    20,     5, 180 
ih		 	FLbox  	"create an initial delay offset in the left or right channel. ", 	1,      5,     14,    490,    20,     5, 200 
ih		 	FLbox  	"The first echo we will hear will be in the channel with the  ", 	1,      5,     14,    490,    20,     5, 220
ih		 	FLbox  	"offset. The offsetting delay buffer has no feedback loop but ", 	1,      5,     14,    490,    20,     5, 240
ih		 	FLbox  	"its output is sent separately to the speakers.               ", 	1,      5,     14,    490,    20,     5, 260
ih		 	FLbox  	"The schematic is shown below:                                ", 	1,      5,     14,    490,    20,     5, 280
ih		 	FLbox  	"                           (feedback)                        ", 	1,      5,     14,    490,    20,     5, 300
ih		 	FLbox  	"                        +-------<------+                     ", 	1,      5,     14,    490,    20,     5, 320
ih		 	FLbox  	"                        |              |                     ", 	1,      5,     14,    490,    20,     5, 340
ih		 	FLbox  	"           +--------+   v  +--------+  ^                     ", 	1,      5,     14,    490,    20,     5, 360
ih		 	FLbox  	"           |OFFSET  |   |  |        |  |                     ", 	1,      5,     14,    490,    20,     5, 380
ih		 	FLbox  	"      +----+DELAY   +-+-+--+DELAY L +--+--+->LEFT_OUT        ", 	1,      5,     14,    490,    20,     5, 400
ih		 	FLbox  	"      |    |time x 1| |    |time x 2|     |                  ", 	1,      5,     14,    490,    20,     5, 420
ih		 	FLbox  	"      |    +--------+ v    +--------+     ^                  ", 	1,      5,     14,    490,    20,     5, 440
ih		 	FLbox  	"      |               |                   |                  ", 	1,      5,     14,    490,    20,     5, 460
ih		 	FLbox  	" IN->-+               +--------->---------+                   ", 	1,      5,     14,    490,    20,     5, 480
ih		 	FLbox  	"      |                                                      ", 	1,      5,     14,    490,    20,     5, 500
ih		 	FLbox  	"      |                    +--------+                        ", 	1,      5,     14,    490,    20,     5, 520
ih		 	FLbox  	"      |                    |        |                        ", 	1,      5,     14,    490,    20,     5, 540
ih		 	FLbox  	"      +-----------------+--+DELAY R +--+---->RIGHT_OUT       ", 	1,      5,     14,    490,    20,     5, 560
ih		 	FLbox  	"                        |  |time x 2|  |                     ", 	1,      5,     14,    490,    20,     5, 580
ih		 	FLbox  	"                        |  +--------+  |                     ", 	1,      5,     14,    490,    20,     5, 600
ih		 	FLbox  	"                        |              |                     ", 	1,      5,     14,    490,    20,     5, 620
ih		 	FLbox  	"                        +-------<------+                     ", 	1,      5,     14,    490,    20,     5, 640
ih		 	FLbox  	"                           (feedback)                        ", 	1,      5,     14,    490,    20,     5, 660
ih		 	FLbox  	"                                                             ", 	1,      5,     14,    490,    20,     5, 680
ih		 	FLbox  	"The delay time for the offsetting delay is the same as that  ", 	1,      5,     14,    490,    20,     5, 700
ih		 	FLbox  	"defined by the on-screen slider for delay time. The delay    ", 	1,      5,     14,    490,    20,     5, 720
ih		 	FLbox  	"times for the other two delays with feedback are twice the   ", 	1,      5,     14,    490,    20,     5, 740
ih		 	FLbox  	"value defined by the slider. When the echoes alternate       ", 	1,      5,     14,    490,    20,     5, 760
ih		 	FLbox  	"the perceived delay time will be the same as that defined by ", 	1,      5,     14,    490,    20,     5, 780 
ih		 	FLbox  	"the slider.                                                  ", 	1,      5,     14,    490,    20,     5, 800
ih		 	FLbox  	"The input sound for the effect in this example can be either ", 	1,      5,     14,    490,    20,     5, 820
ih		 	FLbox  	"a short synthesised impulse triggered by the button or the   ", 	1,      5,     14,    490,    20,     5, 840
ih		 	FLbox  	"computer's live input controlled by the 'Input Gain' slider. ", 	1,      5,     14,    490,    20,     5, 860
ih		 	FLbox  	"                                                             ", 	1,      5,     14,    490,    20,     5, 880
ih		 	FLbox  	"Method 2                                                     ", 	1,      8,     14,    490,    20,     5, 900
ih		 	FLbox  	"This method pans a single delay with a panning rate that     ", 	1,      5,     14,    490,    20,     5, 920
ih		 	FLbox  	"synchronises to the delay time using some simple ratio. For  ", 	1,      5,     14,    490,    20,     5, 940
ih		 	FLbox  	"example if a sync. ratio of 1/2 is chosen then the period of ", 	1,      5,     14,    490,    20,     5, 960
ih		 	FLbox  	"the LFO is twice that of the delay time - for each echo the  ", 	1,      5,     14,    490,    20,     5, 980
ih		 	FLbox  	"LFO will have moved on 180 degrees. If the ratio is 1/4 then ", 	1,      5,     14,    490,    20,     5,1000
ih		 	FLbox  	"the LFO moves on 90 degrees for each echo. The LFO shape can ", 	1,      5,     14,    490,    20,     5,1020
ih		 	FLbox  	"be either a sine wave or a square wave. If a square wave is  ", 	1,      5,     14,    490,    20,     5,1040
ih		 	FLbox  	"chosen then the delay will only be panned extreme left or    ", 	1,      5,     14,    490,    20,     5,1060
ih		 	FLbox  	"extreme right. If sine is chosen then ping-ponging moves     ", 	1,      5,     14,    490,    20,     5,1080
ih		 	FLbox  	"smoothly between the left and right.                         ", 	1,      5,     14,    490,    20,     5,1100
ih		 	FLbox  	"The Schematic is as shown below:                             ", 	1,      5,     14,    490,    20,     5,1120
ih		 	FLbox  	"                                                             ", 	1,      5,     14,    490,    20,     5,1140
ih		 	FLbox  	"                 +--------+   +--------+                     ", 	1,      5,     14,    490,    20,     5,1160
ih		 	FLbox  	"                 | DELAY  |   | SYNCED +---->LEFT_OUT        ", 	1,      5,     14,    490,    20,     5,1180
ih		 	FLbox  	"   MONO_IN->-----+        +---+  LFO   |                     ", 	1,      5,     14,    490,    20,     5,1200
ih		 	FLbox  	"                 |time x 1|   |PANNING +---->RIGHT_OUT       ", 	1,      5,     14,    490,    20,     5,1220
ih		 	FLbox  	"                 +--------+   +--------+                     ", 	1,      5,     14,    490,    20,     5,1240
ih		 	FLbox  	"                                                             ", 	1,      5,     14,    490,    20,     5,1260
                                                                                                                                                 
				FLscrollEnd                                                                                                                                                 
				FLpanel_end                                                                                                      
                                                                                                                                                 
				FLrun	;RUN THE FLTK WIDGET THREAD                                                                              
;END OF FLTK INTERFACE CODE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                     
                                                                                                                                                 
instr	1	;GENERATES A SHORT SYNTHESISED IMPULSE SOUND                                                                                                     
	aenv		expseg	0.0001, 0.01, 1, p3-0.01, 0.0001	;PERCUSSIVE-TYPE AMPLITUDE ENVELOPE
	asig1		noise	0.3 * aenv, 0				;GENERATE A 'WHITE NOISE' SIGNAL
	iCutoff_Oct	random	6,14					;CREATE A RANDOM VALUE FOR CUTOFF FREQUENCY (IN OCT FORMAT)
	asig2		butlp	asig1, cpsoct(iCutoff_Oct)		;LOWPASS FILTER WHITE NOISE
	gasig		balance	asig2, asig1				;BALANCE FILTERED SIGNAL WITH UNFILTERED WHITE NOISE SIGNAL TO COMPENSATE FOR AMPLITUDE LOSS
endin

instr 	2	;STEREO PING-PONG DELAY INSTRUMENT - ALSO READS LIVE INPUT SIGNAL
	gasig		init	0		;SET INITIAL STATE OF GLOBAL AUDIO SIGNAL (SILENCE)
	if gkmethod!=0 kgoto SKIP
	aInL, aInR	ins			;READ LIVE AUDIO INPUT
	aInL		=	(aInL * gkInGain) + gasig	;MIX LIVE AUDIO IN WITH GLOBAL AUDIO SIGNAL RECEIVED FROM INSTR 1
	aInR		=	(aInR * gkInGain) + gasig		;MIX LIVE AUDIO IN WITH GLOBAL AUDIO SIGNAL RECEIVED FROM INSTR 1
	
	iporttime	=		.1			;PORTAMENTO TIME
	kporttime	linseg		0, .001, iporttime	;USE OF AN ENVELOPE VALUE THAT QUICKLY RAMPS UP FROM ZERO TO THE REQUIRED VALUE. THIS PREVENTS VARIABLES GLIDING TO THEIR REQUIRED VALUES EACH TIME THE INSTRUMENT IS STARTED
	kdlt		portk		gkdlt, kporttime 	;PORTAMENTO IS APPLIED TO THE VARIABLE 'gkdlt'. A NEW VARIABLE 'kdlt' IS CREATED.
	adlt		interp		kdlt			;A NEW A-RATE VARIABLE 'adlt' IS CREATED BY INTERPOLATING THE K-RATE VARIABLE 'kdlt' 
	
	;;;LEFT CHANNEL OFFSET;;;NO FEEDBACK!!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	abufferL_OS	delayr	gimaxdelay			;CREATE A DELAY BUFFER OF imaxdelay SECONDS DURATION
	adelsigL_OS 	deltap3	adlt				;TAP THE DELAY LINE AT adlt SECONDS
			delayw	aInL				;WRITE AUDIO SOURCE INTO THE BEGINNING OF THE BUFFER
	
	;;;LEFT CHANNEL DELAY;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	abufferL	delayr	gimaxdelay*2			;CREATE A DELAY BUFFER OF 5 SECONDS DURATION (EQUIVALENT TO THE MAXIMUM DELAY TIME POSSIBLE USING THIS EXAMPLE)
	adelsigL 	deltap3	adlt*2				;TAP THE DELAY LINE AT gkdlt SECONDS
			delayw	adelsigL_OS + (adelsigL * gkfeedamt)	;WRITE AUDIO SOURCE FROM OFFSETTTING DELAY AND FEEDBACK SIGNAL INTO THE BEGINNING OF THE BUFFER
	
	abufferR	delayr	gimaxdelay*2			;CREATE A DELAY BUFFER OF 5 SECONDS DURATION (EQUIVALENT TO THE MAXIMUM DELAY TIME POSSIBLE USING THIS EXAMPLE)
	adelsigR 	deltap3	adlt*2				;TAP THE DELAY LINE AT gkdlt SECONDS
			delayw	aInR+(adelsigR*gkfeedamt)	;WRITE AUDIO SOURCE AND FEEDBACK SIGNAL INTO THE BEGINNING OF THE BUFFER

	;CREATE LEFT AND RIGHT CHANNEL MIXES
	aOutL		sum		(((adelsigL  + adelsigL_OS)* gkmix) + (aInL * (1-gkmix))) * gkamp
	aOutR		sum		((adelsigR)                * gkmix) + (aInR * (1-gkmix))  * gkamp	
			outs		aOutL, aOutR 		;CREATE A MIX BETWEEN THE WET AND THE DRY SIGNALS AT THE OUTPUT
	gasig		=	0				;CLEAR THE GLOBAL AUDIO SEND VARIABLES
	SKIP:
endin

instr	3	;LFO DELAY
	if gkmethod!=1 kgoto SKIP
	aLiveIn		inch	1				;READ LIVE AUDIO INPUT FROM LEFT CHANNEL
	aIn		=	(aLiveIn * gkInGain) + gasig	;MIX LIVE AUDIO IN WITH GLOBAL AUDIO SIGNAL RECEIVED FROM INSTR 1
	iporttime	=		.1			;PORTAMENTO TIME
	kporttime	linseg		0, .001, iporttime	;USE OF AN ENVELOPE VALUE THAT QUICKLY RAMPS UP FROM ZERO TO THE REQUIRED VALUE. THIS PREVENTS VARIABLES GLIDING TO THEIR REQUIRED VALUES EACH TIME THE INSTRUMENT IS STARTED
	kdlt		portk		gkdlt, kporttime 	;PORTAMENTO IS APPLIED TO THE VARIABLE 'gkdlt'. A NEW VARIABLE 'kdlt' IS CREATED.
	adlt		interp		kdlt			;A NEW A-RATE VARIABLE 'adlt' IS CREATED BY INTERPOLATING THE K-RATE VARIABLE 'kdlt' 
	abuffer		delayr	gimaxdelay			;CREATE A DELAY BUFFER OF 5 SECONDS DURATION (EQUIVALENT TO THE MAXIMUM DELAY TIME POSSIBLE USING THIS EXAMPLE)
	adelsig 	deltap3	adlt				;TAP THE DELAY LINE AT gkdlt SECONDS
			delayw	aIn + (adelsig * gkfeedamt)	;WRITE AUDIO SOURCE AND FEEDBACK SIGNAL INTO THE BEGINNING OF THE BUFFER
	iSyncVals	ftgenonce	0,0,-3,-2,0.5,0.25,0.125;TABLE THAT STORE SYNC VALUES
	ktrig		changed	gkSyncNdx,gkLFO_Shape		;IF 'SYNC VALUE' OR 'LFO SHAPE' CONTROLS ARE CHANGED...
	if ktrig==1 then
	 reinit UPDATE_LFO					;UPDATE THE LFO SET-UP
	endif
	UPDATE_LFO:						
	iSyncVal	table	i(gkSyncNdx),iSyncVals		;READ SYNC VALUE USING SYNC INDEX FROM BUTTON BANK VALUE
	kpan		lfo	0.5,iSyncVal/kdlt,i(gkLFO_Shape)*2;LFO SPEED IS RELATED TO DELAY TIME: IT IS 'SYNC VALUE' TIMES THE RECIPROCAL OF DELAY TIME
	if i(gkLFO_Shape)==1 then	;IF SQUAREWAVE LFO...
	 kpan	port	kpan, 0.002	;SMOOTH EDGES
	endif
	rireturn
	aL,aR		pan2	adelsig,kpan+0.5		;PAN THE DELAYED SIGNAL
			outs	aL * gkamp, aR * gkamp
	SKIP:
	gasig		=	0				;CLEAR THE GLOBAL AUDIO SEND VARIABLES
endin

instr	4
	ktrig	changed	gkmethod
	if ktrig==1 then
	 reinit	UPDATE
	endif
	UPDATE:
	if i(gkmethod)==0 then
		FLhide	gihSyncNdx
		FLhide	gihLFO_Shape
		FLhide	gih1
		FLhide	gih2
		FLhide	gih3
		FLhide	gih4
		FLhide	gih5
		FLhide	gih6
		FLhide	gih7
	else
		FLshow	gihSyncNdx
		FLshow	gihLFO_Shape
		FLshow	gih1
		FLshow	gih2
		FLshow	gih3
		FLshow	gih4
		FLshow	gih5
		FLshow	gih6
		FLshow	gih7
	endif
endin

</CsInstruments>

<CsScore>
;INSTR | START | DURATION
i  2       0       3600	;SEPARATE DELAYS ON LEFT AND RIGHT CHANNELS PING-PONG DELAY INSTRUMENT
i  3       0       3600	;LFO PING-PONGDELAY INSTRUMENT
i  4       0       3600	;MODE
</CsScore>

</CsoundSynthesizer>