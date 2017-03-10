MonoPolySynth.csd
Written by Iain McCurdy 2006. Updated 2011.

<CsoundSynthesizer>

<CsOptions>
-odac -Ma -dm0
;-odac -M0 -+rtmidi=virtual -dm0 -b200
</CsOptions>

<CsInstruments>

sr 		= 	44100
ksmps 		= 	8
nchnls 		= 	2
0dbfs		=	1	;MAXIMUM AMPLITUDE REGARDLESS OF BIT DEPTH                                
maxalloc	2,1	;ENSURE THAT ONLY 1 INSTANCE OF INSTR 2 CAN EXIST AT A TIME

;FLTK INTERFACE CODE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FLcolor	255, 255, 255, 0, 0, 0
		FLpanel	"MonoPoly Synth", 1000, 740, 0, 0                   

;BORDERS				TYPE | FONT | SIZE | WIDTH | HEIGHT | X | Y
ih		 	FLbox  	" ", 	6,       9,    15,    475,    110,    5, 240	;PITCH ENVELOPE
ih		 	FLbox  	" ", 	6,       9,    15,    475,    110,    5, 360	;FILTER ENVELOPE
ih		 	FLbox  	" ", 	6,       9,    15,    140,    110,    5, 480	;FILTER
ih		 	FLbox  	" ", 	6,       9,    15,    325,    110,  155, 480	;AMPLITUDE ENVELOPE
ih		 	FLbox  	" ", 	6,       9,    15,    200,    110,    5, 600	;DELAY
ih		 	FLbox  	" ", 	6,       9,    15,    115,    110,  215, 600	;REVERB
ih		 	FLbox  	" ", 	6,       9,    15,    140,    110,  340, 600	;CHORUS
ih		 	FLbox  	" ", 	6,       9,    15,    126,    110,  112, 100	;VCO PARAMETERS
ih		 	FLbox  	" ", 	6,       9,    15,    190,    110,  260, 100	;BUZZ PARAMETERS

;TEXT BOXES						TYPE | FONT | SIZE | WIDTH | HEIGHT | X |  Y
ih		 	FLbox  	"Pitch Envelope",	1,      12,    16,    120,     25,   10,  242
ih		 	FLbox  	"Filter Envelope",	1,      12,    16,    120,     25,   10,  362
ih		 	FLbox  	"Filter",		1,      12,    16,     60,     25,   10,  482
ih		 	FLbox  	"Amplitude Envelope",	1,      12,    16,    140,     25,  165,  482
ih		 	FLbox  	"Delay",		1,      12,    16,     50,     25,   10,  602
ih		 	FLbox  	"Reverb",		1,      12,    16,     50,     25,  230,  602
ih		 	FLbox  	"Chorus",		1,      12,    16,     40,     25,  352,  602
ih		 	FLbox  	"VCO Parameters",	1,      12,    14,    120,     22,  114,  102
ih		 	FLbox  	"Buzz Parameters",	1,      12,    14,    120,     22,  262,  102

;					WIDTH | HEIGHT | X  | Y
idgliss			FLvalue	" ",	 60,      20,     5,  60
idamp			FLvalue	" ",	 60,      20,    65,  60
idtrmdep		FLvalue	" ",	 60,      20,   125,  60
idvibdep		FLvalue	" ",	 60,      20,   185,  60
idmodfreq		FLvalue	" ",	 60,      20,   245,  60
idpw			FLvalue	" ",	 60,      20,   145, 180   
idmul			FLvalue	" ",	 60,      20,   265, 180
;PITCH
idPStrLev		FLvalue	" ",	 60,      20,    30, 320
idPAttTim		FLvalue	" ",	 60,      20,    90, 320
idPAttLev		FLvalue	" ",	 60,      20,   150, 320
idPDecTim		FLvalue	" ",	 60,      20,   210, 320
idPRelTim		FLvalue	" ",	 60,      20,   270, 320
idPRelLev		FLvalue	" ",	 60,      20,   330, 320
;FILTER
idFAttTim		FLvalue	" ",	 60,      20,    30, 440
idFAttLev		FLvalue	" ",	 60,      20,    90, 440        
idFDecTim		FLvalue	" ",	 60,      20,   150, 440
idFSusLev		FLvalue	" ",	 60,      20,   210, 440
idFRelTim		FLvalue	" ",	 60,      20,   270, 440
idFRelLev		FLvalue	" ",	 60,      20,   330, 440
idFEnvAmt		FLvalue	" ",	 60,      20,   390, 440

idBaseFrq		FLvalue	" ",	 60,      20,    15, 560
idRes			FLvalue	" ",	 60,      20,    75, 560
;AMPLITUDE
idAAttTim		FLvalue	" ",	 60,      20,    30+135, 560
idAAttLev		FLvalue	" ",	 60,      20,    90+135, 560
idADecTim		FLvalue	" ",	 60,      20,   150+135, 560
idASusLev		FLvalue	" ",	 60,      20,   210+135, 560
idARelTim		FLvalue	" ",	 60,      20,   270+135, 560

idDlyAmt		FLvalue	" ",	 60,      20,    15, 680
idDlyTim		FLvalue	" ",	 60,      20,    75, 680
idDlyFB			FLvalue	" ",	 60,      20,   135, 680

;KNOBS								MIN    |   MAX | EXP|  TYPE |  DISP    | WIDTH | X | Y
gkgliss, ihgliss	FLknob		"Port.Tim.",		0,          1,    0,    1,   idgliss,      40,  15,   5             
gkamp, gihamp		FLknob		"Amp.",			0,          1,    0,    1,   idamp,        40,  75,   5
gktrmdep, ihtrmdep	FLknob		"Trem.Amt.",		0,          1,    0,    1,   idtrmdep,     40, 135,   5
gkvibdep, ihvibdep	FLknob		"Vib.Amt.",		0,          1,    0,    1,   idvibdep,     40, 195,   5
gkmodfreq, gihmodfreq	FLknob		"Mod.Frq.",		0,         20,    0,    1,   idmodfreq,    40, 255,   5
gkpw, gihpw		FLknob		"P.W.CC#5",   		0,          1,    0,    1,   idpw,         40, 155, 125
gkmul, ihmul		FLknob		"Mul.",			0,          2,    0,    1,   idmul,        40, 275, 125
;PITCH
gkPStrLev, ihPStrLev	FLknob 		"Str.Lev.", 		0.125,      2,    0,    1,   idPStrLev,    40,  40, 265
gkPAttTim, ihPAttTim	FLknob 		"Att.Tim.", 		0.001,      1,    0,    1,   idPAttTim,    40, 100, 265
gkPAttLev, ihPAttLev	FLknob 		"Att.Lev.", 		0.125,      2,    0,    1,   idPAttLev,    40, 160, 265
gkPDecTim, ihPDecTim	FLknob 		"Dec.Tim.", 		0.001,      1,    0,    1,   idPDecTim,    40, 220, 265
gkPRelTim, ihPRelTim	FLknob 		"Rel.Tim.", 		0.001,      5,    0,    1,   idPRelTim,    40, 280, 265 
gkPRelLev, ihPRelLev	FLknob 		"Rel.Lev.", 		0.125,      2,    0,    1,   idPRelLev,    40, 340, 265
;FILTER
gkFAttTim, ihFAttTim	FLknob 		"Att.Tim.", 		0.001,      8,    0,    1,   idFAttTim,    40,  40, 385
gkFAttLev, ihFAttLev	FLknob 		"Att.Lev.", 		0,          1,    0,    1,   idFAttLev,    40, 100, 385
gkFDecTim, ihFDecTim	FLknob 		"Dec.Tim.", 		0.001,      8,    0,    1,   idFDecTim,    40, 160, 385
gkFSusLev, ihFSusLev	FLknob 		"Sus.Lev.", 		0,          1,    0,    1,   idFSusLev,    40, 220, 385
gkFRelTim, ihFRelTim	FLknob 		"Rel.Tim.", 		0.001,      8,    0,    1,   idFRelTim,    40, 280, 385
gkFRelLev, ihFRelLev	FLknob 		"Rel.Lev.", 		0,          1,    0,    1,   idFRelLev,    40, 340, 385
gkFEnvAmt, ihFEnvAmt	FLknob 		"Env.Amt.", 		0,         10,    0,    1,   idFEnvAmt,    40, 400, 385

gkBaseFrq, gihBaseFrq	FLknob 		"Base.Frq.", 		4,         14,    0,    1,   idBaseFrq,    40,  25, 505
gkRes, gihRes		FLknob 		"Res.", 		0,          1,    0,    1,   idRes,        40,  85, 505
;AMPLITUDE
gkAAttTim, ihAAttTim	FLknob 		"Att.Tim.", 		0.001,      5,    0,    1,   idAAttTim,    40,  40+135, 505
gkAAttLev, ihAAttLev	FLknob 		"Att.Lev.", 		0,          1,    0,    1,   idAAttLev,    40, 100+135, 505
gkADecTim, ihADecTim	FLknob 		"Dec.Tim.", 		0.001,      5,    0,    1,   idADecTim,    40, 160+135, 505
gkASusLev, ihASusLev	FLknob 		"Sus.Lev.", 		0,          1,    0,    1,   idASusLev,    40, 220+135, 505
gkARelTim, ihARelTim	FLknob 		"Rel.Tim.", 		0.001,      5,    0,    1,   idARelTim,    40, 280+135, 505
;DELAY
gkDlyAmt, ihDlyAmt	FLknob 		"Level", 		0,          1,    0,    1,   idDlyAmt,     40,  25, 625
gkDlyTim, gihDlyTim	FLknob 		"Time", 		0.01,       2,    0,    1,   idDlyTim,     40,  85, 625
gkDlyFB, ihDlyFB	FLknob 		"Feedback", 		0,          1,    0,    1,   idDlyFB,      40, 145, 625
;REVERB
gkRvbAmt, ihRvbAmt	FLknob 		"Level", 		0,          1,    0,    1,   -1,           40, 225, 630
gkRvbTim, ihRvbTim	FLknob 		"Time", 		0.4,     0.99,    0,    1,   -1,           40, 275, 630
;CHORUS
gkChoAmt, ihChoAmt	FLknob 		"Level", 		0,          1,    0,    1,   -1,           39, 348, 630
gkChoDep, ihChoDep	FLknob 		"Depth", 		0,          1,    0,    1,   -1,           39, 390, 630
gkChoRte, ihChoRte	FLknob 		"Rate", 		0.001,     10,   -1,    1,   -1,           39, 432, 630

;FLCOUNTERS										MIN | MAX | STEP1 | STEP2 | TYPE | WIDTH | HEIGHT | X | Y | OPCODE
gkBendRnge, ihBendRnge 	FLcount  "Bend Range",	       	        	        	0,     24,    1,      1,      2,     60,     25,  330,  20,   -1
gkwave,ihwave 		FLcount  "Waveform: 1-Saw 2-PW/Sq. 3-Tri/Saw/Ramp 4-Buzz", 	1,      4,    1,      1,      2,    100,     25,    5, 105,   -1
gkharm, ihharm 		FLcount  "Number of Harms.",        	               		1,    100,    1,      1,      2,     60,     25,  325, 125,   -1
gklh, ihlh 		FLcount  "Lowest Harm.",		      	        	1,    100,    1,      1,      2,     60,     25,  385, 125,   -1

;GENERAL_TEXT_SETTINGS			SIZE | FONT |  ALIGN | RED | GREEN | BLUE
			FLlabel		13,      4,      1,    255,   255,   255
FLcolor	255,255,255
;BUTTON BANKS				TYPE | NUMX | NUMY | WIDTH | HEIGHT | X   | Y  | OPCODE | INS | STARTTIM | DUR
gkMonoPoly, ihMonoPoly	FLbutBank	11,     1,     2,     18,      30,   400,  20,    -1	
;GENERAL_TEXT_SETTINGS			SIZE | FONT |  ALIGN | RED | GREEN | BLUE
			FLlabel		13,      1,      3,     0,     0,     0
;TEXT BOXES					TYPE | FONT | SIZE | WIDTH | HEIGHT | X |  Y
ih		 	FLbox  	"monophonic",	1,       1,    12,    80,     15,  420,   20
ih		 	FLbox  	"polyphonic",	1,       1,    12,    70,     15,  420,   35

;INITIALISE VALUATORS		VALUE | HANDLE
		FLsetVal_i	.005, 	ihgliss
		FLsetVal_i	2, 	ihBendRnge
		FLsetVal_i	0.5, 	gihpw
		FLsetVal_i	2, 	ihwave
		;FLsetVal_i	0, 	ihleak
		;FLsetVal_i	.5, 	ihnyx
		FLsetVal_i	1, 	ihmul
		FLsetVal_i	30, 	ihharm
		FLsetVal_i	0, 	ihlh
		FLsetVal_i	0.2, 	gihamp
		FLsetVal_i	0, 	ihtrmdep
		FLsetVal_i	.1, 	ihvibdep
		FLsetVal_i	5, 	gihmodfreq

		FLsetVal_i	1, 	ihPStrLev
		FLsetVal_i	.00001,	ihPAttTim
		FLsetVal_i	1, 	ihPAttLev
		FLsetVal_i	.00001,	ihPDecTim
		FLsetVal_i	.00001,	ihPRelTim
		FLsetVal_i	1, 	ihPRelLev

		FLsetVal_i	0.001,  ihFAttTim
		FLsetVal_i	1, 	ihFAttLev
		FLsetVal_i	0.8,    ihFDecTim
		FLsetVal_i	0.616, 	ihFSusLev
		FLsetVal_i	0.1, 	ihFRelTim
		FLsetVal_i	0, 	ihFRelLev
		FLsetVal_i	8, 	ihFEnvAmt
		
		FLsetVal_i	0.001,  ihAAttTim
		FLsetVal_i	1, 	ihAAttLev
		FLsetVal_i	0.001,  ihADecTim
		FLsetVal_i	1, 	ihASusLev
		FLsetVal_i	0.001,  ihARelTim
		FLsetVal_i	0.7, 	gihRes
		FLsetVal_i	2, 	gihBaseFrq
		FLsetVal_i	0.2, 	ihDlyAmt
		FLsetVal_i	0.9, 	gihDlyTim
		FLsetVal_i	0.2, 	ihDlyFB
		FLsetVal_i	0.2, 	ihRvbAmt
		FLsetVal_i	0.8, 	ihRvbTim
		FLsetVal_i	0.5, 	ihChoAmt
		FLsetVal_i	0.4, 	ihChoDep
		FLsetVal_i	1, 	ihChoRte

;TEXT BOXES												TYPE | FONT | SIZE | WIDTH | HEIGHT | X | Y
ih		 	FLbox  	"                    Mono/Poly MIDI synth.                    ", 	1,      5,     14,    490,    20,     505,  20
ih		 	FLbox  	"-------------------------------------------------------------", 	1,      5,     14,    490,    20,     505,  40
ih		 	FLbox  	"NOTE: THIS IS A COMBINATION OF TWO SEPARATE OLDER EXAMPLES.  ", 	1,      5,     14,    490,    20,     505,  60
ih		 	FLbox  	"NOTABLY THE MONOPHONIC SYNTH PART HAS BEEN CONSIDERABLY      ", 	1,      5,     14,    490,    20,     505,  80
ih		 	FLbox  	"REVISED. THE METHOD USED IN THE OLD VERSION WOULD RESULT IN  ", 	1,      5,     14,    490,    20,     505, 100
ih		 	FLbox  	"NON-SOUNDING NOTES IF A NEW PHRASE WAS BEGUN OVER THE RELEASE", 	1,      5,     14,    490,    20,     505, 120
ih		 	FLbox  	"STAGE OF A PREVIOUS PHRASE.                                  ", 	1,      5,     14,    490,    20,     505, 140
ih		 	FLbox  	"OTHER IMPROVEMENTS HAVE ALSO BEEN MADE                       ", 	1,      5,     14,    490,    20,     505, 160
ih		 	FLbox  	"Building upon the principles of the previous example, 'Basic ", 	1,      5,     14,    490,    20,     505, 180
ih		 	FLbox  	"MonophonicSynth.csd', to create a MIDI controlled monophonic ", 	1,      5,     14,    490,    20,     505, 200
ih		 	FLbox  	"instrment, this example provides more user control of        ", 	1,      5,     14,    490,    20,     505, 220
ih		 	FLbox  	"amplitude, filter and pitch envelopes. A variety of waveforms", 	1,      5,     14,    490,    20,     505, 240
ih		 	FLbox  	"can be selected for use by the audio oscillator - three of   ", 	1,      5,     14,    490,    20,     505, 260
ih		 	FLbox  	"them derive from the VCO opcode and a fourth derives from the", 	1,      5,     14,    490,    20,     505, 280
ih		 	FLbox  	"gbuzz opcode. Low frequency modulation of the oscillator can ", 	1,      5,     14,    490,    20,     505, 300
ih		 	FLbox  	"be a user defined combination of tremolo (amplitude          ", 	1,      5,     14,    490,    20,     505, 320
ih		 	FLbox  	"modulation) and vibrato (pitch modulation). The amount of    ", 	1,      5,     14,    490,    20,     505, 340
ih		 	FLbox  	"modulation can be varied using MIDI controller 1 (the        ", 	1,      5,     14,    490,    20,     505, 360
ih		 	FLbox  	"modulation wheel on a MIDI keyboard.                         ", 	1,      5,     14,    490,    20,     505, 380
ih		 	FLbox  	"Additional MIDI controllers are:                             ", 	1,      5,     14,    490,    20,     505, 400
ih		 	FLbox  	"CC#2 - Low frequency modulation frequency                    ", 	1,      5,     14,    490,    20,     505, 420
ih		 	FLbox  	"CC#3 - Lowpass filter cutoff frequency                       ", 	1,      5,     14,    490,    20,     505, 440
ih		 	FLbox  	"CC#4 - Resonance of the filter                               ", 	1,      5,     14,    490,    20,     505, 460
ih		 	FLbox  	"CC#5 - Pulse Width                                           ", 	1,      5,     14,    490,    20,     505, 480
ih		 	FLbox  	"CC#6 - Delay Time                                            ", 	1,      5,     14,    490,    20,     505, 500
ih		 	FLbox  	"CC#7 - Amplitude                                             ", 	1,      5,     14,    490,    20,     505, 520
ih		 	FLbox  	"Additionally the signal is passed through a stereo ping-pong ", 	1,      5,     14,    490,    20,     505, 540
ih		 	FLbox  	"delay, Reverb and Chorus Effect.                             ", 	1,      5,     14,    490,    20,     505, 560
ih		 	FLbox  	"Pitch bend is implemented, the depth of which can be set by  ", 	1,      5,     14,    490,    20,     505, 580
ih		 	FLbox  	"the user in the interface.                                   ", 	1,      5,     14,    490,    20,     505, 600
                                                                                                                                                   
		FLpanel_end	;END OF PANEL CONTENTS
		FLrun		;RUN THE WIDGET THREAD

gkamp		init	0	;INITIALISE GLOBAL VARIABLES
gkcps		init	0       ;INITIALISE GLOBAL VARIABLES
gkNumInstr1	init	0       ;INITIALISE GLOBAL VARIABLES
gkvib		init	0       ;INITIALISE GLOBAL VARIABLES
gaSend		init	0	;INITIALISE GLOBAL VARIABLES
gaRvbSndL	init	0
gaRvbSndR	init	0
gkamp		init	0.2	;INITIALISE GLOBAL VARIABLES
gisine		ftgen	0,0,4096,10,1		;SINE WAVE (USED BY VCO AND VIBRATO AND TREMOLO FUNCTION)
gicos		ftgen	0,0,65536,9,1,1,90  	;COSINE WAVE (USED BY GBUZZ)
giChoShape	ftgen	0, 0, 131072, 19, 0.5, 1, 180, 1	;U-SHAPE PARABOLA

initc7	1,6,(0.9-0.01)/(2-0.01)
initc7	1,7,0.2

instr	99	;UPDATING DUAL MODE MIDI/FLTK VALUATORS
;CREATE A MACRO WITH VARIABLE TO PREVERT CODE REPETITION 
#define	UPDATE_WIDGET(NAME'CNUM'MIN'MAX)
	#
	k$NAME		ctrl7		1,$CNUM,0,1			;READ MIDI CONTROLLER
	k$NAME		scale		k$NAME,$MAX,$MIN		;RESCALE CONTROLLER VALUE
	ktrig$NAME	changed		k$NAME				;CREATE A TRIGGER PULSE IF MIDI CONTROLLER IS MOVED
			FLsetVal	ktrig$NAME, k$NAME, gih$NAME	;UPDATE FLTK VALUATOR IF MIDI CONTROLLER HAS BEEN MOVED
	#
	
	kmetro	metro	30	;SLOW THE RATE OF FLTK UPDATES - THIS IS PARTICULARLY NECESSARY ON THE MAC OS IN ORDER THAST FLTK UPDATES DO NOT INTERFER WITH REALTIME PERFORMANCE
	if kmetro=1 then	;IF METRO TICK IS RECEIVED...
	  ;EXPAND MACRO MULTIPLE TIMES TO SYNCHRONIZE EACH FLTK WIDGET TO ITS CORRESPONDING MIDI CONTROLLER (IF CHANGED)
	  $UPDATE_WIDGET(modfreq'2'0'20)
	  $UPDATE_WIDGET(BaseFrq'3'0'14)
	  $UPDATE_WIDGET(Res'4'0'1)
	  $UPDATE_WIDGET(pw'5'0'1)
	  $UPDATE_WIDGET(DlyTim'6'0.01'2)
	  $UPDATE_WIDGET(amp'7'0'1)
	endif
endin
	
instr	1	;RECEIVES MIDI NOTE INPUT
	icps		cpsmidi		;READ IN MIDI PITCH VALUES
	if i(gkMonoPoly)=0 then		;IF MONOPHONIC/POLYPHONIC SWITCH IS ON MONOPHONIC...
	  gkcps	=	icps		;...CREATE A K-RATE GLOBAL VARIABLE VERSION OF MIDI PITCH THAT WILL BE USED IN INSTRUMENT 2.
	  gkNoteOn	init	1	;A FLAG TO INDICATE WHEN A NEW NOTE HAS BEEN PLAYED. THIS FLAG IS CLEARED (RESET TO ZERO) AT THE BOTTOM OF INSTR 2.
	  event_i	"i",2,0,-1	;THE VERY FIRST MIDI NOTE PLAYED WILL TRIGGER AN INFINITELY HELD NOTE IN INSTR 2. THE maxalloc SETTING IN THE ORCHESTRA HEADER WILL PREVENT ANY FURTHER TRIGGERINGS. 
	else				;OTHERWISE
	  turnoff2	2,0,0		;TURN OFF INSTR 2 (THE MONOPHONIC INSTRUMENT)
	  aL,aR	subinstr	3,icps	;CALL INSTR 3 AS A SUB INSTRUMENT FOR EVERY NOTE PLAYED
	  	outs	aL,aR		;SEND THE OUTPUT OF THE SUB INSTRUMENT TO THE OUTPUTS
	endif				;END OF CONDITIONAL BRANCH
	;PITCH BEND INFORMATION IS READ
	kbend		pchbend	0, 2				;PITCH BEND VARIABLE (IN SEMITONES)
	gkbendmlt	=	semitone(kbend*gkBendRnge)	;CREATE A MULTIPLIER THAT WHEN MULTIPLIED TO THE FREQUENCY OF AN OSCILLATOR WILL IMPLEMENT PITCH BEND
endin
				
instr	2	;THE MONOPHONIC INSTRUMENT
	kactive1	active	1						;CONTINUALLY TRACK THE NUMBER OF ACTIVE INSTANCES OF INSTRUMENT 1. I.E. THE NUMBER OF MIDI NOTES HELD.

	;CREATE PORTAMENTO ON PITCH PARAMETER
	kporttime	linseg	0,0.01,1					;PORTAMENTO TIME RISES QUICKLY TO A HELD VALUE OF '1'
	kporttime	=	kporttime*gkgliss				;PORTAMENTO TIME FUNCTION SCALED BY VALUE OUTPUT BY FLTK SLIDER
	kcps		portk	gkcps, kporttime				;APPLY PORTAMENTO TO PITCH CHANGES
	
	kpw		portk	gkpw, kporttime*0.05				;PORTAMENTO IS APPLIED TO gkpw VARIABLE

	kmodwhl		ctrl7	1,1,0,1						;READ IN CONTROLLER 1 (MODULATION WHEEL)
	kmod		oscil	1,gkmodfreq,gisine				;CREATE GENERAL MODULATION FUNCTION
	gkvib		=	(kmod * kmodwhl * gkvibdep) + 1			;CREATE VIBRATO (PITCH MODULATION) FROM GENERAL MODULATION FUNCTION
	gktrm		=	1-(kmod*0.5*gktrmdep*kmodwhl)-(gktrmdep*0.5*kmodwhl)	;CREATE TREMOLO (AMPLITUDE MODULATION) FROM GENERAL MODULATION FUNCTION
	
	kAmpEnv		init	0						;INITIALISE AMPLITUDE ENVELOPE STATE
	kFEnv		init	0						;INITIALISE FILTER ENVELOPE STATE
	
	if gkNoteOn=1&&kactive1=1 then						;IF A NEW LEGATO PHRASE IS BEGUN (I.E. A NEW NOTE HAS BEEN PRESSED AND NO OTHER PREVIOUS NOTES ARE BEING HELD)...
	  reinit	RestartLegEnvs						;RESTART THE 'LEGATO PHRASE' ENVELOPES (IN THIS CASE AMPLITUDE AND FILTER)
	endif									;END OF THIS CONDITIONAL BRANCH
	RestartLegEnvs:								;A LABEL: BEGIN A REINITIALISATION PASS FROM HERE TO RESTART THE LEGATO PHRASE ENVELOPES
	
	if	kactive1>0 then							;IF A NEW LEGATO PHRASE IS BEGINNING...
	  kAmpEnv		linseg	i(kAmpEnv),i(gkAAttTim),i(gkAAttLev),i(gkADecTim),i(gkASusLev);MOVE THROUGH AMPLITUDE ATTACK (NOTE ON) ENVELOPE. IT WILL HOLD THE FINAL VALUE
	  kAmpSus		=	kAmpEnv					;REGISTER OF THE FINAL VALUE OF THE ATTACK-SUSTAIN ENVELOPE - USED BY THE RELEASE STAGE OF THE ENVELOPE
	  kPEnv			linseg	i(gkPStrLev),i(gkPAttTim),i(gkPAttLev),i(gkPDecTim),1;MOVE THROUGH PITCH ATTACK (NOTE ON) ENVELOPE. IT WILL HOLD THE FINAL VALUE
	  kPSus			=	kPEnv					;REGISTER OF THE FINAL VALUE OF THE ATTACK-SUSTAIN ENVELOPE - USED BY THE RELEASE STAGE OF THE ENVELOPE
	  kFEnv			transeg	i(kFEnv),i(gkFAttTim),-2,i(gkFAttLev),i(gkFDecTim),-2,i(gkFSusLev)	;MOVE THROUGH FILTER ATTACK (NOTE ON) ENVELOPE. IT WILL HOLD THE FINAL VALUE
	  kFSus			=	kFEnv					;REGISTER OF THE FINAL VALUE OF THE ATTACK-SUSTAIN ENVELOPE - USED BY THE RELEASE STAGE OF THE ENVELOPE
	elseif	kactive1=0 then							;OR IF A LEGATO PHRASE HAS FINISHED (NO NOTE ARE BEGIN HELD)...
	  kAmpEnv		linseg	1,i(gkARelTim),0  			;MOVE THROUGH AMPLITUDE RELEASE ENVELOPE (FINAL VALUE WILL BE HELD)
	  kAmpEnv		=	kAmpEnv*kAmpSus				;RELEASE ENVELOPE RESCALED ACCORDING TO THE FINAL VALUE OF THE ATTACK SUSTAIN PORTION OF THE ENVELOPE BEFORE THE NOTE OFF WAS RECEIVED
	  kPEnv			linseg	1,i(gkPRelTim),i(gkPRelLev)  		;MOVE THROUGH PITCH RELEASE ENVELOPE (FINAL VALUE WILL BE HELD)
	  kPEnv			=	kPEnv*kPSus				;RELEASE ENVELOPE RESCALED ACCORDING TO THE FINAL VALUE OF THE ATTACK SUSTAIN PORTION OF THE ENVELOPE BEFORE THE NOTE OFF WAS RECEIVED
	  kFEnv			transeg	1,i(gkFRelTim),-2,i(gkFRelLev)		;MOVE THROUGH FILTER RELEASE ENVELOPE (FINAL VALUE WILL BE HELD)
	  kFEnv			=	kFEnv*kFSus				;RELEASE ENVELOPE RESCALED ACCORDING TO THE FINAL VALUE OF THE ATTACK SUSTAIN PORTION OF THE ENVELOPE BEFORE THE NOTE OFF WAS RECEIVED
	endif									;END OF THIS CONDITIONAL BRANCH
	rireturn								;RETURN FROM REINITIALISATION PASS
	
	aAEnv		interp	kAmpEnv						;SMOOTHER A-RATE AMPLITUDE ENVELOPE - MAY PROVE BENEFICIAL IF THERE ARE FAST CHANGING ENVELOPE SEGMENTS 
		
	if gkwave=4 then
	  ;OUTPUT	OPCODE	AMPLITUDE    |         FREQUENCY         | NO.OF_HARMONICS | LOWEST_HARMONIC | POWER | FUNCTION_TABLE
	  asig		gbuzz 	gkamp*gktrm,    kcps*gkvib*kPEnv*gkbendmlt,      gkharm,            gklh,        gkmul,      gicos
	else
	  ktrig	changed	gkwave
	  if ktrig=1 then
	    reinit	UPDATE
	  endif
	  UPDATE:
	  asig	vco2	gkamp*gktrm, kcps*gkvib*kPEnv*gkbendmlt, (i(gkwave)-1)*2, kpw
	  rireturn
	endif
	
	;CHORUS
	aMod		oscili	gkChoDep,gkChoRte,giChoShape
	aMod		=	((aMod*0.5)+(0.5*gkChoDep))
	aMod		=	(aMod*0.01) + 0.0001
	aCho		vdelay	asig,aMod*1000,0.0101*1000
	asig		=	asig+(aCho*gkChoAmt)	
	
	kBaseFrq	portk	gkBaseFrq, kporttime				;APPLY PORTAMNETO SMOOTHING TO gkBaseFrq (CUTOFF FREQUENCY) VARIABLE
	kCFoct		=	(kFEnv*gkFEnvAmt) + kBaseFrq			;FINAL FILTER CUTOFF VALUE (IN CPS) INCORPORATING THE FILTER ENVELOPE AND THE FILTER BASE LEVEL
	kCFoct		limit	kCFoct, 4, 14					;LIMIT FILTER CUTOFF RANGE - THIS PREVENTS THE POSSIBILITY OF AN EXPLODING FILTER GIVEN CUTOFF FREQUENCIES BEYOND THE NYQUIST FREQUENCY
	kCFcps		=	cpsoct(kCFoct)					;CONVERT CUTOFF VALUE FROM AN OCT FORMAT VALUE INTO A CPS VALUE
	aFilt		moogladder 	asig, kCFcps, gkRes			;APPLY FILTER (MOOGLADDER - CPU INTENSIVE - MORE LIMITED POLYPHONY)
	
	aFilt	=	aFilt*aAEnv						;APPLY AMPLITUDE ENVELOPE
	
	;SEND FILTERED SIGNAL TO OUTPUT
			outs	aFilt, aFilt
	gaSend		=	gaSend+(aFilt*gkDlyAmt)				;CREATE AN EFFECTS SEND AUDIO SIGNAL WHICH IS SENT TO THE DELAY EFFECT INSTRUMENT
	gaRvbSndL	=	gaRvbSndL+(aFilt*gkRvbAmt)
	gaRvbSndR	=	gaRvbSndR+(aFilt*gkRvbAmt)	
	gkNoteOn	=	0						;CLEAR NOTE-ON FLAG
endin

instr	3	;THE POLYPHONIC INSTRUMENT
	;CREATE PORTAMENTO ON PITCH PARAMETER
	kporttime	linseg	0,0.01,1					;PORTAMENTO TIME RISES QUICKLY TO A HELD VALUE OF '1'
	kporttime	=	kporttime*gkgliss				;PORTAMENTO TIME FUNCTION SCALED BY VALUE OUTPUT BY FLTK SLIDER
	kpw		portk	gkpw, kporttime*0.05				;PORTAMENTO IS APPLIED TO gkpw VARIABLE

	kmodwhl		ctrl7	1,1,0,1						;READ IN CONTROLLER 1 (MODULATION WHEEL)
	kmod		oscil	1,gkmodfreq,gisine				;CREATE GENERAL MODULATION FUNCTION
	gkvib		=	(kmod * kmodwhl * gkvibdep) + 1			;CREATE VIBRATO (PITCH MODULATION) FROM GENERAL MODULATION FUNCTION
	gktrm		=	1-(kmod*0.5*gktrmdep*kmodwhl)-(gktrmdep*0.5*kmodwhl)	;CREATE TREMOLO (AMPLITUDE MODULATION) FROM GENERAL MODULATION FUNCTION
	
	kAmpEnv	linsegr	0,i(gkAAttTim),i(gkAAttLev),i(gkADecTim),i(gkASusLev),i(gkARelTim),0				;CREATE AMPLITUDE ENVELOPE
	kPEnv	linsegr	i(gkPStrLev),i(gkPAttTim),i(gkPAttLev),i(gkPDecTim),1,i(gkPRelTim),i(gkPRelLev) 		;CREATE PITCH ENVELOPE
	;kFEnv	transegr	0,i(gkFAttTim),0,i(gkFAttLev),i(gkFDecTim),0,i(gkFSusLev),i(gkFRelTim),0,i(gkFRelLev)	;CREATE FILTER ENVELOPE (transegr DOESN'T SEEM TO WORK PROPERLY HERE SO i HAVE REPLACED WITH linsegr IN THE NEXT LINE)
	kFEnv	linsegr	0,i(gkFAttTim),i(gkFAttLev),i(gkFDecTim),i(gkFSusLev),i(gkFRelTim),i(gkFRelLev)			;CREATE FILTER ENVELOPE

	aAEnv		interp	kAmpEnv						;SMOOTHER A-RATE AMPLITUDE ENVELOPE - MAY PROVE BENEFICIAL IF THERE ARE FAST CHANGING ENVELOPE SEGMENTS 
	
	if gkwave=4 then	;IF WAVE TYPE 4 HAS BEEN CHOSEN...
	  ;OUTPUT	OPCODE	AMPLITUDE    |         FREQUENCY         | NO.OF_HARMONICS | LOWEST_HARMONIC | POWER | FUNCTION_TABLE
	  asig		gbuzz 	gkamp*gktrm,    p4*gkvib*kPEnv*gkbendmlt,      gkharm,            gklh,        gkmul,      gicos	;BUZZ WAVEFORM
	else			;OTHERWISE...
	  ktrig	changed	gkwave	;IF WAVEFORM SELECTOR HAS BEEN CHANGED GENERATE A TRIGGER...
	  if ktrig=1 then	;IF A TRIGGER HAS BEEN GENERATED (I.E. WAVEFORM SELECTOR HAS BEEN CHANGED)...
	    reinit	UPDATE	;BEGIN A REINITIALISATION PASS FROM LABEL 'UPDATE'
	  endif			;END OF THIS CONDITIONAL BRANCH
	  UPDATE:		;LABEL CALLED 'UPDATE'. REINIALISATION PASS BEGINS FROM HERE.
	  asig	vco2	gkamp*gktrm, p4*gkvib*kPEnv*gkbendmlt, (i(gkwave)-1)*2, kpw	;VCO2 WAVEFORM
	  rireturn		;RETURN FROM REINITIALISATION PASS
	endif			;END OF CONDITIONAL BRANCH
	
	;CHORUS
	aMod		oscili	gkChoDep,gkChoRte,giChoShape
	aMod		=	((aMod*0.5)+(0.5*gkChoDep))
	aMod		=	(aMod*0.01) + 0.0001
	aCho		vdelay	asig,aMod*1000,0.0101*1000
	asig		=	asig+(aCho*gkChoAmt)	

	kBaseFrq	portk	gkBaseFrq, kporttime				;APPLY PORTAMNETO SMOOTHING TO gkBaseFrq (CUTOFF FREQUENCY) VARIABLE
	kCFoct		=	(kFEnv*gkFEnvAmt) + kBaseFrq			;FINAL FILTER CUTOFF VALUE (IN CPS) INCORPORATING THE FILTER ENVELOPE AND THE FILTER BASE LEVEL
	kCFoct		limit	kCFoct, 4, 14					;LIMIT FILTER CUTOFF RANGE - THIS PREVENTS THE POSSIBILITY OF AN EXPLODING FILTER GIVEN CUTOFF FREQUENCIES BEYOND THE NYQUIST FREQUENCY
	kCFcps		=	cpsoct(kCFoct)					;CONVERT CUTOFF VALUE FROM AN OCT FORMAT VALUE INTO A CPS VALUE
	aFilt		moogladder 	asig, kCFcps, gkRes			;APPLY FILTER (MOOGLADDER - CPU INTENSIVE - MORE LIMITED POLYPHONY)
	;aFilt		moogvcf2 	asig, kCFcps, gkRes			;APPLY FILTER (MOOCVCF2 - LESS CPU INTENSIVE - BETTER FOR POLYPHONY)
		
	aFilt	=	aFilt*aAEnv						;APPLY AMPLITUDE ENVELOPE
	
	;SEND FILTERED SIGNAL TO OUTPUT
			outs	aFilt, aFilt
	gaSend		=	gaSend+(aFilt*gkDlyAmt)				;CREATE AN EFFECTS SEND AUDIO SIGNAL WHICH IS SENT TO THE DELAY EFFECT INSTRUMENT
	gaRvbSndL	=	gaRvbSndL+(aFilt*gkRvbAmt)
	gaRvbSndR	=	gaRvbSndR+(aFilt*gkRvbAmt)	
endin

instr	4	; PING PONG DELAY
	imaxdelay	=	2		;MAXIMUM DELAY TIME
	kDlyTim		port	gkDlyTim,0.1	;SMOOTH DELAY TIME CHANGES
	aDlyTim		interp	kDlyTim		;INTERPOLATE TO CREATE A-RATE VERSION
	
	;LEFT CHANNEL OFFSETTING DELAY (NO FEEDBACK!)
	aBuffer		delayr	imaxdelay*.5	;ESTABLISH DELAY BUFFER
	aLeftOffset	deltap3	aDlyTim*.5	;TAP BUFFER
			delayw	gaSend		;WRITE AUDIO INTO BUFFER
			
	;LEFT CHANNEL DELAY WITH FEEDBACK
	aBuffer		delayr	imaxdelay			;ESTABLISH DELAY BUFFER
	aDlySigL	deltap3	aDlyTim                         ;TAP BUFFER
			delayw	aLeftOffset+(aDlySigL*gkDlyFB)  ;WRITE AUDIO INTO BUFFER (ADD IN FEEDBACK SIGNAL)
	
	;RIGHT CHANNEL DELAY WITH FEEDBACK
	aBuffer		delayr	imaxdelay			;ESTABLISH DELAY BUFFER
	aDlySigR	deltap3	aDlyTim                         ;TAP BUFFER
			delayw	gaSend+(aDlySigR*gkDlyFB)       ;WRITE AUDIO INTO BUFFER (ADD IN FEEDBACK SIGNAL)
	
			outs	aDlySigL+aLeftOffset, aDlySigR	;SEND DELAY OUTPUT SIGNALS TO THE SPEAKERS
	gaSend		=	0				;RESET EFFECTS SEND GLOBAL AUDIO VARIABLE TO PREVENT STUCK VALUES WHEN INSTR 1 STOPS PLAYING
	gaRvbSndL	=	gaRvbSndL+((aDlySigL+aLeftOffset)*gkRvbAmt)
	gaRvbSndR	=	gaRvbSndR+(aDlySigR*gkRvbAmt)	
endin

instr	5
	aL,aR	reverbsc	gaRvbSndL,gaRvbSndR,gkRvbTim,10000
		outs		aL,aR
		clear		gaRvbSndL,gaRvbSndR
endin

</CsInstruments>

<CsScore>
i 4 0 3600	;INSTRUMENT 4 PLAYS FOR 1 HOUR (PING-PONG DELAY)
i 5 0 3600	;INSTRUMENT 5 PLAYS FOR 1 HOUR (REVERB)
i99 0 3600	;INSTRUMENT 99 PLAYS FOR 1 HOUR (UPDATES FLTK-MIDI SYNC)
</CsScore>

</CsoundSynthesizer>