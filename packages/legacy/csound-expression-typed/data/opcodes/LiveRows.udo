; Anton Kholomiov, 2019

; UDO to play row of toggle clip like in Ableton Live
;
; Stere version of liveRow. Toggle of stereo-audio clips syncronised with BPM.
;
; > aOut liveRows iTabSize, iLeftTabs, iRightTabs, iBpm, iBeatDur, kUserIndex, iAuxParams
;
; * iTabSize - how many audio clips in the row
; * iLeftTabs, iRightTabs - tables that contains indices to left and right channels
;   of tables with monophonic audio clips (loaded with GEN01). Supposed that we load specific channels with GEN01.
; * iBpm - BPM of the track
; * iBeatDur - duration of the bar relative to quaters. So 4 - means 4/4, 3 means 3/4, 3.5  means 7/8 etc.
; * kUserIndex - index of the played clip, if it's out of bounds silence is played back.
;
; The clip switch happens only in even iBeatDur times. So that it's always in sync.
; It's supposed that audio clips of right and left channels are taken from single
; stereo source so they have the same length and they are aligned with specified tempo.
; So no automatic rescaling is done.
;
; Basic functionality is to switch the clips. But there are also aux-params that
; can control the length and spacing of playback.
;
; The aux-params are packed into single table. It's last parameter and we can omit it.
; The params follow by packets of 6 digits per audio-clip in the row.
;
; We can create the aux-param table with GEN02 like this
;
; > ; Aux params  Size  Del  Tail  AutoSwitch   NeedRetrig  Volume
; > f201 0 10 -2  1     0    1     -1           1           1
;                 1     0    0     -1           0           1
;
; in the example above we can see two 5-packets sets for two audio clips.
;
; There are params:
;  * Size (Int) - the size of the audio in bars
;  * Delay (Int) - how many bars to skip on clip start
;  * Tail delay (Int) - how many empty bara to play at the end of the clip
;  * AutoSwitch (Int) - what audio-clip to switch when the current is over
;     if it equals to -1 than we continue to play the current clip.
;  * NeedRetrig - if we switch the clips we can continue to play from where we left out (default behaviour)
;                  or we can start from beginning on switch (if NeedRetrig equals 1).
;  * Volume scale per audio-clip
;
opcode liveRows, aa, iiiiikjj
iTabSize, iLeftTabs, iRightTabs, iBpm, iBeatDur, kUserIndex, iAuxParams, isDebugPrint xin
kUserIndex = floor(kUserIndex)

iDur = iBeatDur / (iBpm / 60)

ii = 0
; iTabSize = ftlen(iTabs)
itTotalSteps[] init iTabSize
itTotalDurs[] init iTabSize
itTrans[] init iTabSize
itDels[] init iTabSize
itTailDels[] init iTabSize
itRetrig[] init iTabSize
itClipVolume[] init iTabSize
while ii < iTabSize do
  iTab tab_i ii, iLeftTabs
  itCurrentFileSteps = floor((ftlen(iTab) / ftsr(iTab)) / iDur)

  if (iAuxParams < 0) then
    itTotalSteps[ii] = itCurrentFileSteps
  else
    itUserTotalSteps tab_i (ii * 6), iAuxParams
    if (itUserTotalSteps > 0) then
      itTotalSteps[ii] = itUserTotalSteps
    else
      itTotalSteps[ii] = itCurrentFileSteps
    endif
  endif

  itTotalDurs[ii] = itTotalSteps[ii] * iDur * itCurrentFileSteps / itTotalSteps[ii]

  if (iAuxParams < 0) then
    itDels[ii] = 0
  else
    itDels[ii] tab_i (ii * 6 + 1), iAuxParams
    if (itDels[ii] <= 0) then
      itDels[ii] = 0
    endif
  endif

  if (iAuxParams < 0) then
    itTailDels[ii] = 0
  else
    itTailDels[ii] tab_i (ii * 6 + 2), iAuxParams
    if (itTailDels[ii] <= 0) then
      itTailDels[ii] = 0
    endif
  endif

  if (iAuxParams < 0) then
    itTrans[ii] = ii
  else
    itTrans[ii] tab_i (ii * 6 + 3), iAuxParams
    if (itTrans[ii] < 0) then
      itTrans[ii] = ii
    endif
  endif

  if (iAuxParams < 0) then
    itRetrig[ii] = ii
  else
    itRetrig[ii] tab_i (ii * 6 + 4), iAuxParams
    if (itRetrig[ii] < 0) then
      itRetrig[ii] = 0
    endif
  endif

  if (iAuxParams < 0) then
    itClipVolume[ii] = 1
  else
    itClipVolume[ii] tab_i (ii * 6 + 5), iAuxParams
    if (itClipVolume[ii] < 0) then
      itClipVolume[ii] = 1
    endif
  endif


  ii = ii + 1
od

ki init 0
iLeftInitTab tab_i 0, iLeftTabs
iRightInitTab tab_i 0, iRightTabs
iInitDur = itTotalDurs[0]
kLeftTab      init iLeftInitTab
kRightTab     init iRightInitTab
kTotalDur init iInitDur
kStart    init 0
kDel      init 0
kLim      init 0
kClipVolume init 1
kStarts[] init iTabSize
kTrans[]  init iTabSize
kStop     init 0
ktTotalSteps[] init iTabSize
ktTotalDurs[]  init iTabSize
ktDels[]       init iTabSize
ktTailDels[]   init iTabSize
ktRetrig[] init iTabSize
ktClipVolume[] init iTabSize
while ki < iTabSize do
  kStarts[ki] = 0
  kTrans[ki] = itTrans[ki]
  ktRetrig[ki] = itRetrig[ki]
  ktClipVolume[ki] = itClipVolume[ki]
  ktDels[ki] = itDels[ki]
  ktTailDels[ki] = itTailDels[ki]
  ktTotalSteps[ki] = itTotalSteps[ki] + ktDels[ki] + ktTailDels[ki]
  ktTotalDurs[ki]  = itTotalDurs[ki]
  ki = ki + 1
od

aNdx phasor (1 / iDur)
aNdx = iDur * aNdx

kIndex init 0
kTrigIndex changed kUserIndex
if (kTrigIndex == 1) then
  if (kUserIndex != kIndex) then
    kIndex = kUserIndex

    ; should we retriger audio clip from the start or continue (default: continue)
    if ktRetrig[kIndex] == 1 then
      kStarts[kIndex] = 0
    endif
  endif
endif


kTrig metro (1 / iDur)
if (kTrig == 1) then
  if kIndex >= 0 && kIndex <= (iTabSize - 1) then
    kStop = 0
    kLeftTabNext  tab kIndex, iLeftTabs
    kRightTabNext tab kIndex, iRightTabs
    kStart = kStarts[kIndex]
    kDel = ktDels[kIndex]
    kLim = ktTotalSteps[kIndex] - ktTailDels[kIndex]
    kTotalDur = ktTotalDurs[kIndex]
    kClipVolume = ktClipVolume[kIndex]

    if (isDebugPrint > 0) then
      printks "kTabLeft  %d\n", 0, kLeftTab
      printks "kTabright %d\n", 0, kRightTab
      printks "kLeftTabNext  %d\n", 0, kLeftTabNext
      printks "kRightTabNext  %d\n", 0, kRightTabNext
      printks "kStart    %d\n", 0, kStart
      printks "kLim      %d\n", 0, (ktTotalSteps[kIndex] - 1)
      printks "kTotalDur %f\n", 0, kTotalDur
      printks "kDel      %f\n", 0, kDel
    endif

    if (kStart == ktTotalSteps[kIndex] - 1) then
      kStarts[kIndex] = 0
      kIndex = kTrans[kIndex]
    else
      kStarts[kIndex] = (kStarts[kIndex] + 1) % ktTotalSteps[kIndex]
    endif
    kLeftTab  = kLeftTabNext
    kRightTab = kRightTabNext
  else
    kStop = 1
  endif
endif

kFadeEnv loopseg (1 / iDur), kTrig, 0, 1, 0.99, 1, 0.01, 0

if (kStop == 0 && kStart >= kDel && kStart < kLim) then
  aPtr = ((kStart - kDel) * iDur + aNdx) / kTotalDur
  aLeft  tableikt aPtr, kLeftTab, 1
  aRight tableikt aPtr, kRightTab, 1

  aLeft = aLeft * kClipVolume * kFadeEnv
  aRight = aRight * kClipVolume * kFadeEnv
else
  aLeft = 0
  aRight = 0
endif

xout aLeft, aRight
endop
