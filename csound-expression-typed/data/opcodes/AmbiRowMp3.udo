; plays mp3-files in row
;
; > al, ar AmbiRowMp3 Sfiles, kUserIndex, iFadeTime
opcode AmbiRowMp3, aa, S[]ki

Sfiles[], kUserIndex, iFadeTime xin
kNext init 0
iSize lenarray Sfiles

kEnvPoints init 0

kenv linseg 0, iFadeTime, 1, 1, 1
kUserIndex = floor(kUserIndex)
aLeft init 0
aRight init 0

kTrig changed kUserIndex
kTrigChange delayk kTrig, iFadeTime
kLoad init 1
kTrigChange = kTrigChange + kLoad

kSkipFirst linseg 1, 0.1, 1, 0.1, 0, iFadeTime + 0.1, 0, 0.1, 1
kTrigChange = kTrigChange * kSkipFirst

if (kTrig == 1) && (kNext != kUserIndex) then
  ;printks "Fade out: %d\n", 0.001, kUserIndex
  kNext = kUserIndex
  kEnvPoints = 0
  ; timout 0, iFadeTime, fade_out
  ;reinit fade_out
endif

if kTrigChange == 1 then
  ;printks "Change: %d\n", 0.001, kUserIndex
  kEnvPoints = 1
  reinit load_file
  kLoad = 0
endif

kEnv2 portk kEnvPoints, iFadeTime * 0.5

xout kEnv2 * aLeft, kEnv2 * aRight

fade_out:
  ;prints "Do fade\n", 0.001
  kenv linseg 1, iFadeTime, 0, 1, 0
  ; rireturn

load_file:
  kenv linseg 0, iFadeTime, 1, 1, 1
  ;prints "Do load: %d\n", 0.001, kNext
  iIndex = i(kNext)
  if (iIndex >= 0) && (iIndex < iSize) then
    Sfile = Sfiles[iIndex]
    iStop = 0
  else
    iStop = 1
  endif

  if iStop == 0 then
    aLeft, aRight mp3in Sfile
  else
    aLeft = 0
    aRight = 0
  endif
  rireturn

endop
